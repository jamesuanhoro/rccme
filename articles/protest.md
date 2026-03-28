# Protest: A moderation example

## Preamble

``` r
library(rccme)
library(modelsummary) # for model summary
```

Also install the `sandwich` package for robust standard errors, but you
don’t have to load it.

Summarise the dataset:

``` r
summary(protest)
#>      subnum         protest          sexism          angry      
#>  Min.   :  7.0   Min.   :0.000   Min.   :2.870   Min.   :1.000  
#>  1st Qu.: 68.0   1st Qu.:0.000   1st Qu.:4.500   1st Qu.:1.000  
#>  Median :115.0   Median :1.000   Median :5.120   Median :1.000  
#>  Mean   :121.8   Mean   :1.031   Mean   :5.117   Mean   :2.124  
#>  3rd Qu.:182.0   3rd Qu.:2.000   3rd Qu.:5.620   3rd Qu.:2.000  
#>  Max.   :240.0   Max.   :2.000   Max.   :7.000   Max.   :7.000  
#>      liking         respappr    
#>  Min.   :1.000   Min.   :1.500  
#>  1st Qu.:5.000   1st Qu.:4.000  
#>  Median :5.830   Median :5.250  
#>  Mean   :5.637   Mean   :4.866  
#>  3rd Qu.:6.500   3rd Qu.:5.750  
#>  Max.   :7.000   Max.   :7.000
```

The goal is to fit the moderation model in Figure 10.2 of the 2017
Introduction to Process text by Andrew Hayes.

In this model, `sexism` interacts with both levels of `protest` to
predict `liking`.

We already have scale average scores for `sexism` and `liking`.

We also gather construct reliabilities from table 1 in the paper:

``` r
cons_rel <- list(
  "sexism" = .75, "liking" = .88
)
```

## Calibrating the scores

We calibrate the `sexism` score since it’s the predictor. We pass the
dummy variables for the two levels of protest since they are predictors
that do not need calibrating.

``` r
head(cov_lik <- model.matrix(~ factor(protest), protest))
#>   (Intercept) factor(protest)1 factor(protest)2
#> 1           1                0                1
#> 2           1                0                0
#> 3           1                0                1
#> 4           1                0                1
#> 5           1                0                1
#> 6           1                1                0
cal_scores_for_lik_out <- rccme_calib_me(
  protest[, c("sexism")],
  rel_vec = cons_rel$sexism,
  z_mat = cov_lik[, -1] # remove intercept
)
head(cal_scores_for_lik_out)
#>       [,1]
#> 1 4.912301
#> 2 4.458092
#> 3 5.009350
#> 4 5.382615
#> 5 5.472199
#> 6 5.621909
protest$sexism_to_lik <- cal_scores_for_lik_out[, 1]
```

## Run the regression

``` r
(fit_cal <- lm(
  liking ~ sexism_to_lik * factor(protest),
  protest
))
#> 
#> Call:
#> lm(formula = liking ~ sexism_to_lik * factor(protest), data = protest)
#> 
#> Coefficients:
#>                    (Intercept)                   sexism_to_lik  
#>                         8.5197                         -0.6329  
#>               factor(protest)1                factor(protest)2  
#>                        -5.7057                         -4.8265  
#> sexism_to_lik:factor(protest)1  sexism_to_lik:factor(protest)2  
#>                         1.2072                          1.0419
```

``` r
(fit_no_cal <- lm(
  liking ~ sexism * factor(protest),
  protest
))
#> 
#> Call:
#> lm(formula = liking ~ sexism * factor(protest), data = protest)
#> 
#> Coefficients:
#>             (Intercept)                   sexism         factor(protest)1  
#>                  7.7062                  -0.4725                  -4.1288  
#>        factor(protest)2  sexism:factor(protest)1  sexism:factor(protest)2  
#>                 -3.4908                   0.9012                   0.7778
```

All coefficients are different.

``` r
modelsummary(
  list(
    "Without Calibration" = fit_no_cal,
    "With Calibration" = fit_cal
  ),
  estimate = "{estimate} ({std.error})", statistic = "{p.value}",
  # Set standard error to HCSEs and omit distracting fit indices
  gof_omit = "IC|F|Log", vcov = "HC4",
  # Rename sexism_to_lik:
  coef_rename = c("sexism_to_lik" = "sexism")
)
```

|                         | Without Calibration | With Calibration |
|-------------------------|---------------------|------------------|
| (Intercept)             | 7.706 (1.636)       | 8.520 (2.228)    |
|                         | \<0.001             | \<0.001          |
| sexism                  | -0.472 (0.345)      | -0.633 (0.462)   |
|                         | 0.173               | 0.173            |
| factor(protest)1        | -4.129 (1.818)      | -5.706 (2.460)   |
|                         | 0.025               | 0.022            |
| factor(protest)2        | -3.491 (1.797)      | -4.826 (2.439)   |
|                         | 0.054               | 0.050            |
| sexism:factor(protest)1 | 0.901 (0.373)       | 1.207 (0.500)    |
|                         | 0.017               | 0.017            |
| sexism:factor(protest)2 | 0.778 (0.375)       | 1.042 (0.502)    |
|                         | 0.040               | 0.040            |
| Num.Obs.                | 129                 | 129              |
| R2                      | 0.135               | 0.135            |
| R2 Adj.                 | 0.100               | 0.100            |
| RMSE                    | 0.97                | 0.97             |
| Std.Errors              | HC4                 | HC4              |

## Original paper for dataset

Garcia, D. M., Schmitt, M. T., Branscombe, N. R., & Ellemers, N. (2010).
Women’s reactions to ingroup members who protest discriminatory
treatment: The importance of beliefs about inequality and response
appropriateness. *European Journal of Social Psychology, 40*(5),
733–745. https://doi.org/10.1002/ejsp.644

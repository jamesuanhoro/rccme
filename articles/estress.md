# The Economic Stress model mediation example

## Preamble

``` r
library(rccme)
library(modelsummary) # for model summary
```

Also install the `sandwich` package for robust standard errors, but you
don’t have to load it.

Summarise the dataset:

``` r
summary(estress)
#>      tenure          estress         affect         withdraw    
#>  Min.   : 0.000   Min.   :1.00   Min.   :1.000   Min.   :1.000  
#>  1st Qu.: 0.940   1st Qu.:3.50   1st Qu.:1.160   1st Qu.:1.000  
#>  Median : 4.000   Median :4.50   Median :1.330   Median :2.000  
#>  Mean   : 5.929   Mean   :4.62   Mean   :1.598   Mean   :2.321  
#>  3rd Qu.: 8.980   3rd Qu.:5.50   3rd Qu.:1.830   3rd Qu.:3.000  
#>  Max.   :33.000   Max.   :7.00   Max.   :5.000   Max.   :7.000  
#>       sex              age             ese       
#>  Min.   :0.0000   Min.   :23.00   Min.   :2.530  
#>  1st Qu.:0.0000   1st Qu.:36.00   1st Qu.:5.000  
#>  Median :1.0000   Median :44.00   Median :5.730  
#>  Mean   :0.6183   Mean   :43.79   Mean   :5.607  
#>  3rd Qu.:1.0000   3rd Qu.:51.00   3rd Qu.:6.330  
#>  Max.   :1.0000   Max.   :71.00   Max.   :7.000
```

The goal is to fit the mediation model in Figure 4.2 of the 2017
Introduction to Process text by Andrew Hayes.

In this model, `estress`, `ese`, `sex` and `tenure` predict `affect`;
and all five variables predict `withdraw`.

We already have scale average scores for `estress`, `ese`, `affect`,
`withdraw`

We also gather construct reliabilities from table 1 in the paper:

``` r
cons_rel <- list(
  "estress" = .72, "affect" = .88, "withdraw" = .73, "ese" = .95
)
```

When using regression calibration, we need to think about each
regression model, one at-a-time.

## The model for affect

In this model, `estress` and `ese` are scale predictors, i.e., measured
with error. `sex` and `tenure` are assumed to be measured without error.

Prior to regression, we calibrate the `estress` and `ese` scores:

``` r
cal_scores_for_affect <- rccme_calib_me(
  estress[, c("estress", "ese")],
  rel_vec = c(cons_rel$estress, cons_rel$ese),
  z_mat = estress[, c("sex", "tenure")]
)
head(cal_scores_for_affect)
#>       estress      ese
#> [1,] 5.643158 5.340113
#> [2,] 4.764247 6.023981
#> [3,] 5.290965 5.276879
#> [4,] 3.594975 4.427072
#> [5,] 4.629474 4.901408
#> [6,] 5.690184 5.071872
estress$estress_to_aff <- cal_scores_for_affect[, "estress"]
estress$ese_to_aff <- cal_scores_for_affect[, "ese"]
```

Now we can run the regression:

``` r
(fit_aff_cal <- lm(
  affect ~ estress_to_aff + ese_to_aff + sex + tenure, estress
))
#> 
#> Call:
#> lm(formula = affect ~ estress_to_aff + ese_to_aff + sex + tenure, 
#>     data = estress)
#> 
#> Coefficients:
#>    (Intercept)  estress_to_aff      ese_to_aff             sex          tenure  
#>        1.45953         0.22453        -0.14667        -0.01102        -0.01174
```

We also run the regression with the original variables:

``` r
(fit_aff_no_cal <- lm(
  affect ~ estress + ese + sex + tenure, estress
))
#> 
#> Call:
#> lm(formula = affect ~ estress + ese + sex + tenure, data = estress)
#> 
#> Coefficients:
#> (Intercept)      estress          ese          sex       tenure  
#>     1.78549      0.15934     -0.15488      0.01479     -0.01084
```

We see the estress coefficient is larger with the calibrated variable.

## The model for withdraw

In this model, `estress`, `ese` and `affect` are scale predictors, i.e.,
measured with error. `sex` and `tenure` are assumed to be measured
without error.

Prior to regression, we calibrate the `estress`, `ese` and `affect`
scores:

``` r
cal_scores_for_with <- rccme_calib_me(
  estress[, c("estress", "ese", "affect")],
  rel_vec = c(cons_rel$estress, cons_rel$ese, cons_rel$affect),
  z_mat = estress[, c("sex", "tenure")]
)
head(cal_scores_for_with)
#>       estress      ese   affect
#> [1,] 5.784934 5.329278 2.501453
#> [2,] 4.632448 6.034053 1.091612
#> [3,] 5.403243 5.268299 2.321956
#> [4,] 3.507735 4.433739 1.220640
#> [5,] 4.482927 4.912607 1.101863
#> [6,] 5.612494 5.077809 1.554001
estress$estress_to_wth <- cal_scores_for_with[, "estress"]
estress$ese_to_wth <- cal_scores_for_with[, "ese"]
estress$affect_to_wth <- cal_scores_for_with[, "affect"]
```

Now we can run the regression:

``` r
(fit_wth_cal <- lm(
  withdraw ~ estress_to_wth + ese_to_wth + affect_to_wth + sex + tenure,
  estress
))
#> 
#> Call:
#> lm(formula = withdraw ~ estress_to_wth + ese_to_wth + affect_to_wth + 
#>     sex + tenure, data = estress)
#> 
#> Coefficients:
#>    (Intercept)  estress_to_wth      ese_to_wth   affect_to_wth             sex  
#>      2.8152882      -0.1691593      -0.2108133       0.8623629       0.1461277  
#>         tenure  
#>      0.0001724
```

We also run the regression with the original variables:

``` r
(fit_wth_no_cal <- lm(
  withdraw ~ estress + ese + affect + sex + tenure, estress
))
#> 
#> Call:
#> lm(formula = withdraw ~ estress + ese + affect + sex + tenure, 
#>     data = estress)
#> 
#> Coefficients:
#> (Intercept)      estress          ese       affect          sex       tenure  
#>    2.746089    -0.093542    -0.212106     0.707137     0.127391    -0.002065
```

We see the estress and affect coefficients are larger with the
calibrated variables.

## Summarise all models

``` r
modelsummary(
  list(
    "Affect (No Calibration)" = fit_aff_no_cal,
    "Affect (With Calibration)" = fit_aff_cal,
    "Withdraw (No Calibration)" = fit_wth_no_cal,
    "Withdraw (With Calibration)" = fit_wth_cal
  ),
  estimate = "{estimate} ({std.error})", statistic = "{p.value}",
  # Set standard error to HCSEs and omit distracting fit indices
  gof_omit = "IC|F|Log", vcov = "HC4",
  # Align variable names across parameters:
  coef_map = c(
    "(Intercept)" = "(Intercept)",
    "estress" = "estress", "ese" = "ese", "affect" = "affect",
    "estress_to_aff" = "estress", "estress_to_wth" = "estress",
    "ese_to_aff" = "ese", "ese_to_wth" = "ese", "affect_to_wth" = "affect",
    "sex" = "sex", "tenure" = "tenure"
  )
)
```

|             | Affect (No Calibration) | Affect (With Calibration) | Withdraw (No Calibration) | Withdraw (With Calibration) |
|-------------|-------------------------|---------------------------|---------------------------|-----------------------------|
| (Intercept) | 1.785 (0.326)           | 1.460 (0.378)             | 2.746 (0.634)             | 2.815 (0.709)               |
|             | \<0.001                 | \<0.001                   | \<0.001                   | \<0.001                     |
| estress     | 0.159 (0.041)           | 0.225 (0.059)             | -0.094 (0.065)            | -0.169 (0.102)              |
|             | \<0.001                 | \<0.001                   | 0.152                     | 0.097                       |
| ese         | -0.155 (0.056)          | -0.147 (0.058)            | -0.212 (0.088)            | -0.211 (0.094)              |
|             | 0.006                   | 0.012                     | 0.016                     | 0.026                       |
| affect      |                         |                           | 0.707 (0.187)             | 0.862 (0.234)               |
|             |                         |                           | \<0.001                   | \<0.001                     |
| sex         | 0.015 (0.088)           | -0.011 (0.091)            | 0.127 (0.155)             | 0.146 (0.158)               |
|             | 0.867                   | 0.904                     | 0.413                     | 0.356                       |
| tenure      | -0.011 (0.006)          | -0.012 (0.006)            | -0.002 (0.010)            | 0.000 (0.010)               |
|             | 0.073                   | 0.052                     | 0.836                     | 0.986                       |
| Num.Obs.    | 262                     | 262                       | 262                       | 262                         |
| R2          | 0.163                   | 0.163                     | 0.206                     | 0.206                       |
| R2 Adj.     | 0.150                   | 0.150                     | 0.190                     | 0.190                       |
| RMSE        | 0.66                    | 0.66                      | 1.11                      | 1.11                        |
| Std.Errors  | HC4                     | HC4                       | HC4                       | HC4                         |

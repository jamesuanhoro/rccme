
# rccme

<!-- badges: start -->

[![Project Status: Active The project has reached a stable, usable state
and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
![GitHub](https://img.shields.io/github/license/jamesuanhoro/rccme)
<!-- [![Codecov test coverage](https://codecov.io/gh/jamesuanhoro/rccme/branch/main/graph/badge.svg)](https://app.codecov.io/gh/jamesuanhoro/rccme?branch=main) -->
![GitHub R package
version](https://img.shields.io/github/r-package/v/jamesuanhoro/rccme)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/rccme)](https://cran.r-project.org/package=rccme)
![GitHub last
commit](https://img.shields.io/github/last-commit/jamesuanhoro/rccme)
[![R-CMD-check](https://github.com/jamesuanhoro/rccme/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jamesuanhoro/rccme/actions/workflows/R-CMD-check.yaml)
[![rccme status
badge](https://jamesuanhoro.r-universe.dev/badges/rccme)](https://jamesuanhoro.r-universe.dev)
<!-- badges: end -->

The goal of rccme is to produce regression calibrated scores for
downstream regression models. Yet to implement tests!!

## Installation

You can install the latest version of rccme with:

``` r
install.packages(
  'rccme',
  repos = c(
    'https://jamesuanhoro.r-universe.dev',
    'https://cloud.r-project.org'
  )
)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(rccme)

library(lavaan) # for data source and measurement models
```

    ## This is lavaan 0.6-21
    ## lavaan is FREE software! Please report any bugs.

``` r
dat <- HolzingerSwineford1939[, paste0("x", 1:9)]

# SEM where x9 is predicted by
# latent variables, F1, F2 and observed variables,
# x7 and x8 and
# where F1 is reflected in x1 - x3 and
# where F2 is reflected in x4 - x6
sem_fit <- sem(
  paste(
    "F1 =~ x1 + x2 + x3",
    "F2 =~ x4 + x5 + x6",
    "x9 ~ F1 + F2 + x7 + x8",
    "F1 + F2 ~~ x7 + x8",
    sep = "\n"
  ),
  dat,
  std.lv = TRUE
)

# two-step
cfa_f1 <- cfa("F1 =~ x1 + x2 + x3", dat, std.lv = TRUE)
cfa_f2 <- cfa("F2 =~ x4 + x5 + x6", dat, std.lv = TRUE)
score_f1 <- lavaan::lavPredict(cfa_f1, se = "standard")
score_f2 <- lavaan::lavPredict(cfa_f2, se = "standard")
se_f1 <- unname(attr(score_f1, "se")[[1]][, 1, drop = TRUE])
se_f2 <- unname(attr(score_f2, "se")[[1]][, 1, drop = TRUE])

x_hat_c <- rccme_calib_me(
  cbind(score_f1, score_f2),
  w_se_vec = c(se_f1, se_f2),
  z_mat = dat[, c("x7", "x8")]
)

dat$f1 <- x_hat_c[, 1]
dat$f2 <- x_hat_c[, 2]
dat$std_1 <- scale(rowMeans(dat[, c("x1", "x2", "x3")]))
dat$std_2 <- scale(rowMeans(dat[, c("x4", "x5", "x6")]))

# without measurement error correction
summary(lm(x9 ~ std_1 + std_2 + x7 + x8, dat))
```

    ## 
    ## Call:
    ## lm(formula = x9 ~ std_1 + std_2 + x7 + x8, data = dat)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.30536 -0.58327  0.04516  0.47479  2.84397 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.12304    0.27993  11.156  < 2e-16 ***
    ## std_1        0.32064    0.05168   6.204 1.85e-09 ***
    ## std_2        0.07384    0.05092   1.450 0.148042    
    ## x7           0.17268    0.05085   3.396 0.000778 ***
    ## x8           0.27651    0.05568   4.966 1.16e-06 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8285 on 296 degrees of freedom
    ## Multiple R-squared:  0.335,  Adjusted R-squared:  0.326 
    ## F-statistic: 37.27 on 4 and 296 DF,  p-value: < 2.2e-16

``` r
# with calibrated scores
summary(lm(x9 ~ f1 + f2 + x7 + x8, dat))
```

    ## 
    ## Call:
    ## lm(formula = x9 ~ f1 + f2 + x7 + x8, data = dat)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -2.33398 -0.57696  0.02545  0.46304  2.75533 
    ## 
    ## Coefficients:
    ##             Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)  3.34598    0.28692  11.662  < 2e-16 ***
    ## f1           0.42704    0.06962   6.134 2.74e-09 ***
    ## f2           0.02507    0.05833   0.430 0.667680    
    ## x7           0.18402    0.05120   3.594 0.000381 ***
    ## x8           0.22758    0.05782   3.936 0.000103 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.8274 on 296 degrees of freedom
    ## Multiple R-squared:  0.3368, Adjusted R-squared:  0.3278 
    ## F-statistic: 37.58 on 4 and 296 DF,  p-value: < 2.2e-16

``` r
# SEM results
sem_fit |>
  parameterestimates() |>
  subset(op == "~")
```

    ##    lhs op rhs    est    se      z pvalue ci.lower ci.upper
    ## 7   x9  ~  F1  0.464 0.078  5.945  0.000    0.311    0.616
    ## 8   x9  ~  F2 -0.018 0.065 -0.285  0.776   -0.145    0.108
    ## 9   x9  ~  x7  0.191 0.046  4.138  0.000    0.101    0.282
    ## 10  x9  ~  x8  0.219 0.054  4.061  0.000    0.113    0.324

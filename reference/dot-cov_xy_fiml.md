# Cross-covariance when y has missing values

Cross-covariance when y has missing values

## Usage

``` r
.cov_xy_fiml(x_mat, y_mat, mu_x, mu_y, cov_y)
```

## Arguments

- x_mat:

  (matrix) complete matrix

- y_mat:

  (matrix) matrix with missing elements

- mu_x:

  (vector) known mean vector of x_mat

- mu_y:

  (vector) known mean vector of y_mat

- cov_y:

  (matrix) known covariance of y_mat

## Value

Complete cross-covariance

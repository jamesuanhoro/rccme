# Validate inputs for rccme_calib_me

Validate inputs for rccme_calib_me

## Usage

``` r
.validate_calib_me(
  w_mat,
  rel_vec = NULL,
  w_se_vec = NULL,
  w_se_mat = NULL,
  z_mat
)
```

## Arguments

- w_mat:

  (matrix) a matrix of trait estimates for n respondents on p latent
  variables

- rel_vec:

  (vector) a vector of marginal reliability for p latent variables

- w_se_vec:

  (vector) a vector of standard errors of measurement for p latent
  variables

- w_se_mat:

  (matrix) a matrix of trait estimates standard-errors for n respondents
  on p latent variables

- z_mat:

  (matrix) a matrix of error-free covariates for n respondents and q
  covariates

## Value

Returns cleaned obects if all checks pass.

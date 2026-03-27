# Calibrate scores under measurement error

Calibrate scores under measurement error

## Usage

``` r
rccme_calib_me(
  w_mat,
  rel_vec = NULL,
  w_se_vec = NULL,
  w_se_mat = NULL,
  z_mat = matrix(0, nrow = nrow(w_mat), ncol = 0),
  rescale = TRUE,
  standard = FALSE
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

- rescale:

  (logical) Should the trait estimates and their standard errors be
  re-scaled? Default is TRUE. The variables are rescaled under the
  assumption that the trait estimates were not conditioned on any
  background variables and that the marginal variance of the latent
  trait is 1. This is true for standardised latent variables in CFA or
  the default prior variance assumption of 1 in IRT score estimates.
  This rescaling ensures the trait coefficients are correct for the
  standardised traits.

- standard:

  (logical) Only relevant when passing reliability. If TRUE, attempt to
  return the standardised version of the calibrated scores If FALSE
  (default), do not attempt standardisation.

## Value

Calibrated trait estimates.

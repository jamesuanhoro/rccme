# Prepare sum-score, EAP or regression factor scores for calibration

Prepare sum-score, EAP or regression factor scores for calibration

## Usage

``` r
.prep_score(
  w_mat,
  rel_vec = NULL,
  w_se_vec = NULL,
  w_se_mat = NULL,
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

- standard:

  (logical) Only relevant when passing reliability. If TRUE, attempt to
  return the standardised version of the calibrated scores If FALSE
  (default), do not attempt standardisation.

## Value

Returns rescaled scores.

# rccme (development version)

* Added tests
* Added vignettes
* Added `sum_standardize` option to sum-score calibration (default `TRUE`).
  Sum scores are z-scored before the reliability is applied.
* Added `cov_mats` argument to `rccme_calib_me()`. It returns the transformed
  scores `w`, the error covariance `delta`, and the latent covariance `theta`.
  Calibrated scores are not yet available through this argument.
* Minor bug fixes

# rccme 0.0.1

* Returns regression calibrated for:
  * EAP scores from IRT models with heteroskedastic measurement error,
  * regression factor scores from CFA models with homoskedastic or heteroskedastic error, and
  * sum scores with reliability estimate.
* Handles missing data in error-free covariates using FIML estimation

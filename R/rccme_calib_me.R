#' Calibrate scores under measurement error
#'
#' @param w_mat (matrix) a matrix of trait estimates for n respondents
#' on p latent variables
#' @param rel_vec (vector) a vector of marginal reliability
#' for p latent variables
#' @param w_se_vec (vector) a vector of standard errors of measurement
#' for p latent variables
#' @param w_se_mat (matrix) a matrix of trait estimates standard-errors
#' for n respondents on p latent variables
#' @param z_mat (matrix) a matrix of error-free covariates for n respondents
#' and q covariates
#' @param rescale (logical) Should the trait estimates and their
#' standard errors be re-scaled? Default is TRUE. The variables are
#' rescaled under the assumption that the trait estimates were not
#' conditioned on any background variables and that the marginal
#' variance of the latent trait is 1. This is true for standardised
#' latent variables in CFA or the default prior variance assumption
#' of 1 in IRT score estimates. This rescaling ensures the trait
#' coefficients are correct for the standardised traits.
#' @param standard (logical) Only relevant when passing reliability.
#' If TRUE, attempt to return the standardised version of the calibrated scores
#' If FALSE (default), do not attempt standardisation.
#' @param cov_mats (list) Optional. A specification of the connected
#' measurement model, as described in `.validate_cov_mats()`. When
#' supplied, the function returns the transformed scores `w`, the
#' error covariance `delta`, and the latent covariance `theta` from
#' equations 2 to 4. It does not return calibrated scores yet, and
#' `rel_vec`, `w_se_vec`, `w_se_mat`, and `z_mat` cannot be used with
#' it. The `rescale` and `standard` arguments are ignored.
#' @param sum_standardize (logical) Only relevant when `cov_mats` is
#' supplied and an entry gives a reliability. If TRUE (default), the
#' sum score is z-scored before the reliability is applied, so its
#' variance is 1. If FALSE, the raw scale is kept.
#' @return Calibrated trait estimates. When `cov_mats` is supplied,
#' a list with elements `w`, `delta`, and `theta`.
#' @export
rccme_calib_me <- function(
    w_mat,
    rel_vec = NULL, w_se_vec = NULL, w_se_mat = NULL,
    z_mat = matrix(0, nrow = nrow(w_mat), ncol = 0),
    rescale = TRUE, standard = FALSE,
    cov_mats = NULL, sum_standardize = TRUE) {
  if (!is.null(cov_mats)) {
    if (!is.null(rel_vec) || !is.null(w_se_vec) || !is.null(w_se_mat)) {
      stop("`cov_mats` cannot be combined with `rel_vec`, `w_se_vec`, ",
           "or `w_se_mat`; give the reliabilities or standard errors ",
           "inside `cov_mats`.")
    }
    if (ncol(z_mat) > 0) {
      stop("`z_mat` is not yet supported with `cov_mats`.")
    }
    return(.calib_connected(w_mat, cov_mats, sum_standardize))
  }

  valid_objects <- .validate_calib_me(
    w_mat, rel_vec, w_se_vec, w_se_mat, z_mat
  )

  w_mat <- valid_objects$w_mat
  rel_vec <- valid_objects$rel_vec
  w_se_vec <- valid_objects$w_se_vec
  w_se_mat <- valid_objects$w_se_mat
  z_mat <- valid_objects$z_mat

  if (!isFALSE(rescale) || !is.null(rel_vec)) {
    rescaled_objects <- .prep_score(
      w_mat, rel_vec, w_se_vec, w_se_mat, standard
    )
    w_mat <- rescaled_objects$w_mat
    w_se_vec <- rescaled_objects$w_se_vec
    w_se_mat <- rescaled_objects$w_se_mat
  }

  if (!is.null(w_se_vec)) {
    calibrated_scores <- .get_e_z_zw_hom(w_mat, w_se_vec, z_mat)
  } else {
    calibrated_scores <- .get_e_z_zw_het(w_mat, w_se_mat, z_mat)
  }

  return(calibrated_scores)
}

# Builds w, delta, and theta for the cov_mats path. With sum_standardize
# TRUE, each reliability-based column is z-scored first, the same step
# .prep_score() takes, so the standard error from the reliability is on
# the standardized scale.
.calib_connected <- function(w_mat, cov_mats, sum_standardize = TRUE) {
  .validate_cov_mats(w_mat, cov_mats)
  if (isTRUE(sum_standardize)) {
    for (entry in cov_mats) {
      if (is.null(entry$rel)) next
      for (cons in entry$cons) {
        x <- w_mat[, cons]
        w_mat[, cons] <- (x - mean(x)) / stats::sd(x)
      }
    }
  }
  .build_connected(w_mat, cov_mats)
}

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
#' @return Calibrated trait estimates.
#' @export
rccme_calib_me <- function(
    w_mat,
    rel_vec = NULL, w_se_vec = NULL, w_se_mat = NULL,
    z_mat = matrix(0, nrow = nrow(w_mat), ncol = 0),
    rescale = TRUE, standard = FALSE) {
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

#' Validate a cov_mats specification against w_mat
#'
#' Checks that every entry in `cov_mats` is exactly one of the three
#' allowed shapes (`rel` + `cons`, `se` + `cons`, or `cov` + `acov`
#' and/or `se` + `cons`), that `cons` values are unique across
#' entries, and that they exactly cover the columns of `w_mat`. A
#' `cov` entry must be paired with `se`, `acov`, or both: `cov`
#' alone describes the true-score side but says nothing about the
#' entry's error, and leaving that unrequired would let a model-
#' based entry pass validation and then fail silently downstream. A
#' multi-column `cov` entry must specifically have `acov`, since
#' `se` alone cannot describe the error covariance between columns.
#'
#' Every number in `rel`, `se`, `cov`, and `acov` must be finite, and
#' every matrix in an `acov` list is checked, not just the first. For
#' a model-based entry, `cov` minus each respondent's error
#' covariance must be positive definite. Otherwise `psi` in equation
#' 3 is infinite or negative, and `ginv()` would hide that instead of
#' failing.
#'
#' @param w_mat (matrix) a matrix of trait estimates for n respondents
#' on p constructs
#' @param cov_mats (list) a list of lists, one per construct or group
#' of jointly-modeled constructs, each naming its columns in `cons`
#' @return Returns `cov_mats` unchanged if every check passes.
#' @keywords internal
.validate_cov_mats <- function(w_mat, cov_mats) {
  if (!is.list(cov_mats) || length(cov_mats) == 0) {
    stop("`cov_mats` must be a non-empty list.")
  }

  n <- nrow(w_mat)
  all_cons <- vector("list", length(cov_mats))

  for (idx in seq_along(cov_mats)) {
    entry <- cov_mats[[idx]]

    if (is.null(entry$cons)) {
      stop(paste0("`cov_mats[[", idx, "]]` is missing `cons`."))
    }
    if (!all(entry$cons %in% colnames(w_mat))) {
      stop(paste0(
        "`cov_mats[[", idx, "]]$cons` names columns not present",
        " in `w_mat`."
      ))
    }
    all_cons[[idx]] <- entry$cons
    p_entry <- length(entry$cons)

    has_rel <- !is.null(entry$rel)
    has_se <- !is.null(entry$se)
    has_cov <- !is.null(entry$cov)
    has_acov <- !is.null(entry$acov)

    if (has_rel && (has_se || has_cov || has_acov)) {
      stop(paste0(
        "`cov_mats[[", idx, "]]` mixes `rel` with `se`, `cov`,",
        " or `acov`; `rel` must stand alone with `cons`."
      ))
    }
    if (has_acov && !has_cov) {
      stop(paste0(
        "`cov_mats[[", idx, "]]` has `acov` without `cov`;",
        " a covariance-based entry needs both."
      ))
    }
    if (p_entry > 1 && !(has_cov && has_acov)) {
      stop(paste0(
        "`cov_mats[[", idx, "]]` spans multiple columns;",
        " multi-construct entries must provide both `cov`",
        " and `acov`."
      ))
    }
    if (has_cov && !has_se && !has_acov) {
      stop(paste0(
        "`cov_mats[[", idx, "]]` has `cov` but no `se` or",
        " `acov`; a model-based entry needs one of these to",
        " describe its error."
      ))
    }
    if (!has_rel && !has_se && !has_cov) {
      stop(paste0(
        "`cov_mats[[", idx, "]]` must provide `rel`, `se`, or",
        " `cov`."
      ))
    }

    if (has_rel && (length(entry$rel) != 1 || !is.finite(entry$rel))) {
      stop(paste0(
        "`cov_mats[[", idx, "]]$rel` must be a single finite value."
      ))
    }
    if (has_se && !(length(entry$se) %in% c(1, n))) {
      stop(paste0(
        "`cov_mats[[", idx, "]]$se` must have length 1 or n."
      ))
    }
    if (has_se && !all(is.finite(entry$se))) {
      stop(paste0(
        "`cov_mats[[", idx, "]]$se` has NA or infinite values."
      ))
    }
    if (has_cov) {
      cov_ok <- is.matrix(entry$cov) &&
        all(dim(entry$cov) == p_entry) && all(is.finite(entry$cov))
      if (!cov_ok) {
        stop(paste0(
          "`cov_mats[[", idx, "]]$cov` must be a ", p_entry,
          "x", p_entry, " matrix of finite values."
        ))
      }
    }
    if (has_acov) {
      v_list <- if (is.matrix(entry$acov)) list(entry$acov) else
        entry$acov
      acov_ok <- (is.matrix(entry$acov) ||
        (is.list(entry$acov) && length(entry$acov) == n)) &&
        all(vapply(v_list, function(m) {
          is.matrix(m) && all(dim(m) == p_entry) && all(is.finite(m))
        }, TRUE))
      if (!acov_ok) {
        stop(paste0(
          "`cov_mats[[", idx, "]]$acov` must be a ", p_entry,
          "x", p_entry, " matrix of finite values, or a length-n",
          " list of such matrices."
        ))
      }
    }
    if (has_cov) {
      if (!has_acov) {
        v_list <- lapply(entry$se^2, matrix)
      }
      n_bad <- sum(!vapply(v_list, function(v) {
        ev <- eigen(entry$cov - v, symmetric = TRUE, only.values = TRUE)
        min(ev$values) > 0
      }, TRUE))
      if (n_bad > 0) {
        stop(paste0(
          "`cov_mats[[", idx, "]]` has error covariance that is not",
          " smaller than `cov` for ", n_bad, " respondent(s);",
          " `psi` would be infinite or negative."
        ))
      }
    }
  }

  all_cons <- unlist(all_cons)
  if (anyDuplicated(all_cons)) {
    stop("`cons` values are duplicated across `cov_mats` entries.")
  }
  if (!setequal(all_cons, colnames(w_mat))) {
    stop("`cov_mats` entries do not exactly cover `w_mat` columns.")
  }

  cov_mats
}

#' Transformed scores, error covariance, and phi for one entry
#'
#' Returns one model's pieces of equation 4: the transformed scores
#' `x_star`, their error covariance `psi`, and the within-model
#' covariance `phi`.
#'
#' The error covariance `v` is `acov` if given, or the squared
#' standard error otherwise. A reliability is turned into a standard
#' error as the observed standard deviation times the square root of
#' one minus the reliability, as in `.prep_score()`.
#'
#' A sum or mean score (no `cov`) is not shrunk, so `x_star` is the
#' observed score, `psi` is `v`, and `phi` is the observed variance
#' minus the average error variance. An entry with `cov` is a
#' regression or EAP score and goes through equations 2 and 3:
#' `omega = I - v %*% ginv(phi)`, `x_star = ginv(omega) %*% x`, and
#' `psi = ginv(ginv(v) - ginv(phi))`. `phi` is inverted once per
#' entry. When `v` is the same for everyone, equation 3 runs once
#' over all respondents. It runs per person only when `v` varies by
#' person.
#'
#' @param w_mat (matrix) the full score matrix
#' @param entry (list) one validated cov_mats entry
#' @return A list with `x_star` (n x k matrix), `psi` (a k x k
#' matrix, or a length-n list of them when error varies by person),
#' and `phi` (k x k matrix).
#' @keywords internal
.entry_parts <- function(w_mat, entry) {
  x <- w_mat[, entry$cons, drop = FALSE]

  v <- entry$acov
  if (is.null(v)) {
    se <- entry$se
    if (!is.null(entry$rel)) {
      se <- stats::sd(x) * sqrt(1 - entry$rel)
    }
    v <- if (length(se) == 1) matrix(se^2) else lapply(se^2, matrix)
  }

  if (is.null(entry$cov)) {
    phi <- stats::var(x) - mean(se^2)
    return(list(x_star = x, psi = v, phi = phi))
  }

  phi_inv <- MASS::ginv(entry$cov)
  deshrink <- function(x_rows, v_rows) {
    omega_inv <- MASS::ginv(diag(ncol(x)) - v_rows %*% phi_inv)
    list(
      x_star = x_rows %*% t(omega_inv),
      psi = MASS::ginv(MASS::ginv(v_rows) - phi_inv)
    )
  }

  if (is.matrix(v)) {
    res <- deshrink(x, v)
  } else {
    per_person <- lapply(seq_len(nrow(x)), function(i) {
      deshrink(x[i, , drop = FALSE], v[[i]])
    })
    res <- list(
      x_star = do.call(rbind, lapply(per_person, `[[`, "x_star")),
      psi = lapply(per_person, `[[`, "psi")
    )
  }

  c(res, list(phi = entry$cov))
}

#' Build w, delta, and theta from cov_mats (equation 4)
#'
#' Combines every entry's pieces into the three objects the
#' calibration step needs. `w` holds the transformed scores of every
#' entry. `delta` places each entry's `psi` in that entry's own block
#' and zeros everywhere else, since errors from separately fitted
#' models are independent. `theta` places each entry's `phi` in its
#' own diagonal block, and fills every cross-entry cell from the
#' sample covariance of `w`, which is unbiased because those cells
#' involve errors from different models (Croon, 2002).
#'
#' All three keep the column order of `w_mat`, so the blocks of
#' `delta` and `theta` sit wherever their columns sit in `w_mat`.
#' `delta` is a single p x p matrix when every entry's error is the
#' same for everyone, and a length-n list of p x p matrices
#' otherwise, matching the split between `.get_e_z_zw_hom()` and
#' `.get_e_z_zw_het()`.
#'
#' Combining model-based diagonal blocks with sample-based
#' off-diagonal blocks does not guarantee a positive definite
#' `theta`, so this warns when it is not.
#'
#' @param w_mat (matrix) a matrix of trait estimates for n
#' respondents on p constructs
#' @param cov_mats (list) a cov_mats specification, see
#' `.validate_cov_mats()`
#' @param sum_standardize (logical) If TRUE (default), each column that
#' is given by reliability is centered and divided by its standard
#' deviation before anything else, so its variance is 1. This is the
#' same scaling `.prep_score()` uses. FALSE keeps the raw scale.
#' @return A list with `w` (n x p), `delta` (p x p, or a length-n
#' list of p x p), and `theta` (p x p).
#' @keywords internal
.build_connected <- function(w_mat, cov_mats, sum_standardize = TRUE) {
  .validate_cov_mats(w_mat, cov_mats)
  if (isTRUE(sum_standardize)) {
    w_mat <- .standardize_sum(w_mat, cov_mats)
  }

  cons_all <- colnames(w_mat)
  p <- length(cons_all)
  parts <- lapply(cov_mats, .entry_parts, w_mat = w_mat)

  w <- w_mat
  theta <- matrix(NA_real_, p, p, dimnames = list(cons_all, cons_all))
  for (idx in seq_along(cov_mats)) {
    cons <- cov_mats[[idx]]$cons
    w[, cons] <- parts[[idx]]$x_star
    theta[cons, cons] <- parts[[idx]]$phi
  }
  cross <- is.na(theta)
  theta[cross] <- stats::cov(w)[cross]

  eig <- eigen(theta, symmetric = TRUE, only.values = TRUE)$values
  if (min(eig) <= 0) {
    warning("`theta` is not positive definite.")
  }

  delta_for <- function(i) {
    d <- matrix(0, p, p, dimnames = list(cons_all, cons_all))
    for (idx in seq_along(cov_mats)) {
      psi <- parts[[idx]]$psi
      cons <- cov_mats[[idx]]$cons
      d[cons, cons] <- if (is.list(psi)) psi[[i]] else psi
    }
    d
  }
  varying <- any(vapply(parts, function(pt) is.list(pt$psi), TRUE))
  if (varying) {
    delta <- lapply(seq_len(nrow(w)), delta_for)
  } else {
    delta <- delta_for(1)
  }

  list(w = w, delta = delta, theta = theta)
}

# Z-scores the columns of reliability-based entries with the same two
# steps as .prep_score(): center each column, then divide by its
# standard deviation. Other columns are left alone.
.standardize_sum <- function(w_mat, cov_mats) {
  for (entry in cov_mats) {
    if (is.null(entry$rel)) next
    cols <- entry$cons
    x <- w_mat[, cols, drop = FALSE]
    x <- sweep(x, 2, colMeans(x), "-")
    sd_x <- apply(x, 2, stats::sd)
    bad <- !is.finite(sd_x) | sd_x <= 0
    if (any(bad)) {
      stop(
        "`w_mat` has constant or non-finite columns, so it cannot be ",
        "standardized: ", paste(cols[bad], collapse = ", "), "."
      )
    }
    x <- sweep(x, 2, sd_x, "/")
    w_mat[, cols] <- x
  }
  w_mat
}

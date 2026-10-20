# Checks for rccme_cov_mats.R. Each case has an answer that can be
# worked out by hand, so a pass means the arithmetic is right, not
# only that the code runs.

set.seed(20240517)

# Shared inputs

w_mat <- matrix(rnorm(30), 10, 3, dimnames = list(NULL, c("A", "B", "C")))
phi_2 <- matrix(c(1, 0.3, 0.3, 1), 2, 2)
v_2 <- matrix(c(0.2, 0.05, 0.05, 0.25), 2, 2)
phi_irt <- matrix(c(1, 0.2, 0.2, 1), 2, 2)
v_low <- matrix(c(0.093, 0.005, 0.005, 0.154), 2, 2)
acov_list <- lapply(1:10, function(i) v_low)

test_that("valid cov_mats specifications pass validation", {
  expect_no_error(.validate_cov_mats(w_mat, list(
    list(rel = 0.8, cons = "A"),
    list(cov = phi_2, acov = v_2, cons = c("B", "C"))
  )))

  # acov as one matrix per respondent, the shape mirt returns
  expect_no_error(.validate_cov_mats(w_mat, list(
    list(rel = 0.8, cons = "A"),
    list(cov = phi_irt, acov = acov_list, cons = c("B", "C"))
  )))
})

test_that("duplicate constructs are rejected", {
  expect_error(.validate_cov_mats(w_mat, list(
    list(rel = 0.8, cons = "A"),
    list(rel = 0.8, cons = "A"),
    list(cov = phi_2, acov = v_2, cons = c("B", "C"))
  )), "duplicated")
})

test_that("entries must exactly cover the columns of w", {
  expect_error(.validate_cov_mats(w_mat, list(
    list(rel = 0.8, cons = "A"),
    list(rel = 0.8, cons = "B")
  )), "exactly cover")
})

test_that("rel and cov cannot be mixed in one entry", {
  expect_error(.validate_cov_mats(w_mat, list(
    list(rel = 0.8, cov = matrix(1, 1, 1), cons = "A"),
    list(cov = phi_2, acov = v_2, cons = c("B", "C"))
  )), "mixes `rel`")
})

test_that("acov requires cov", {
  expect_error(.validate_cov_mats(w_mat, list(
    list(rel = 0.8, cons = "A"),
    list(acov = v_2, cons = c("B", "C"))
  )), "without `cov`")
})

test_that("se cannot describe the error between jointly modeled columns", {
  expect_error(.validate_cov_mats(w_mat, list(
    list(rel = 0.8, cons = "A"),
    list(cov = phi_2, se = c(0.4, 0.5), cons = c("B", "C"))
  )), "multiple columns")
})

test_that("acov must be a matrix of finite values for a joint entry", {
  expect_error(.validate_cov_mats(w_mat, list(
    list(rel = 0.8, cons = "A"),
    list(cov = matrix(1, 1, 1), acov = v_2, cons = c("B", "C"))
  )), "matrix of finite values")

  expect_error(.validate_cov_mats(w_mat, list(
    list(rel = 0.8, cons = "A"),
    list(cov = matrix(c(1, NA, NA, 1), 2, 2), acov = v_2,
         cons = c("B", "C"))
  )), "matrix of finite values")
})

test_that("a single-column entry needs se, not only cov", {
  expect_error(.validate_cov_mats(w_mat, list(
    list(rel = 0.8, cons = "A"),
    list(rel = 0.8, cons = "B"),
    list(cov = matrix(1, 1, 1), cons = "C")
  )), "no `se` or")
})

test_that("an se padded with NA is rejected", {
  # This is what a length-1 se turns into when it is indexed with a
  # row mask
  expect_error(.validate_cov_mats(w_mat, list(
    list(rel = 0.8, cons = "A"),
    list(rel = 0.8, cons = "B"),
    list(cov = matrix(1, 1, 1), se = c(0.3, rep(NA, 9)), cons = "C")
  )), "NA or infinite")
})

test_that("every matrix in an acov list is checked for size and NA", {
  acov_bad_size <- acov_list
  acov_bad_size[[7]] <- diag(3)
  expect_error(.validate_cov_mats(w_mat, list(
    list(rel = 0.8, cons = "A"),
    list(cov = phi_irt, acov = acov_bad_size, cons = c("B", "C"))
  )), "length-n")

  acov_bad_na <- acov_list
  acov_bad_na[[4]][1, 2] <- NA
  expect_error(.validate_cov_mats(w_mat, list(
    list(rel = 0.8, cons = "A"),
    list(cov = phi_irt, acov = acov_bad_na, cons = c("B", "C"))
  )), "length-n")
})

test_that("error variance at or above phi is rejected", {
  expect_error(.validate_cov_mats(w_mat, list(
    list(rel = 0.8, cons = "A"),
    list(rel = 0.8, cons = "B"),
    list(cov = matrix(1, 1, 1), se = c(rep(0.5, 9), 1.1), cons = "C")
  )), "for 1 respondent")

  acov_too_big <- acov_list
  acov_too_big[[3]] <- diag(2) * 1.5
  expect_error(.validate_cov_mats(w_mat, list(
    list(rel = 0.8, cons = "A"),
    list(cov = phi_irt, acov = acov_too_big, cons = c("B", "C"))
  )), "for 1 respondent")
})

test_that("equation 3 holds for one construct", {
  # With phi = 1 and v = 0.36, omega is 0.64, so a raw score of 0.64
  # maps back to 1.
  one <- .entry_parts(
    matrix(0.64, 1, 1, dimnames = list(NULL, "C")),
    list(cov = matrix(1), se = 0.6, cons = "C")
  )
  expect_equal(one$x_star[1, 1], 1)
  expect_equal(one$psi[1, 1], 1 / (1 / 0.36 - 1))
})

test_that("equation 3 gives back the true scores for two constructs", {
  # Two constructs with a different error covariance for each person,
  # as in a two-dimensional IRT model. One person has about four times
  # the error of the other. Raw scores are built as omega_i t_i, so
  # the transformed scores must give back t_i.
  v_high <- matrix(c(0.386, 0.029, 0.029, 0.220), 2, 2)
  v_two <- list(v_low, v_high)
  t_two <- rbind(c(0.4, -0.6), c(-0.2, 0.9))
  x_two <- t(vapply(1:2, function(i) {
    omega_i <- diag(2) - v_two[[i]] %*% solve(phi_irt)
    as.numeric(omega_i %*% t_two[i, ])
  }, numeric(2)))
  colnames(x_two) <- c("B", "C")
  two <- .entry_parts(
    x_two, list(cov = phi_irt, acov = v_two, cons = c("B", "C"))
  )
  expect_equal(unname(two$x_star), t_two)
  expect_equal(two$psi[[2]], solve(solve(v_high) - solve(phi_irt)))
})

# Simulated data for the full build. Equation 4 is checked on a large
# sample. A is a classical score: true score plus independent noise. B
# and C are a factor-score pair generated as equation 1 describes, x =
# omega t + e with Var(e) = omega v. For such scores Var(x_star) = phi
# + psi, so with a large sample cov(w) should be close to theta +
# delta. That is the identity equation 5 relies on, so it checks w,
# delta, and theta together.

n <- 60000
cons_3 <- c("A", "B", "C")
sigma_true <- matrix(
  c(
    1.00, 0.30, -0.20,
    0.30, 1.20, 0.40,
    -0.20, 0.40, 0.90
  ),
  3, 3, dimnames = list(cons_3, cons_3)
)
true_scores <- matrix(rnorm(n * 3), n, 3) %*% chol(sigma_true)
colnames(true_scores) <- cons_3

se_a <- 0.5
raw_a <- true_scores[, "A"] + rnorm(n, 0, se_a)

phi_bc <- sigma_true[c("B", "C"), c("B", "C")]
v_bc <- matrix(c(0.18, 0.03, 0.03, 0.22), 2, 2)
omega_bc <- diag(2) - v_bc %*% solve(phi_bc)
e_bc <- matrix(rnorm(n * 2), n, 2) %*% chol(omega_bc %*% v_bc)
raw_bc <- true_scores[, c("B", "C")] %*% t(omega_bc) + e_bc

w_mat_sim <- cbind(raw_a, raw_bc)
colnames(w_mat_sim) <- cons_3
cov_mats_sim <- list(
  list(se = se_a, cons = "A"),
  list(cov = phi_bc, acov = v_bc, cons = c("B", "C"))
)
out <- .build_connected(w_mat_sim, cov_mats_sim)
psi_bc <- unname(solve(solve(v_bc) - solve(phi_bc)))

test_that("w leaves A unchanged and multiplies B and C by omega inverse", {
  expect_true(isTRUE(all.equal(out$w[, "A"], w_mat_sim[, "A"])))
  expect_true(isTRUE(all.equal(
    unname(out$w[, c("B", "C")]),
    unname(raw_bc %*% t(solve(omega_bc)))
  )))
})

test_that("delta is one shared matrix when no one's error differs", {
  expect_true(is.matrix(out$delta))
  expect_true(isTRUE(all.equal(
    unname(out$delta[c("B", "C"), c("B", "C")]), psi_bc
  )))
  expect_true(isTRUE(all.equal(out$delta["A", "A"], se_a^2)))
  expect_true(all(out$delta["A", c("B", "C")] == 0))
})

test_that("theta copies phi on the (B, C) block and estimates the rest", {
  expect_true(isTRUE(all.equal(
    unname(out$theta[c("B", "C"), c("B", "C")]), unname(phi_bc)
  )))
  expect_lt(max(abs(out$theta - sigma_true)), 0.02)
})

test_that("cov(w) is close to theta plus delta on the large sample", {
  expect_lt(max(abs(cov(out$w) - (out$theta + out$delta))), 0.02)
})

test_that("the per-person path agrees with the shared path", {
  # When every person has the same acov matrix, a list of identical
  # matrices must give the same w and the same delta as one matrix.
  w_200 <- w_mat_sim[1:200, ]
  cov_mats_listed <- list(
    list(se = se_a, cons = "A"),
    list(
      cov = phi_bc, acov = lapply(1:200, function(i) v_bc),
      cons = c("B", "C")
    )
  )
  out_fixed <- .build_connected(w_200, cov_mats_sim)
  out_listed <- .build_connected(w_200, cov_mats_listed)
  expect_true(isTRUE(all.equal(out_fixed$w, out_listed$w)))
  expect_true(is.list(out_listed$delta))
  expect_equal(length(out_listed$delta), 200)
  for (i in 1:200) {
    expect_true(isTRUE(all.equal(
      out_listed$delta[[i]], out_fixed$delta
    )))
  }
})

test_that("a per-person se and a reliability-based score give the right psi", {
  # A single IRT score with a per-person se, next to a score given by
  # reliability. Each person's psi is 1 / (1 / se_i^2 - 1 / phi) and
  # x_star is x / (1 - se_i^2 / phi). A's psi is the squared se derived
  # from its reliability.
  w_irt <- matrix(rnorm(10), 5, 2, dimnames = list(NULL, c("A", "D")))
  se_d <- c(0.30, 0.45, 0.25, 0.60, 0.35)
  out_irt <- .build_connected(w_irt, list(
    list(rel = 0.8, cons = "A"),
    list(cov = matrix(1.1), se = se_d, cons = "D")
  ), sum_standardize = FALSE)
  se_a_rel <- sd(w_irt[, "A"]) * sqrt(1 - 0.8)
  expect_true(is.list(out_irt$delta))
  expect_equal(length(out_irt$delta), 5)
  for (i in 1:5) {
    expect_true(isTRUE(all.equal(
      out_irt$delta[[i]]["D", "D"], 1 / (1 / se_d[i]^2 - 1 / 1.1)
    )))
    expect_true(isTRUE(all.equal(
      out_irt$delta[[i]]["A", "A"], se_a_rel^2
    )))
    expect_equal(out_irt$delta[[i]]["A", "D"], 0)
  }
  expect_true(isTRUE(all.equal(
    out_irt$w[, "D"], w_irt[, "D"] / (1 - se_d^2 / 1.1)
  )))
})

test_that("the order of entries does not change the output", {
  # Entries listed in a different order from the columns of w_mat give
  # the same output, in w_mat's column order.
  out_rev <- .build_connected(w_mat_sim, rev(cov_mats_sim))
  expect_true(isTRUE(all.equal(out_rev, out)))
})

test_that("reliability-based columns are z-scored by default", {
  # The builder centers and divides by the standard deviation, the
  # same scaling as .prep_score(), before the reliability is applied.
  w_rel <- matrix(rnorm(40, 10, 4), 20, 2,
                  dimnames = list(NULL, c("A", "B")))
  cm_rel <- list(
    list(rel = 0.7, cons = "A"),
    list(se = 0.5, cov = matrix(1, 1, 1), cons = "B")
  )
  z_a <- (w_rel[, "A"] - mean(w_rel[, "A"])) / stats::sd(w_rel[, "A"])
  res <- .build_connected(w_rel, cm_rel)
  expect_equal(unname(res$w[, "A"]), z_a)
  expect_equal(stats::sd(res$w[, "A"]), 1, tolerance = 1e-10)
  expect_equal(res$delta[["A", "A"]], 1 - 0.7)
})

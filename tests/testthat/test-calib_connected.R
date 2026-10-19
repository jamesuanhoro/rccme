set.seed(20240601)
w_conn <- matrix(
  rnorm(100, 5, 3), 50, 2,
  dimnames = list(NULL, c("A", "B"))
)
cm_conn <- list(
  list(rel = 0.8, cons = "A"),
  list(cov = matrix(1, 1, 1), se = 0.4, cons = "B")
)

test_that("cov_mats returns w, delta, and theta with the sum score standardized", {
  res <- rccme_calib_me(w_conn, cov_mats = cm_conn)
  expect_named(res, c("w", "delta", "theta"))

  w_std <- w_conn
  w_std[, "A"] <- (w_conn[, "A"] - mean(w_conn[, "A"])) /
    stats::sd(w_conn[, "A"])
  expect_equal(res, .build_connected(w_std, cm_conn))

  expect_equal(mean(res$w[, "A"]), 0, tolerance = 1e-10)
  expect_equal(stats::sd(res$w[, "A"]), 1, tolerance = 1e-10)
})

test_that("sum_standardize = FALSE keeps the raw sum score scale", {
  res <- rccme_calib_me(w_conn, cov_mats = cm_conn, sum_standardize = FALSE)
  expect_equal(res, .build_connected(w_conn, cm_conn))
})

test_that("cov_mats cannot be combined with the older inputs", {
  expect_error(
    rccme_calib_me(w_conn, rel_vec = c(0.8, 0.8), cov_mats = cm_conn),
    "cannot be combined"
  )
  expect_error(
    rccme_calib_me(w_conn, w_se_vec = c(0.4, 0.4), cov_mats = cm_conn),
    "cannot be combined"
  )
  expect_error(
    rccme_calib_me(
      w_conn, z_mat = matrix(rnorm(50), 50, 1), cov_mats = cm_conn
    ),
    "not yet supported"
  )
})

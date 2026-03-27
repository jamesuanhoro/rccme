test_that("validate fails with no error inputs", {
  w_mat <- matrix(rnorm(20), ncol = 2)

  expect_error(
    .validate_calib_me(w_mat, z_mat = matrix(0, 10, 0))
  )
})

test_that("validate fails with multiple error inputs", {
  w_mat <- matrix(rnorm(20), ncol = 2)

  expect_error(
    .validate_calib_me(
      w_mat,
      rel_vec = c(0.8, 0.9), w_se_vec = c(0.2, 0.3), z_mat = matrix(0, 10, 0)
    )
  )
})

test_that("validate checks rel_vec length", {
  w_mat <- matrix(rnorm(20), ncol = 2)

  expect_error(
    .validate_calib_me(w_mat, rel_vec = 0.8, z_mat = matrix(0, 10, 0))
  )
})

test_that("validate rejects constant columns", {
  w_mat <- cbind(rep(1, 10), rnorm(10))

  expect_error(
    .validate_calib_me(w_mat, rel_vec = c(0.8, 0.7), z_mat = matrix(0, 10, 0))
  )
})

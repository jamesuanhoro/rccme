test_that("optim_cov_fiml matches sample moments without missing", {
  set.seed(1)

  x_mat <- matrix(rnorm(200), ncol = 2)

  res <- .optim_cov_fiml(x_mat)

  mu_hat <- colMeans(x_mat)
  cov_hat <- cov(x_mat)

  expect_equal(res$mu, mu_hat, tolerance = 1e-1)
  expect_equal(res$c_mat, cov_hat, tolerance = 1e-1)
})

test_that("optim_cov_fiml works with missing data", {
  set.seed(1)

  x_mat <- matrix(rnorm(200), ncol = 2)
  x_mat[sample(length(x_mat), 20)] <- NA

  res <- .optim_cov_fiml(x_mat)

  expect_true(is.numeric(res$mu))
  expect_true(is.matrix(res$c_mat))
  expect_equal(length(res$mu), ncol(x_mat))
  expect_equal(dim(res$c_mat), c(2, 2))
  expect_false(any(is.na(res$mu)))
  expect_false(any(is.na(res$c_mat)))
})

test_that("cov_xy_fiml matches cov() without missing", {
  set.seed(1)

  x_mat <- matrix(rnorm(200), ncol = 2)
  y_mat <- matrix(rnorm(300), ncol = 3)

  mu_x <- colMeans(x_mat)
  mu_y <- colMeans(y_mat)
  cov_y <- cov(y_mat)

  res <- .cov_xy_fiml(x_mat, y_mat, mu_x, mu_y, cov_y)

  cov_xy_ref <- cov(x_mat, y_mat)

  expect_equal(res, cov_xy_ref, tolerance = 1e-1)
})

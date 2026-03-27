test_that("rccme_calib_me works with rel_vec", {
  set.seed(1)
  w_mat <- matrix(rnorm(100), ncol = 2)
  rel_vec <- c(0.8, 0.7)

  res <- rccme_calib_me(w_mat, rel_vec = rel_vec)

  expect_true(is.matrix(res))
  expect_equal(dim(res), dim(w_mat))
})

test_that("rccme_calib_me works with w_se_vec", {
  set.seed(1)
  w_mat <- matrix(rnorm(100), ncol = 2)
  w_se_vec <- c(0.3, 0.4)

  res <- rccme_calib_me(w_mat, w_se_vec = w_se_vec)

  expect_true(is.matrix(res))
  expect_equal(dim(res), dim(w_mat))
})

test_that("rccme_calib_me works with w_se_mat", {
  set.seed(1)
  w_mat <- matrix(rnorm(100), ncol = 2)
  w_se_mat <- matrix(runif(100, 0.2, 0.5), ncol = 2)

  res <- rccme_calib_me(w_mat, w_se_mat = w_se_mat)

  expect_true(is.matrix(res))
  expect_equal(dim(res), dim(w_mat))
})

test_that("rccme_calib_me handles rel_vec and z_mat", {
  set.seed(1)
  w_mat <- matrix(rnorm(100), ncol = 2)
  rel_vec <- c(0.8, 0.7)
  z_mat <- matrix(rnorm(150), ncol = 3)

  res <- rccme_calib_me(w_mat, rel_vec = rel_vec, z_mat = z_mat)

  expect_equal(dim(res), dim(w_mat))
})

test_that("rccme_calib_me handles w_se_vec and z_mat", {
  set.seed(1)
  w_mat <- matrix(rnorm(100), ncol = 2)
  w_se_vec <- c(0.3, 0.4)
  z_mat <- matrix(rnorm(150), ncol = 3)

  res <- rccme_calib_me(w_mat, w_se_vec = w_se_vec, z_mat = z_mat)

  expect_equal(dim(res), dim(w_mat))
})

test_that("rccme_calib_me handles w_se_mat and z_mat", {
  set.seed(1)
  w_mat <- matrix(rnorm(100), ncol = 2)
  w_se_mat <- matrix(runif(100, 0.2, 0.5), ncol = 2)
  z_mat <- matrix(rnorm(150), ncol = 3)

  res <- rccme_calib_me(w_mat, w_se_mat = w_se_mat, z_mat = z_mat)

  expect_equal(dim(res), dim(w_mat))
})

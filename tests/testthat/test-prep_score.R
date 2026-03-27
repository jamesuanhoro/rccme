test_that("prep_score with rel_vec rescales correctly", {
  set.seed(1)
  w_mat <- matrix(rnorm(100), ncol = 2)
  rel_vec <- c(0.8, 0.7)

  res <- .prep_score(w_mat, rel_vec = rel_vec)

  expect_true(is.matrix(res$w_mat))
  expect_equal(length(res$w_se_vec), 2)
})

test_that("prep_score with w_se_vec returns adjusted values", {
  set.seed(1)
  w_mat <- matrix(rnorm(100), ncol = 2)
  w_se_vec <- c(0.3, 0.4)

  res <- .prep_score(w_mat, w_se_vec = w_se_vec)

  expect_true(is.matrix(res$w_mat))
  expect_equal(length(res$w_se_vec), 2)
})

test_that("prep_score with w_se_mat returns adjusted values", {
  set.seed(1)
  w_mat <- matrix(rnorm(100), ncol = 2)
  w_se_mat <- matrix(runif(100, 0.2, 0.5), ncol = 2)

  res <- .prep_score(w_mat, w_se_mat = w_se_mat)

  expect_true(is.matrix(res$w_mat))
  expect_equal(dim(res$w_se_mat), dim(w_mat))
})

# Checks for rccme_cov_mats.R. Each case has an answer that can be
# worked out by hand, so a pass means the arithmetic is right, not
# only that the code runs.

set.seed(20240517)

# Fails unless expr stops with a message matching pattern, so each
# case fails for the reason it is meant to catch.
expect_msg_error <- function(expr, pattern) {
  msg <- tryCatch({
    force(expr)
    ""
  }, error = function(e) conditionMessage(e))
  stopifnot(grepl(pattern, msg))
}

# Validation: valid input passes

w_mat <- matrix(rnorm(30), 10, 3, dimnames = list(NULL, c("A", "B", "C")))
phi_2 <- matrix(c(1, 0.3, 0.3, 1), 2, 2)
v_2 <- matrix(c(0.2, 0.05, 0.05, 0.25), 2, 2)
phi_irt <- matrix(c(1, 0.2, 0.2, 1), 2, 2)
v_low <- matrix(c(0.093, 0.005, 0.005, 0.154), 2, 2)
acov_list <- lapply(1:10, function(i) v_low)

invisible(.validate_cov_mats(w_mat, list(
  list(rel = 0.8, cons = "A"),
  list(cov = phi_2, acov = v_2, cons = c("B", "C"))
)))

# acov as one matrix per respondent, the shape mirt returns
invisible(.validate_cov_mats(w_mat, list(
  list(rel = 0.8, cons = "A"),
  list(cov = phi_irt, acov = acov_list, cons = c("B", "C"))
)))

# Validation: each bad input stops with the right message

expect_msg_error(.validate_cov_mats(w_mat, list(
  list(rel = 0.8, cons = "A"),
  list(rel = 0.8, cons = "A"),
  list(cov = phi_2, acov = v_2, cons = c("B", "C"))
)), "duplicated")

expect_msg_error(.validate_cov_mats(w_mat, list(
  list(rel = 0.8, cons = "A"),
  list(rel = 0.8, cons = "B")
)), "exactly cover")

expect_msg_error(.validate_cov_mats(w_mat, list(
  list(rel = 0.8, cov = matrix(1, 1, 1), cons = "A"),
  list(cov = phi_2, acov = v_2, cons = c("B", "C"))
)), "mixes `rel`")

expect_msg_error(.validate_cov_mats(w_mat, list(
  list(rel = 0.8, cons = "A"),
  list(acov = v_2, cons = c("B", "C"))
)), "without `cov`")

# se cannot describe the error between two jointly modeled columns
expect_msg_error(.validate_cov_mats(w_mat, list(
  list(rel = 0.8, cons = "A"),
  list(cov = phi_2, se = c(0.4, 0.5), cons = c("B", "C"))
)), "multiple columns")

expect_msg_error(.validate_cov_mats(w_mat, list(
  list(rel = 0.8, cons = "A"),
  list(cov = matrix(1, 1, 1), acov = v_2, cons = c("B", "C"))
)), "matrix of finite values")

# cov alone says nothing about the entry's error
expect_msg_error(.validate_cov_mats(w_mat, list(
  list(rel = 0.8, cons = "A"),
  list(rel = 0.8, cons = "B"),
  list(cov = matrix(1, 1, 1), cons = "C")
)), "no `se` or")

# se padded with NA, which is what a length-1 se turns into when it
# is indexed with a row mask
expect_msg_error(.validate_cov_mats(w_mat, list(
  list(rel = 0.8, cons = "A"),
  list(rel = 0.8, cons = "B"),
  list(cov = matrix(1, 1, 1), se = c(0.3, rep(NA, 9)), cons = "C")
)), "NA or infinite")

expect_msg_error(.validate_cov_mats(w_mat, list(
  list(rel = 0.8, cons = "A"),
  list(cov = matrix(c(1, NA, NA, 1), 2, 2), acov = v_2,
       cons = c("B", "C"))
)), "matrix of finite values")

# every matrix in an acov list is checked, not only the first
acov_bad_size <- acov_list
acov_bad_size[[7]] <- diag(3)
expect_msg_error(.validate_cov_mats(w_mat, list(
  list(rel = 0.8, cons = "A"),
  list(cov = phi_irt, acov = acov_bad_size, cons = c("B", "C"))
)), "length-n")

acov_bad_na <- acov_list
acov_bad_na[[4]][1, 2] <- NA
expect_msg_error(.validate_cov_mats(w_mat, list(
  list(rel = 0.8, cons = "A"),
  list(cov = phi_irt, acov = acov_bad_na, cons = c("B", "C"))
)), "length-n")

# error variance at or above phi would make psi infinite or negative
expect_msg_error(.validate_cov_mats(w_mat, list(
  list(rel = 0.8, cons = "A"),
  list(rel = 0.8, cons = "B"),
  list(cov = matrix(1, 1, 1), se = c(rep(0.5, 9), 1.1), cons = "C")
)), "for 1 respondent")

acov_too_big <- acov_list
acov_too_big[[3]] <- diag(2) * 1.5
expect_msg_error(.validate_cov_mats(w_mat, list(
  list(rel = 0.8, cons = "A"),
  list(cov = phi_irt, acov = acov_too_big, cons = c("B", "C"))
)), "for 1 respondent")

# Equation 3, one construct. With phi = 1 and v = 0.36, omega is
# 0.64, so a raw score of 0.64 maps back to 1.

one <- .entry_parts(
  matrix(0.64, 1, 1, dimnames = list(NULL, "C")),
  list(cov = matrix(1), se = 0.6, cons = "C")
)
stopifnot(isTRUE(all.equal(one$x_star[1, 1], 1)))
stopifnot(isTRUE(all.equal(one$psi[1, 1], 1 / (1 / 0.36 - 1))))

# Equation 3, two constructs with a different error covariance for
# each person, as in a two-dimensional IRT model. One person has
# about four times the error of the other, as happens between precise
# and imprecise respondents. Raw scores are built as omega_i t_i, so the
# transformed scores must give back t_i.

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
stopifnot(isTRUE(all.equal(unname(two$x_star), t_two)))
stopifnot(isTRUE(all.equal(
  two$psi[[2]], solve(solve(v_high) - solve(phi_irt))
)))

# Equation 4, main simulation. A is a classical score: true score
# plus independent noise. B and C are a factor-score pair generated
# as equation 1 describes, x = omega t + e with Var(e) = omega v. For
# such scores Var(x_star) = phi + psi, so with a large sample cov(w)
# should be close to theta + delta. That is the identity equation 5
# relies on, so it checks w, delta, and theta together.

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

# w: A unchanged, B and C multiplied by omega inverse
stopifnot(isTRUE(all.equal(out$w[, "A"], w_mat_sim[, "A"])))
stopifnot(isTRUE(all.equal(
  unname(out$w[, c("B", "C")]),
  unname(raw_bc %*% t(solve(omega_bc)))
)))

# delta: one matrix, since nobody's error differs
stopifnot(is.matrix(out$delta))
stopifnot(isTRUE(all.equal(
  unname(out$delta[c("B", "C"), c("B", "C")]), psi_bc
)))
stopifnot(isTRUE(all.equal(out$delta["A", "A"], se_a^2)))
stopifnot(all(out$delta["A", c("B", "C")] == 0))

# theta: phi copied exactly on the (B, C) block, the rest estimated
stopifnot(isTRUE(all.equal(
  unname(out$theta[c("B", "C"), c("B", "C")]), unname(phi_bc)
)))
stopifnot(max(abs(out$theta - sigma_true)) < 0.02)

stopifnot(max(abs(cov(out$w) - (out$theta + out$delta))) < 0.02)

# The per-person path agrees with the shared path when every person
# has the same acov matrix.

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
stopifnot(isTRUE(all.equal(out_fixed$w, out_listed$w)))
stopifnot(is.list(out_listed$delta), length(out_listed$delta) == 200)
for (i in 1:200) {
  stopifnot(isTRUE(all.equal(out_listed$delta[[i]], out_fixed$delta)))
}

# A single IRT score with a per-person se, next to a score given by
# reliability. Each person's psi is 1 / (1 / se_i^2 - 1 / phi) and
# x_star is x / (1 - se_i^2 / phi). A's psi is the squared se
# derived from its reliability.

w_irt <- matrix(rnorm(10), 5, 2, dimnames = list(NULL, c("A", "D")))
se_d <- c(0.30, 0.45, 0.25, 0.60, 0.35)
out_irt <- .build_connected(w_irt, list(
  list(rel = 0.8, cons = "A"),
  list(cov = matrix(1.1), se = se_d, cons = "D")
))
se_a_rel <- sd(w_irt[, "A"]) * sqrt(1 - 0.8)
stopifnot(is.list(out_irt$delta), length(out_irt$delta) == 5)
for (i in 1:5) {
  stopifnot(isTRUE(all.equal(
    out_irt$delta[[i]]["D", "D"], 1 / (1 / se_d[i]^2 - 1 / 1.1)
  )))
  stopifnot(isTRUE(all.equal(out_irt$delta[[i]]["A", "A"], se_a_rel^2)))
  stopifnot(out_irt$delta[[i]]["A", "D"] == 0)
}
stopifnot(isTRUE(all.equal(
  out_irt$w[, "D"], w_irt[, "D"] / (1 - se_d^2 / 1.1)
)))

# Entries listed in a different order from the columns of w_mat give
# the same output, in w_mat's column order.

out_rev <- .build_connected(w_mat_sim, rev(cov_mats_sim))
stopifnot(isTRUE(all.equal(out_rev, out)))

cat("All checks passed.\n")

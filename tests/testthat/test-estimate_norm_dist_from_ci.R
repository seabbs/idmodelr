context("estimate_norm_dist_from_ci.R")


## Run function to estimate normal distribution for a 95% CI of 1, to 2
df <- estimate_norm_dist_from_ci(1,2)
## Check
x <- rnorm(10000, df$mean, df$sd)

q_x <- quantile(x, c(0.025, 0.975))

test_that("estimate_norm_dist_from_ci can correctly estimate the normal distribution", {
  expect_equal(c(1,2), unname(round(q_x, digits = 1)))
})


test_that("estimate_norm_dist_from_ci can only handle 95% intervals", {
  expect_error(estimate_norm_dist_from_ci(1,2, interval = "99%"))
})

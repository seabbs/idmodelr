context("SIS_demographics_ode")



S_0 <- 999
I_0 <- 1
beta <- 3
chi <- 0.5
mu <- 1/81

parameters <- c(beta = beta, mu = mu, chi = chi)
inits <- c(S = S_0, I = I_0)

test <- SIS_demographics_ode(1, inits, parameters)


test_that("SIS_demographics_ode can correctly return rates of change over one timestep", {
  skip_on_cran()
  expect_known_output(test, file = "../../tests/test-files/SIS_demographics_ode/test-01.rds")
})

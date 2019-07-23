context("SIRS_ode")



 ##Model Input
S_0 <- 989
I_0 <- 1
R_0 <- 0
beta <- 3
tau <- 2
chi <- 0.5
dt <- 1

parameters <- c(beta = beta, tau = tau, chi = chi)
inits <- c(S = S_0, I = I_0, R_0 = R_0)

test <- SIRS_ode(1, inits, parameters)


test_that("SIRS_ode can correctly return rates of change over one timestep", {
  skip_on_cran()
  expect_known_output(test, file = "../../tests/test-files/SIRS_ode/test-01.rds")
})

context("SIR_ode")



 ##Model Input
S_0 <- 989
I_0 <- 1
R_0 <- 0
beta <- 3
tau <- 2
dt <- 1

parameters <- c(beta = beta, tau = tau)
inits <- c(S = S_0, I = I_0, R_0 = R_0)

test <- SIR_ode(1, inits, parameters)


test_that("SIR_ode can correctly return rates of change over one timestep", {
  skip_on_cran()
  expect_known_output(test, file = "../../tests/test-files/SIR_ode/test-01.rds")
})

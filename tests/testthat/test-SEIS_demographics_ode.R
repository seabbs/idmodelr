context("SEIS_demographics_ode")




S_0 <- 989
E_0 <- 10
I_0 <- 1

beta <- 3
gamma = 1/2
chi <- 0.5
dt <- 1
mu = 1/84

parameters <- c(beta = beta, gamma = gamma, mu = mu)
inits <- c(S = S_0, E = E_0, I = I_0)

test <- SEIS_demographics_ode(1, inits, parameters)


test_that("SEIS_demographics_ode can correctly return rates of change over one timestep", {
  expect_known_output(test, file = "../../tests/test-files/SEIS_demographics_ode/test-01.rds")
})

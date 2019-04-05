context("SI_demo_ode")



S_0 <- 999
I_0 <- 1
beta <- 3
mu <- 1/81

parameters <- c(beta = beta, mu = mu)
inits <- c(S = S_0, I = I_0)

test <- SI_demo_ode(1, inits, parameters)


test_that("SI_demo_ode can correctly return rates of change over one timestep", {
  expect_known_output(test, file = "../../tests/test-files/SI_demo_ode/test-01.rds")
})

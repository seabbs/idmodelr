context("SIo_ode")



S_0 <- 999
I_0 <- 1
beta <- 3

parameters <- c(beta = beta)
inits <- c(S = S_0, I = I_0)

test <- SI_ode(1, inits, parameters)


test_that("SI_ode can correctly return rates of change over one timestep", {
  expect_known_output(test, file = "../../tests/test-files/SI_ode/test-01.rds")
})

context("SIS_ode")



S_0 <- 999
I_0 <- 1
beta <- 3
chi <- 0.5

parameters <- c(beta = beta, chi = chi)
inits <- c(S = S_0, I = I_0)

test <- SIS_ode(1, inits, parameters)


test_that("SIS_ode can correctly return rates of change over one timestep", {
  expect_known_output(test, file = "../../tests/test-files/SIS_ode/test-01.rds")
})

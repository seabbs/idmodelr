context("SI_ode.R")



S_0 <- 999
I_0 <- 1
beta <- 3

parameters <- c(beta = beta)
inits <- c(S = S_0, I = I_0)

test <- SI_ode(1, inits, parameters)

result <- list(c(S = -2.997, S = 2.997))

test_that("SI_ode can correctly return rates of change over one timestep", {
  expect_equal(result, test)
})

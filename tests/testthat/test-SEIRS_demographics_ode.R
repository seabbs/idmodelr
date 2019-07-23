context("SEIRS_demographics_ode")



##Model Input
S_0 <- 989
E_0 <- 10
I_0 <- 1
R_0 <- 0
beta <- 3
gamma <-  1/2
tau <- 2
chi <- 0.5
mu <- 1/81
dt <- 1

parameters <- c(beta = beta, gamma = gamma, tau = tau, mu = mu,
                chi = chi)
inits <- c(S = S_0, E = E_0, I = I_0, R_0 = R_0)

test <- SEIRS_demographics_ode(1, inits, parameters)


test_that("SEIRS_demographics_ode can correctly return rates of change over one timestep", {
  skip_on_cran()
  expect_known_output(test, file = "../../tests/test-files/SEIRS_demographics_ode/test-01.rds")
})

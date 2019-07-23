context("SIR_vaccination_ode")



##Model Input
S_u_0 <- 989
I_u_0 <- 1
R_u_0 <- 0
S_v_0 <- 0
I_v_0 <- 0
R_v_0 <- 0
beta <- 3
tau <- 2
lambda <- 0.7
dt <- 1

parameters <- c(beta = beta, tau = tau,
                lambda = 0.7)
inits <- c(S_u = S_u_0, I_u = I_u_0, R_u_0 = R_u_0,
           S_v = S_v_0, I_v = I_v_0, R_v_0 = R_v_0)

test <- SIR_vaccination_ode(1, inits, parameters)


test_that("SIR_vaccination_ode can correctly return rates of change over one timestep", {
  skip_on_cran()
  expect_known_output(test, file = "../../tests/test-files/SIR_vaccination_ode/test-01.rds")
})

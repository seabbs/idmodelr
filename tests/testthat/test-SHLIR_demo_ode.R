context("SHLIR_demo_ode")



##Model Input
S_0 <- 989
H_0 <- 10
L_0 <- 0
I_0 <- 1
R_0 <- 0
beta = 3 # Rate of transmission
gamma_H = 1/5 # Rate of progression to active symptoms from high risk latent
nu = 1/2 #Rate of progression from high to low risk latent
gamma_L = 1/100 # Rate of progression to active symptoms for low risk latent
tau = 1/2 # Rate of recovery
mu = 1/81 # Rate of natural mortality
dt <- 1

parameters <- c(beta = beta, gamma_H = gamma_H, gamma_L = gamma_L, nu = nu, tau = tau, mu = mu)
inits <- c(S = S_0, H = H_0, L = L_0, I = I_0, R_0 = R_0)

test <- SHLIR_demo_ode(1, inits, parameters)


test_that("SHLIR_demo_ode can correctly return rates of change over one timestep", {
  expect_known_output(test, file = "../../tests/test-files/SHLIR_demo_ode/test-01.rds")
})

context("SHLITR_risk_group_ode")



## initialise
inits <- c(
# General population
S = 800,
H = 0,
L = 0,
I = 0,
Tr = 0,
R = 0,
## High risk population
S_H = 199,
H_H = 0,
L_H = 0,
I_H = 1,
Tr_H = 0,
R_H = 0
 )

parameters <- c(
 beta = 3, # Rate of transmission
 beta_H = 6, # High risk rate of transmission
 gamma_H = 1/5, # Rate of progression to active symptoms from high risk latent
 nu = 1/2, #Rate of progression from high to low risk latent
 gamma_L = 1/100, # Rate of progression to active symptoms for low risk latent
 epsilon = 1/3, # Rate of treatment
 tau = 1/2, # Rate of recovery
 mu = 1/81, # Rate of natural mortality
 p = 0.2, # proportion of new births that are high risk
 M = 0.2 # Between group mixing
 )

test <- SHLITR_risk_group_ode(1, inits, parameters)


test_that("SHLITR_risk_group_ode can correctly return rates of change over one timestep", {
  expect_known_output(test, file = "../../tests/test-files/SHLITR_risk_group_ode/test-01.rds")
})

context("simulate_model")


N = 100000
I_0 = 1
S_0 = N - I_0
R_0 = 1.1
beta = R_0

##Time for model to run over
tbegin = 0
tend = 50
times <- seq(tbegin, tend, 1)

##Vectorise input
parameters <- c(beta = beta)
inits <- c(S = S_0, I = I_0)

test <- simulate_model(model = SI_ode, sim_fn = solve_ode, inits, parameters, times)

test_that("simulate_model can correctly solve the SI_ode", {
  expect_known_output(test, file = "../../tests/test-files/simulate_model/test-01.rds")
})

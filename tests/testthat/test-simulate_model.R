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

  test <- simulate_model(model = SI_ode, sim_fn = solve_ode, inits, parameters, times)

  skip_on_cran()
  expect_known_output(test, file = "../../tests/test-files/simulate_model/test-01.rds")
})


test_that("simulate_model errors when given parameters as a list", {

  expect_error(simulate_model(model = SI_ode, sim_fn = solve_ode, inits, as.list(parameters), times))
})

test_that("simulate_model errors when given initial conditions as a list", {

  expect_error(simulate_model(model = SI_ode, sim_fn = solve_ode, as.list(inits), parameters, times))
})


test_that("simulate_model errors when initial conditions and parameters have different numbers of rows.", {

  inits_df <- data.frame(S = rep(S_0, 2), I = rep(I_0, 2))
  parameters_df <- data.frame(beta = beta)
  expect_error(simulate_model(model = SI_ode, sim_fn = solve_ode, inits_df, parameters_df, times))
})


test_that("simulate_model can handle multiple simulations at once.", {

  inits_df <- data.frame(S = rep(S_0, 2), I = rep(I_0, 2))
  parameters_df <- data.frame(beta = rep(beta, 2))
  test <- simulate_model(model = SI_ode, sim_fn = solve_ode, inits_df, parameters_df,
                         times, by_row = TRUE, verbose = TRUE)
  skip_on_cran()
  expect_known_output(test, file = "../../tests/test-files/simulate_model/test-02.rds")
})

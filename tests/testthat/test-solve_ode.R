context("solve_ode")


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
parameters <- as.matrix(c(beta = beta))
inits <- as.matrix(c(S = S_0, I = I_0))

test <- solve_ode(model = SI_ode, inits, parameters, times, as.data.frame = TRUE)


test_that("solve_ode can correctly solve the SI_ode", {
  skip_on_cran()
  expect_known_output(test, file = "../../tests/test-files/solve_ode/test-01.rds")
})

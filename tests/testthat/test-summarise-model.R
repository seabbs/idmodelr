context("summarise")


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

test <- summarise_model(test)

test_that("summarise_mdoel can correctly summarise the SI_ode", {
  expect_known_output(test, file = "../../tests/test-files/summarise_model/test-01.rds")
})

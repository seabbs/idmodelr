context("solve_ode.R")

## Intialise
N = 1000
I_0 = 1
S_0 = N - I_0
R_0 = 1.1
beta = R_0
inits <- c(S = S_0, I = I_0)
parameters <- c(beta = beta)
##Time for model to run over
tbegin = 0
tend = 50
times <- seq(tbegin, tend, 1)

test <- solve_ode(model = SI_ode, inits, parameters, times, as.data.frame = TRUE)
test <- head(test)
test <- round(test, digits = 0)


result <- tibble::tibble(time = 0:5,
                         S = c(999, 997, 991, 974, 925, 803),
                         I = c(1, 3, 9, 26, 75, 197)
                         )
result <- round(result, digits = 0)


test_that("solve_ode can run a simple model simulation", {
  expect_equal(result, test)
})

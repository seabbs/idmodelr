context("solve_ode.R")

## Intialise
N = 100000
I_0 = 1
S_0 = N - I_0
R_0 = 1.1
beta = R_0

##Time for model to run over
tbegin = 0
tend = 50
times <- seq(tbegin, tend, 1)

test <- solve_ode(model = SI_ode, inits, parameters, times, as.data.frame = TRUE)
test <- head(test)
test <- round(test, digits = 0)


result <- tibble::tibble(time = 0:5, 
                         S = c(999, 980, 712, 110, 6, 0),
                         I = c(1, 20, 288, 890, 994, 1000)
                         )
result <- round(result, digits = 0)


test_that("solve_ode can run a simple model simulation", {
  expect_equal(result, test)
})

context("plot_model")

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

 ##Vectorise input
parameters <- as.matrix(c(beta = beta))
inits <- as.matrix(c(S = S_0, I = I_0))

sim <- solve_ode(model = SI_ode, inits, parameters, times, as.data.frame = TRUE)

plot_model(sim, facet = FALSE)

plot_model(sim, facet = TRUE)

## Compare with an updated model run

## Intialise
R_0 = 1.3
beta = R_0
parameters <- as.matrix(c(beta = beta))

new_sim <- solve_ode(model = SI_ode, inits, parameters, times, as.data.frame = TRUE)

test_that("plot_model fails when both a list and a previous simulation are supplied", {

  expect_error(plot_model(list(new_sim, sim), sim))
})


test_that("plot_model produces a plot", {
  plot <- plot_model(sim)

  expect_true(!is.null(plot))
  expect_equal("ggplot", class(plot)[2])
  skip_on_cran()
  vdiffr::expect_doppelganger("base-plot", plot)
})

test_that("plot_model facets correctly", {
  plot <- plot_model(sim, facet = TRUE)

  expect_true(!is.null(plot))
  expect_equal("ggplot", class(plot)[2])
  skip_on_cran()
  vdiffr::expect_doppelganger("facet-plot", plot)
})

test_that("plot_model allows custom labels", {
  plot <- plot_model(sim, new_sim, model_labels = c("wow", "yer"))

  expect_true(!is.null(plot))
  expect_equal("ggplot", class(plot)[2])
  skip_on_cran()
  vdiffr::expect_doppelganger("label-plot", plot)
})

test_that("plot_model uses labels when plots are a list", {
  plot <- plot_model(list(sim, new_sim), model_labels = c("wow", "yer"), facet = FALSE)

  expect_true(!is.null(plot))
  expect_equal("ggplot", class(plot)[2])
  skip_on_cran()
  vdiffr::expect_doppelganger("list-label-plot", plot)
})

test_that("plot_model can provide default labels when using a list", {
  plot <- plot_model(list(sim, new_sim))

  expect_true(!is.null(plot))
  expect_equal("ggplot", class(plot)[2])
  skip_on_cran()
  vdiffr::expect_doppelganger("list-label-default-plot", plot)
})

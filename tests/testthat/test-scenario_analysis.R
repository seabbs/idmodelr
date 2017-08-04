context("scenario_analysis")

## Set up testing parameter set
scenarios <- tibble::data_frame(scenario = c("test_1", "test_2"), scenario_param = c(0, 1))
variable_params <-  tibble::data_frame(variable = c(0, 0.5, 1))
fixed_params <- c(fixed_1 = 2, fixed_2 = c(1, 3, 4))
sample_params <- c(sample_1 = 2, sample_2 = c(2, 1))

parameter_df <- generate_parameter_permutations(variable_params, fixed_params, sample_params,
                                                excluded_params = c("variable"), scenarios,
                                                parameter_samples = 10, save = FALSE)

## set up dummy simulation function (returning an empty dataframe)
dummy_sim_fn <- function(object, inits, params, times, as.data.frame) {
  x <- tibble::tibble()
  return(x)
  }
dummy_model <- function(){}

## Results to check
variations <- tibble::tibble(scenario = rep(c("test_1", "test_2"), 3),
                             variable = c(0.0, 0.0, 0.5, 0.5, 1.0, 1.0))

parameters <- tibble::tibble(variable = rep(0, 10), scenario_param = rep(0, 10),
                             fixed_1 = rep(2, 10), fixed_21 = rep(1, 10), fixed_22 = rep(3, 10),
                             fixed_23 = rep(4, 10), sample_1 = rep(2, 10), sample_21 = rep(2, 10),
                             sample_22 = rep(1, 10))
simulations <- tibble::tibble()

test_that("scenaria_analysis works correctly on sample data
          with a dummy model and simulation function", {
   results <- scenario_analysis(parameter_df, variable_params = "variable",
                                model = dummy_model, sim_fn = dummy_sim_fn,
                                cores = 1, save = FALSE)

   expect_equal(variations, dplyr::select(results, scenario, variable))
   expect_equal(parameters, tidyr::unnest(dplyr::select(results[1,], parameters)))
   expect_equal(simulations, tidyr::unnest(dplyr::select(results[1,], simulations)))

          })


test_that("scenario_analysis works correctly on sample data
          with a dummy model and simulation function over multiple cores", {
            results <- scenario_analysis(parameter_df, variable_params = "variable",
                                         model = dummy_model, sim_fn = dummy_sim_fn,
                                         cores = 1, save = FALSE, test = TRUE)

            expect_equal(variations, dplyr::select(results, scenario, variable))
            expect_equal(parameters, tidyr::unnest(dplyr::select(results[1,], parameters)))
            expect_equal(simulations, tidyr::unnest(dplyr::select(results[1,], simulations)))

          })



#' A Function to Perform Scenario Analysis for a Generic Model Object.
#'
#' @inheritParams simulate_model
#' @inheritParams generate_parameter_permutations
#' @description This function uses parameter permutations produced by
#' \code{\link[idmodelr]{generate_parameter_permutations}} to simulate from a supplied model function.
#' It can be used to examine multiple scenarios, with any number of parameter variations, for multiple samples.
#' @param parameter_df A dataframe of parameter permutations as produced by \code{\link[idmodelr]{generate_parameter_permutations}}.
#' Using the default options it will save results when run for the first time, and afterwards load them in.
#' @param variable_params A character vector containing the names of the paramters that are varied in \code{parameter_df}.
#' @param cores The number of cores to use for the scenario analysis, defaults to 1.
#' @param test A logical (defaults to \code{FALSE}) if \code{TRUE} function uses multicore functionality regardless
#' of the number of cores specified.
#' @param summary_fn A function which accepts a single dataframe arguement customised to fit with the standard
#' output of \code{scenario_analysis}  and your \code{simulate_model} function. Defaults to \code{NULL} for which
#' no summarisation takes place. Warning: If a previous analysis has been saved, changing this option will not
#' summarise the result. The analysis must be rerun.
#' @param ... Pass additional arguements to sim_fn. Only implemented when a single core is used.
#' @return A tidy dataframe containing simulated model trajectories for each scenario
#'  varied parameter combination. Use \code{\link[tidyr]{unnest}} to examine all simulation results.
#' @export
#' @import magrittr
#' @importFrom dplyr select bind_cols group_by ungroup collect arrange_
#' @importFrom tidyr nest
#' @importFrom purrr map
#' @importFrom furrr future_map
#' @importFrom future plan multisession sequential
#' @examples
#'
#' scenarios <- data.frame(scenario = c("test_1", "test_2"), scenario_param = c(0, 1))
#' variable_params <-  data.frame(variable = c(0, 0.5, 1))
#' fixed_params <- c(fixed_1 = 2, fixed_2 = c(1, 3, 4))
#' sample_params <- c(sample_1 = 2, sample_2 = c(2, 1))
#'
#' parameter_df <- generate_parameter_permutations(variable_params, fixed_params, sample_params,
#'                                                 excluded_params = c("variable"), scenarios,
#'                                                 parameter_samples = 10)
#'
#' ## set up dummy simulation function (returning an empty dataframe)
#' dummy_sim_fn <- function(object, inits, params, times, as.data.frame) {
#'  x <- tibble::tibble()
#'  return(x)
#' }
#'
#'## Set up dummy summary function
#'dummy_sum_fn <- function(df){
#'df <- dplyr::mutate(df, summarised_simulations = simulations)
#'
#'return(df)
#'}
#' dummy_model <- function(){}
#'
#' ## Run scenario analysis
#' scenario_analysis(parameter_df, variable_params = "variable", model = dummy_model,
#'                   sim_fn = dummy_sim_fn, cores = 1, summary_fn = dummy_sum_fn)
scenario_analysis <- function(parameter_df, variable_params = NULL, model = NULL, sim_fn = NULL,
                              summary_fn = NULL, cores = 1, rerun = FALSE, verbose = FALSE,
                              by_row = FALSE, test = FALSE, ...) {
  scenario <- NULL; parameters <- NULL



  ## Run model trajectories
  scenario_results <- parameter_df %>%
    select(-sample)

  ## Set up temporary variable names for tracked parameters
  group_var_string <- "scenario"
  if (!is.null(variable_params)) {

    group_var_string <- c(group_var_string,
                          paste0("tmp_var_", 1:length(variable_params)))

    compute_var <- map(1:length(variable_params), function(x, df) {
      tmp <- df[variable_params[x]]
      colnames(tmp) <- paste0("tmp_var_", x)
      return(tmp)
    }, scenario_results) %>%
      bind_cols

    scenario_results <- compute_var %>%
      bind_cols(scenario_results) %>%
      select(scenario, everything())
  }

  # Group and nest by parameter sets
  scenario_results <- scenario_results %>%
    group_by(.dots = group_var_string) %>%
    nest(.key = parameters)

  # partition data between cores
  if (cores > 1 || test) {
    plan(multisession, workers = cores)
  }else{
    plan(sequential)
  }

  # run model simulations for each paramter set
  scenario_results <- scenario_results %>%
    mutate(simulations = future_map(parameters,
                                    ~ idmodelr::simulate_model(model,
                                                               sim_fn = sim_fn,
                                                               params = .,
                                                               as_tibble = TRUE,
                                                               by_row = by_row,
                                                               verbose = verbose,
                                                               ...),
                                    .progress = TRUE
    )
    )


  ##ungroup results
  scenario_results <- scenario_results %>%
    ungroup

  ## remove compute variable names and replace with actual variable names
  if (!is.null(variable_params)) {
    col_names <- colnames(scenario_results)
    col_names[colnames(scenario_results) %in% group_var_string[-1]] <- variable_params
    colnames(scenario_results) <- col_names
  }

  if (!is.null(summary_fn)) {
    scenario_results <- scenario_results %>%
      summary_fn
  }


  return(scenario_results)
}


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
#' @return A tidy dataframe containing simulated model trajectories for each scenario
#'  varied parameter combination. Use \code{\link[tidyr]{unnest}} to examine all simulation results.
#' @export
#' @import magrittr
#' @importFrom dplyr select bind_cols group_by ungroup collect arrange_
#' @importFrom tidyr nest
#' @importFrom purrr map
#' @importFrom multidplyr create_cluster partition
#' @importFrom parallel clusterExport
#' @examples
#'
#' scenarios <- tibble::data_frame(scenario = c("test_1", "test_2"), scenario_param = c(0, 1))
#' variable_params <-  tibble::data_frame(variable = c(0, 0.5, 1))
#' fixed_params <- c(fixed_1 = 2, fixed_2 = c(1, 3, 4))
#' sample_params <- c(sample_1 = 2, sample_2 = c(2, 1))
#'
#' parameter_df <- generate_parameter_permutations(variable_params, fixed_params, sample_params,
#'                                                 excluded_params = c("variable"), scenarios,
#'                                                 parameter_samples = 10, save = FALSE)
#'
#' ## set up dummy simulation function (returning an empty dataframe)
#' dummy_sim_fn <- function(object, inits, params, times, as.data.frame) {
#'  x <- tibble::tibble()
#'  return(x)
#' }
#'
#' dummy_model <- function(){}
#'
#' ## Run scenario analysis
#' scenario_analysis(parameter_df, variable_params = "variable", model = dummy_model,
#'                   sim_fn = dummy_sim_fn, rowwise = FALSE, cores = 1, save = FALSE)
scenario_analysis <- function(parameter_df, variable_params = NULL, model = NULL, sim_fn = NULL,
                              cores = 1, save = TRUE, save_name = "scenario_analysis_results",
                              save_path = NULL, save_format = NULL, rerun = FALSE,
                              verbose = FALSE, test = FALSE) {
  file_rds <- paste0(save_name, ".rds")
  file_path <- ifelse(!is.null(save_path),
                      file.path(save_path, file_rds),
                      file_rds)

  if (file.exists(file_path) && !rerun) {
    scenario_results <- readRDS(file_path)
    save <- FALSE
  }else {

    if (cores > 1 || test) {
      ## Set up cluster and export functions
      cluster <- create_cluster(cores)
      parallel::clusterExport(cluster, c(as.character(quote(model)),
                               as.character(quote(sim_fn))), envir = environment())
    }

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
      scenario_results <- scenario_results %>%
        partition(cluster = cluster)
    }

    # run model simulations for each paramter set
    scenario_results <- scenario_results %>%
      mutate(simulations = purrr::map(parameters,
                                       ~ idmodelr::simulate_model(model,
                                                                  sim_fn = sim_fn,
                                                                  params = .,
                                                                  as_tibble = TRUE,
                                                                  ...)
                                       )
      )

    ## collect results from each core
    if (cores > 1 || test) {
      scenario_results <- scenario_results %>%
        collect
    }

    ##ungroup results
    scenario_results <- scenario_results %>%
      ungroup

    ## drop leftover parallel variables
    if (cores > 1 || test) {
      scenario_results <- scenario_results %>%
        arrange_(.dots = c(group_var_string[-1], group_var_string[1])) %>%
        select(-PARTITION_ID)
    }
    ## remove compute variable names and replace with actual variable names
    if (!is.null(variable_params)) {
      col_names <- colnames(scenario_results)
      col_names[colnames(scenario_results) %in% group_var_string[-1]] <- variable_params
      colnames(scenario_results) <- col_names
    }
  }

  ## save results
  if (save) {
    saveRDS(scenario_results, file_path)
    if (!is.null(save_format)) {
      save_data(scenario_results, name = save_name,
                path = save_path, format = save_format)
    }
  }

  return(scenario_results)
}

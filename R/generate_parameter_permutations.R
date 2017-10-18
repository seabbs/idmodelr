#' A Function to Generate Parameter Permutations
#'
#' @description A function to generate paramter permutations from a generic sampling function (or if not given
#' from the inputed parameters). This function can be used to rapidly generate new parameter combinations given
#' parameters to be varied, and scenarios to be investigated.
#' @param variable_params A dataframe containing any parameter variations to be investigated. If
#' these paramters would normalling be sampled then they should be added to the excluded_params arguement.
#' @param fixed_params A named vector of parameters that are not sampled by the sampling function. If
#' these paramters would usually be sampled then they should be added to the excluded_params arguement.
#' @param sample_params A named vector of paramters to be sampled. If a sampling function is not supplied these
#' parameters will be used in the final permutation dataframe.
#' @param excluded_params A character vector indicating which parameters should have there sampled values
#' kept.
#' @param scenarios A dataframe of possible scenarios to investigate. It must contain a scenario variable to
#' identify each seperate scenarios. If parameters are included here that would normally be sampled then they
#' should be added to the excluded_params arguement
#' @param sampling_function A sampling function, this should be designed such that it's input is a matrix
#' with each paramter having a named row. It should return it's output in the same format. This has been tested with
#' the \code{rprior} function from the [pomp](https://kingaa.github.io/pomp/). If not supplied defaults to passing
#' through parameters, this may not be the required behaviour.
#' @param parameter_samples The number of parameter samples to take, defaults to one.
#' @param repeat_sample A logical (defaults to \code{TRUE}) which indicates if each scenario should independantly
#' sample from the sampling function. If set to \code{FALSE} then each scenario will share the same sampled parameter set.
#' @param save Logical specifying if the results should be saved. Defaults to \code{TRUE}.
#' @param save_name A character string of the name the results should be saved under.
#' @param save_path A character string indicating the saving location for the results.
#' If not specified defaults to saving in the working directory.
#' @param save_format If additional file formats are required, see \code{\link[idmodelr]{save_data}}.
#' @param rerun A logical indicating if the function should be rerun or saved results should be loaded.
#' Defaults to \code{FALSE}.
#' @param verbose A logical, indicating if progress messages should be printed, defaults to \code{FALSE}.
#' @param ... Additional arguements to be passed to the sampling_function.
#'
#' @return A dataframe containing sampled parameter permutations
#' @importFrom dplyr mutate full_join select bind_cols everything
#' @import magrittr
#' @importFrom tibble as_data_frame data_frame
#' @importFrom purrr map_df
#' @export
#'
#' @examples
#'
#'#' scenarios <- tibble::data_frame(scenario = c("test_1", "test_2"), scenario_param = c(0, 1))
#' variable_params <-  tibble::data_frame(variable = c(0, 0.5, 1))
#' fixed_params <- c(fixed_1 = 2, fixed_2 = c(1, 3, 4))
#' sample_params <- c(sample_1 = 2, sample_2 = c(2, 1))
#'
#' generate_parameter_permutations(variable_params, fixed_params, sample_params,
#'                                 excluded_params = c("variable"), scenarios,
#'                                 parameter_samples = 1, save = FALSE)
generate_parameter_permutations <- function(variable_params = NULL, fixed_params = NULL,
                                            sample_params = NULL, excluded_params = NULL,
                                            scenarios = NULL, sampling_function = NULL,
                                            parameter_samples = 1, repeat_sample = TRUE,
                                            save = TRUE, save_name = "parameter_permutations",
                                            save_path = NULL, save_format = NULL, rerun = FALSE,
                                            verbose = FALSE, ...) {
  ## defaults to saving file as RDS other options can be specified in addition
  file_rds <- paste0(save_name, ".rds")
  file_path <- ifelse(!is.null(save_path),
                      file.path(save_path, file_rds),
                      file_rds)

  if (file.exists(file_path) && !rerun) {
    sample_params <- readRDS(file_path)
    save <- FALSE
  }else {
    if (!is.null(variable_params)) {
      if (!"data.frame" %in% class(variable_params)) {
        stop("variable_params must be a data frame")
      }

      variable_params <- mutate(variable_params, id = 1)
    }

    if (!is.null(scenarios)) {
      if (!"data.frame" %in% class(scenarios)) {
        stop("scenarios must be a data frame")
      }
      scenarios <- mutate(scenarios, id = 1)
    }

    ## Bind scenarios and variable paramters together
    ## If neither are supplied then a sample of the normal parameters will be drawn
    if (!is.null(variable_params) && !is.null(scenarios)) {
      params_perms <- variable_params %>%
        full_join(scenarios, by = "id")
    }else if (is.null(variable_params) && !is.null(scenarios)) {
      params_perms <- scenarios
    }else if (!is.null(variable_params) && is.null(scenarios)) {
      params_perms <- variable_params
    }else{
      params_perms <- data_frame(id = 1)
    }

    if (!is.null(fixed_params) || !is.null(sample_params)) {
      if (is.null(fixed_params)) {
        join_params <- sample_params
      }else if (is.null(sample_params)) {
        join_params <- fixed_params
      }else {
        join_params <- c(fixed_params, sample_params)
      }

      ## munge join_params
      join_params <- join_params %>%
        t %>%
        as_data_frame %>%
        mutate(id = 1)

      params_perms <- params_perms %>%
        full_join(join_params, by = "id")
    }

    if (is.null(sampling_function)) {
      sampling_function <- function(params) {
        return(params)
      }
    }
    ## Generate parameter permutations

    # Number of samples to test
    sample_params <- map_df(1:parameter_samples, function(x, df, exc_params){
      params_as_matrix <- select(df, -id, -scenario) %>%
        as.matrix %>%
        t

      if (x == 1 | repeat_sample) {
        prior_sample  <- sampling_function(params = params_as_matrix, ...) %>%
          t %>%
          as_data_frame %>%
          mutate(sample = x) %>%
          select(sample, everything())
        if (!repeat_sample) assign("master_prior_sample", prior_sample, env = parent.frame())
      }else{
        prior_sample <- master_prior_sample
      }

      join_params <- c("id", "scenario")
      if (!is.null(exc_params)) {
        join_params <- c(join_params, exc_params)
        prior_sample <- prior_sample[, !(colnames(prior_sample) %in% exc_params)]
      }

      params_df <- df[,join_params] %>%
        bind_cols(prior_sample)

      return(params_df)
    }, params_perms, excluded_params) %>%
      select(-id)
  }

  ## save results
  if (save) {
    saveRDS(sample_params, file_path)
    if (!is.null(save_format)) {
      save_data(sample_params, name = save_name,
                path = save_path, format = save_format)
    }
  }

  return(sample_params)
}


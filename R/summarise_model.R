#' Summarise a Model Simulation
#'
#' @description Provides simple summary statistics for a model produced using
#' \code{\link[idmodelr]{solve_ode}}. These include the final population
#' sizes, the time and size of the maximum epidemic peak, and the duration of
#' the epidemic.
#' @param sim A tibble of model output as produced by
#' \code{\link[idmodelr]{solve_ode}}.
#'
#' @return A tibble of summary information for a model simulation.
#' @export
#' @importFrom dplyr filter pull arrange select bind_cols slice rename rename_at funs
#' @importFrom tibble tibble
#' @import magrittr
#' @examples
#'
#' ## Intialise
#'N = 100000
#'I_0 = 1
#'S_0 = N - I_0
#'R_0 = 1.1
#'beta = R_0
#'
#' ##Time for model to run over
#'tbegin = 0
#'tend = 50
#'times <- seq(tbegin, tend, 1)
#'
#' ##Vectorise input
#'parameters <- as.matrix(c(beta = beta))
#'inits <- as.matrix(c(S = S_0, I = I_0))
#'
#'sim <- solve_ode(model = SI_ode, inits, parameters, times, as.data.frame = TRUE)
#'
#'summarise_model(sim)

summarise_model <- function(sim) {

  time <- NULL; . <- NULL;

  epi_peak <- sim %>%
    filter(I == max(I)) %>%
    arrange(time) %>%
    slice(1)

  epi_peak_size <- epi_peak %>%
    pull(I)

  epi_peak_time <- epi_peak %>%
    pull(time)

  epi_dur <- sim %>%
    filter(I < 1) %>%
    arrange(I) %>%
    slice(1) %>%
    pull(time)

  if (length(epi_dur) == 0) {
    epi_dur <- Inf
  }

  sum_stat <- tibble(epi_peak_time = epi_peak_time,
                     epi_peak_size = round(epi_peak_size, digits = 0),
                     epi_dur = epi_dur)

  sum_stat <-  sim %>%
    filter(time == max(time)) %>%
    round(digits = 0) %>%
    select(-time) %>%
    bind_cols(sum_stat)

  ## Format output
  sum_stat <- sum_stat %>%
    rename_at(.vars = colnames(.)[!grepl("epi_", colnames(.))], .funs = funs(paste0("Final size: ", .))) %>%
    rename(`Epidemic peak time` = epi_peak_time,
           `Epidemic peak` = epi_peak_size,
           `Epidemic duration` = epi_dur)

  return(sum_stat)
}

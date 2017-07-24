#' Summarise Model Trajectories
#'
#' @description This function checks, cleans and summarises inputed model trajectories. This function is based on
#'   [plotTraj](https://github.com/sbfnk/fitR/blob/master/R/plot.r) from
#' the [fitR](https://github.com/sbfnk/fitR) package written by [Sebaustian Funk](http://sbfnk.github.io/).
#' @param traj Data frame, generic model output although tailored
#' to work with [pomp](https://www.rdocumentation.org/packages/pomp/versions/1.4.1.1/topics/pomp) model objects.
#' @param state.names A character vector. Names of the state variables to plot.
#' If \code{NULL} (default) all state variables are plotted.
#' @param data A data frame. Observation times and observed data. The time column must match that used in the model object, whilt
#' the observation name should also match one of those found in the model object.
#' @param time.column A character vector. The column in the data that indicates time
#' @param summary Logical. If \code{TRUE}, the mean, median as well as the 50th and 95th percentile
#' of the trajectories are plotted (default). If \code{FALSE}, all individual trajectories are plotted.
#' @param replicate.column A character vector. The column in the data that indicates the replicate
#'  (if muliple replicates are to be plotted, i.e. if \code{summary} is \code{FALSE}
#' @param non.extinct A character vector. Names of the infected states which must be non-zero so the epidemic is still ongoing.
#' When the names of these states are provided, the extinction probability is plotted by computing the proportion of faded-out
#' epidemics over time. An epidemic has faded-out when all the infected states (whose names are provided) are equal to 0.
#' This is only relevant for stochastic models.
#' @param init.date Character. Date of the first point of the time series (default to \code{NULL}). If provided, the x-axis will be
#' in calendar format. NB: currently only works if the unit of time is the day.
#' @param set_theme Set the ggplot2 theme, defaults to theme_minimal.
#' @param verbose Logical (defaults to \code{FALSE}), indicates if internal process messages should be printed
#' @import reshape2 ggplot2 stringr
#' @importFrom dplyr do mutate group_by
#' @importFrom tibble as_tibble
#' @return A list of summarised dataframes for an inputed model trajectory. These are the model trajectories, probability
#'   of extinction, and model summary trajectories.
#' @export
#' @seealso plot_model
#' @examples
#'
summarise_model <- function(traj = NULL, state.names = NULL, data = NULL, time.column = "time",
                            summary = TRUE, replicate.column = "replicate",
                            non.extinct = NULL, init.date = NULL, same = FALSE,
                            verbose = FALSE) {

  if (!is.null(init.date)) {
    init.date <- as.Date(init.date)
  }
  if (is.null(traj) && is.null(data)) {
    stop("No simulation or observed data inputed")
  }
  if (!is.null(traj) & !any(duplicated(traj[[time.column]]))) {
    traj[[replicate.column]] <- 1
    if (summary) {
      summary <- FALSE
    }
  }
  if (is.null(state.names)) {
    numeric.names <- names(traj)[sapply(names(traj), function(x) {
      any(class(traj[[x]]) %in% c("numeric", "integer"))
    })]
    state.names <- setdiff(numeric.names, c(time.column,
                                            replicate.column))
  }
  else if (!is.character(state.names))
    stop(sQuote("state.names"), ", if given, must be a numeric vector")
  if (!is.null(init.date)) {
    traj[[time.column]] <- traj[[time.column]] + init.date
    if (!is.null(data)) {
      data[[time.column]] <- data[[time.column]] + init.date
    }
  }


  if (!is.null(traj)) {
    if (!is.null(non.extinct)) {
      traj <- mutate(traj, infected = eval(parse(text = paste(non.extinct,
                                                              collapse = "+")), traj))
      df.infected <- melt(traj, measure.vars = "infected",
                          variable.name = "state")
      df.p.ext <- df.infected %>%
        group_by(time.column) %>%
        do(data.frame(value = sum(.$value == 0)/nrow(.)))

      df.p.ext$state <- "p.extinction"
      df.p.ext[replicate.column] <- 0
      if (summary) {
        traj <- subset(traj, infected > 0)
        traj$infected <- NULL
      }
    }else {
      df.p.ext <- NULL
    }
    df.traj <- melt(traj, measure.vars = state.names, variable.name = "state")
    df.traj <- subset(df.traj, !is.na(value))


    if (summary) {
      traj.CI <- df.traj %>%
        group_by(.dots = c(time.column, "state")) %>%
        do(data.frame(low_95 = quantile(.$value, prob = 0.025)[[1]],
                      low_50 = quantile(.$value, prob = 0.25)[[1]],
                      median = quantile(.$value, prob = 0.5)[[1]],
                      up_50 = quantile(.$value, prob = 0.75)[[1]],
                      up_95 = quantile(.$value, prob = 0.975)[[1]],
                      mean = mean(.$value)))
    }else{
      traj.CI <- NULL
    }

  }else{
    df.traj <- NULL
    df.p.ext <- NULL
    traj.CI <- NULL
  }

  sum_model <- c(df.traj, df.p.ext, traj.CI)
  names(sum_model) <- c("traj", "prob_ext", "sum_traj")
  class(sum_model) <- c(class(sum_model), "idmodelr")
  return(sum_model)
}

#' Summarise and Plot Model Trajectories
#' @description This function uses faceting to plot all trajectories in a data frame. It allows the conveniant
#' manipulation of several simulations or data. Additionally if data is present, then an additional plot can be displayed
#' with data and potentially observations generated. This function is heaviliy
#' based on [plotTraj](https://github.com/sbfnk/fitR/blob/master/R/plot.r) from
#' the [fitR](https://github.com/sbfnk/fitR) package written by [Sebaustian Funk](http://sbfnk.github.io/).
#' @param traj Data frame, generic model output although tailored
#' to work with [pomp](https://www.rdocumentation.org/packages/pomp/versions/1.4.1.1/topics/pomp) model objects.
#' @param state.names A character vector. Names of the state variables to plot.
#' If \code{NULL} (default) all state variables are plotted.
#' @param data A data frame. Observation times and observed data. The time column must match that used in the model object, whilt
#' the observation name should also match one of those found in the model object.
#' @param time.column A character vector. The column in the data that indicates time
#' @param lines.data Logical. If \code{TRUE}, the data will be plotted as lines.
#' @param summary Logical. If \code{TRUE}, the mean, median as well as the 50th and 95th percentile
#' of the trajectories are plotted (default). If \code{FALSE}, all individual trajectories are plotted
#'  (transparency can be set with \code{alpha}).
#' @param replicate.column A character vector. The column in the data that indicates the replicate
#'  (if muliple replicates are to be plotted, i.e. if \code{summary} is \code{FALSE}
#' @param non.extinct A character vector. Names of the infected states which must be non-zero so the epidemic is still ongoing.
#' When the names of these states are provided, the extinction probability is plotted by computing the proportion of faded-out
#' epidemics over time. An epidemic has faded-out when all the infected states (whose names are provided) are equal to 0.
#' This is only relevant for stochastic models.
#' @param alpha Transparency of the trajectories (between 0 and 1).
#' @param plot if \code{TRUE} the plot is displayed, and returned otherwise.
#' @param colour A character vector. If a character, will use that colour to plot trajectories. If "all", use all available colours.
#'  If \code{NULL}, don't set the colour.
#' @param init.date Character. Date of the first point of the time series (default to \code{NULL}). If provided, the x-axis will be
#' in calendar format. NB: currently only works if the unit of time is the day.
#' @param same Logical. If \code{TRUE} does not facet plots.
#' @param set_theme Set the ggplot2 theme, defaults to theme_minimal.
#' @import reshape2 ggplot2 stringr
#' @importFrom tibble as_tibble
#' @return Optionally prints or stores a plot of trajectories
#' @export
#'
#' @examples
#'
plot_model <- function(traj = NULL, state.names = NULL, data = NULL, time.column = "time",
                             lines.data = FALSE, summary = TRUE, replicate.column = "replicate",
                             non.extinct = NULL, alpha = 1, plot = TRUE, colour = "firebrick2", set_theme = theme_minimal,
                             init.date = NULL, same = FALSE)
{
  if (!is.null(init.date)) {
    init.date <- as.Date(init.date)
  }
  if (is.null(traj) && is.null(data)) {
    stop("Nothing to plot")
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
  if (colour == "all" && summary == TRUE) {
    warning("Ignoring ", sQuote("colour = \"all\""), " which doesn't make sense if ",
            sQuote("summary == TRUE"))
    colour <- NULL
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

        traj.CI.line <- melt(traj.CI[c(time.column, "state",
                                       "mean", "median")], id.vars = c(time.column,
                                                                       "state"))
        traj.CI.area <- melt(traj.CI[c(time.column, "state",
                                       "low_95", "low_50", "up_50", "up_95")], id.vars = c(time.column,
                                                                                           "state"))
        traj.CI.area$type <- sapply(traj.CI.area$variable,
                                    function(x) {
                                      str_split(x, "_")[[1]][1]
                                    })
        traj.CI.area$CI <- sapply(traj.CI.area$variable,
                                  function(x) {
                                    str_split(x, "_")[[1]][2]
                                  })
        traj.CI.area$variable <- NULL
        traj.CI.area <- dcast(traj.CI.area, paste0(time.column,
                                                   "+state+CI~type"))

        p <- ggplot(traj.CI.area)
        if (!same) {
          p <- p + facet_wrap(~state, scales = "free_y")
        }
        if (is.null(colour)) {
          p <- p + geom_ribbon(data = traj.CI.area, aes_string(x = time.column,
                                                               ymin = "low", ymax = "up", alpha = "CI"))
          p <- p + geom_line(data = traj.CI.line, aes_string(x = time.column,
                                                             y = "value", linetype = "variable"))
        }
        else if (colour == "all") {
          p <- p + geom_ribbon(data = traj.CI.area, aes_string(x = time.column,
                                                               ymin = "low", ymax = "up", alpha = "CI", fill = "state"))
          p <- p + geom_line(data = traj.CI.line, aes_string(x = time.column,
                                                             y = "value", linetype = "variable", colour = "state"))
        }
        else {
          p <- p + geom_ribbon(data = traj.CI.area, aes_string(x = time.column,
                                                               ymin = "low", ymax = "up", alpha = "CI"), fill = colour)
          p <- p + geom_line(data = traj.CI.line, aes_string(x = time.column,
                                                             y = "value", linetype = "variable"), colour = colour)
        }
        p <- p + scale_alpha_manual("Percentile", values = c(`95` = 0.25,
                                                             `50` = 0.45), labels = c(`95` = "95th", `50` = "50th"))
        p <- p + scale_linetype("Stats")
        p <- p + guides(linetype = guide_legend(order = 1))
      }
      else {
        p <- ggplot(df.traj)
        if (!same) {
          p <- p + facet_wrap(~state, scales = "free_y")
        }
        if (is.null(colour)) {
          if (same) {
            p <- p + geom_line(data = df.traj, aes_string(x = time.column,
                                                          y = "value", group = "state", color = "state"),
                               alpha = alpha)
          }
          else {
            p <- p + geom_line(data = df.traj, aes_string(x = time.column,
                                                          y = "value", group = replicate.column), alpha = alpha)
          }
        }
        else if (colour == "all") {
          p <- p + geom_line(data = df.traj, aes_string(x = time.column,
                                                        y = "value", group = replicate.column, color = replicate.column),
                             alpha = alpha)
        }
        else {
          p <- p + geom_line(data = df.traj, aes_string(x = time.column,
                                                        y = "value", group = replicate.column), alpha = alpha,
                             colour = colour)
        }
      }
      if (!is.null(non.extinct)) {
        p <- p + geom_line(data = df.p.ext, aes_string(x = time.column,
                                                       y = "value"), color = "black", alpha = 1)
      }
    }
    else {
      p <- ggplot()
    }
    if (!is.null(data)) {
      obs_names <- grep("obs", names(data), value = TRUE)
      if (length(obs_names) == 0) {
        obs_names <- setdiff(names(data), time.column)
      }
      data <- melt(data, measure.vars = obs_names, variable.name = "state")
      if (lines.data) {
        p <- p + geom_line(data = data, aes_string(x = time.column,
                                                   y = "value"), colour = "black")
      }
      else {
        p <- p + geom_point(data = data, aes_string(x = time.column,
                                                    y = "value"), colour = "black")
      }
    }
    p <- p  + set_theme() + theme(legend.position = "bottom", legend.box = "horizontal")
    if (plot) {
      print(p)
    }
    else {
      return(p)
    }
}

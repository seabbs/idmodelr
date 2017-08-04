#' Summarise and Plot Model Trajectories
#' @description This function uses faceting to plot all trajectories in a data frame. It allows the conveniant
#' manipulation of several simulations or data. It also allows complex model output to be aggregated.
#' Additionally if data is present, then an additional plot can be displayed
#' with data and potentially observations generated. This function is heaviliy
#' based on [plotTraj](https://github.com/sbfnk/fitR/blob/master/R/plot.r) from
#' the [fitR](https://github.com/sbfnk/fitR) package written by [Sebaustian Funk](http://sbfnk.github.io/).
#' @inheritParams summarise_model
#' @param lines.data Logical. If \code{TRUE}, the data will be plotted as lines.
#' @param alpha Transparency of the trajectories (between 0 and 1).
#' @param plot if \code{TRUE} the plot is displayed, and returned otherwise.
#' @param colour A character vector. If a character, will use that colour to plot trajectories. If "all", use all available colours.
#'  If \code{NULL}, don't set the colour.
#' @param same Logical. If \code{TRUE} does not facet plots.
#' @param set_theme Set the ggplot2 theme, defaults to theme_minimal.
#' @import reshape2 ggplot2 stringr
#' @importFrom tibble as_tibble
#' @return Optionally prints or stores a plot of trajectories
#' @export
#' @seealso summarise_model aggregate_model
#' @examples
#'
#'## Intialise
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
#'parameters <- c(beta = beta)
#'inits <- c(S = S_0, I = I_0)
#'
#'SI_sim <- simulate_model(model = SI_ode, sim_fn = solve_ode, inits, parameters, times)
#'
#'plot_model(SI_sim)
#'
plot_model <- function(traj = NULL, state.names = NULL, data = NULL, time.column = "time",
                             lines.data = FALSE, summary = TRUE, replicate.column = "replicate",
                             non.extinct = NULL, alpha = 1, plot = TRUE, colour = "firebrick2",
                             set_theme = theme_minimal, init.date = NULL, same = FALSE,
                             aggregate_to = NULL, compartments = NULL,
                             strat = NULL, hold_out_var = NULL, new_var = "incidence",
                             id_col = NULL, groups = NULL, total_pop = TRUE, summary_var = FALSE, verbose = FALSE)
{
  if (!is.null(traj) && "idmodelr" %in% class(traj)) {
    message("Trajectories/simulations have already been summarised by idmodelr - passing directly to plotting function")
    sum_model <- traj
  }else{
    sum_model <- summarise_model(traj, state.names = state.names, time.column = time.column,
                                summary = summary, replicate.column = replicate.column,
                                non.extinct = non.extinct, init.date = init.date, aggregate_to = aggregate_to,
                                compartments = compartments, strat = strat, hold_out_var = hold_out_var,
                                id_col = id_col, groups = groups, new_var = new_var, total_pop = total_pop,
                                summary_var = summary_var, verbose = verbose)
  }

  if (!is.null(aggregate_to)) {
    if (aggregate_to %in% "tidy") {
      if (same) {
        if (verbose) {
          message("Cannot plot all states and stratifications on the same graph, seeting same to be FALSE")
        }
        same <- FALSE
      }
  }

    if (is.null(id_col)) {
      id_col <- "id"
    }
  }

  if (!is.null(id_col) && !is.null(colour)) {
    if (verbose) {
      message("Cannot customise colour when tidy aggregating")
    }
    colour <- NULL
  }

  if (!is.null(traj) & is.null(sum_model[["sum_traj"]])) {
    if (summary) {
      summary <- FALSE
    }
  }
  if (colour == "all" && summary == TRUE) {
    warning("Ignoring ", sQuote("colour = \"all\""), " which doesn't make sense if ",
            sQuote("summary == TRUE"))
    colour <- NULL
  }
  if (!is.null(traj)) {
    if (summary) {
      melt_states <- c(time.column, "state")
      if (!is.null(id_col)) {
        melt_states <- c(melt_states, id_col)
      }
        traj.CI.line <- melt(sum_model[["sum_traj"]][c(melt_states,
                                       "mean", "median")], id.vars = melt_states)
        traj.CI.area <- melt(sum_model[["sum_traj"]][c(melt_states,
                                       "low_95", "low_50", "up_50", "up_95")], id.vars = melt_states)

        traj.CI.area$type <- sapply(traj.CI.area$variable,
                                    function(x) {
                                      str_split(x, "_")[[1]][1]
                                    })
        traj.CI.area$CI <- sapply(traj.CI.area$variable,
                                  function(x) {
                                    str_split(x, "_")[[1]][2]
                                  })
        traj.CI.area$variable <- NULL
        caststates <- paste(c(melt_states, "CI~type"), sep = "+", collapse = "+")
        traj.CI.area <- dcast(traj.CI.area, caststates)

        p <- ggplot(traj.CI.area)
        if (!same) {
          p <- p + facet_wrap(~state, scales = "free_y")
        }
        if (is.null(colour)) {
          if (is.null(id_col)) {
            p <- p + geom_ribbon(data = traj.CI.area, aes_string(x = time.column,
                                                                 ymin = "low", ymax = "up", alpha = "CI", fill = id_col,
                                                                 group = id_col, colour = id_col))
            p <- p + geom_line(data = traj.CI.line, aes_string(x = time.column,
                                                               y = "value", linetype = "variable", fill = id_col,
                                                               group = id_col, colour = id_col))
          }else {
            p <- p + geom_ribbon(data = traj.CI.area, aes_string(x = time.column,
                                                                 ymin = "low", ymax = "up", alpha = "CI"))
            p <- p + geom_line(data = traj.CI.line, aes_string(x = time.column,
                                                               y = "value", linetype = "variable"))
          }

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
        p <- ggplot(sum_model[["traj"]])
        if (!same) {
          p <- p + facet_wrap(~state, scales = "free_y")
        }
        if (is.null(colour)) {
          if (same) {
            p <- p + geom_line(data = sum_model[["traj"]], aes_string(x = time.column,
                                                          y = "value", group = "state", color = "state"),
                               alpha = alpha)
          }
          else {
            if (is.null(id_col)) {
              p <- p + geom_line(data = sum_model[["traj"]], aes_string(x = time.column, y = "value",
                                                                        group = id_col, colour = id_col),
                                 alpha = alpha)
            }else {
              p <- p + geom_line(data = sum_model[["traj"]], aes_string(x = time.column, y = "value",
                                                                        group = replicate.column),
                                 alpha = alpha)
            }

          }
        }
        else if (colour == "all") {
          p <- p + geom_line(data = sum_model[["traj"]], aes_string(x = time.column,
                                                        y = "value", group = replicate.column, color = replicate.column),
                             alpha = alpha)
        }
        else {
          p <- p + geom_line(data = sum_model[["traj"]], aes_string(x = time.column,
                                                        y = "value", group = replicate.column), alpha = alpha,
                             colour = colour)
        }
      }
      if (!is.null(non.extinct)) {
        p <- p + geom_line(data = sum_model[["prob_ext"]], aes_string(x = time.column,
                                                       y = "value"), color = "black", alpha = 1)
      }
    }
    else {
      p <- ggplot()
    }
    if (!is.null(data)) {
      obs_names <- grep("obs", names(sum_model[["obs"]]), value = TRUE)
      if (length(obs_names) == 0) {
        obs_names <- setdiff(names(sum_model[["obs"]]), time.column)
      }
      if (is.null(id_col)) {
        data <- melt(sum_model[["obs"]], measure.vars = obs_names, variable.name = "state")

        if (lines.data) {
          p <- p + geom_line(data = sum_model[["obs"]], aes_string(x = time.column,
                                                                   y = "value"), colour = "black")
        }
        else {
          p <- p + geom_point(data = sum_model[["obs"]], aes_string(x = time.column,
                                                                    y = "value"), colour = "black")
        }
      }else{
        data <- melt(sum_model[["obs"]], measure.vars = obs_names, variable.name = c("state", id_col))

        if (lines.data) {
          p <- p + geom_line(data = sum_model[["obs"]], aes_string(x = time.column,
                                                                   y = "value", group = id_col, colour = id_col))
        }
        else {
          p <- p + geom_point(data = sum_model[["obs"]], aes_string(x = time.column,
                                                                    y = "value", group = ic_col, colour = id_col))
        }
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

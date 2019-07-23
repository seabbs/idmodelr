#' Plot Compartment Populations over Time for a Model Simulation
#'
#' @description Make seperate plots for each model compartment. Assumes model output is structured
#' as that produced from \code{\link[idmodelr]{solve_ode}}.
#' @param sim A tibble of model output as formated by \code{\link[idmodelr]{solve_ode}}. Optionally a list of simulations
#' can be passed when comparing multiple model runs.
#' @param facet Logical, defaults to \code{TRUE}. If \code{FALSE} then the plot will not be facetted
#' otherwise it will be.
#' @param prev_sim A second tibble of model output formated as for \code{sim}. Used to compare to model runs. Can only be
#' supplied if \code{sim} is not a list.
#' @param model_labels A character vector of model names. Defaults to \code{c("Current", "Previous")} when two model simulations are used
#' and the list names when \code{sim} is a list. If \code{sim} is unnamed the index of the list is used.
#' @return A Plot of each model compartments population over time.
#' @importFrom ggplot2 ggplot aes aes_string guides facet_wrap geom_line theme_minimal theme labs
#' @importFrom viridis scale_color_viridis
#' @importFrom tidyr gather
#' @importFrom dplyr mutate bind_rows
#' @importFrom purrr map_dfr
#' @export
#'
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
#'parameters <- as.matrix(c(beta = beta))
#'inits <- as.matrix(c(S = S_0, I = I_0))
#'
#'sim <- solve_ode(model = SI_ode, inits, parameters, times, as.data.frame = TRUE)
#'
#'plot_model(sim, facet = FALSE)
#'
#'plot_model(sim, facet = TRUE)
#'
#'## Compare with an updated model run
#'
#'#'## Intialise
#'R_0 = 1.3
#'beta = R_0
#'parameters <- as.matrix(c(beta = beta))
#'
#'new_sim <- solve_ode(model = SI_ode, inits, parameters, times, as.data.frame = TRUE)
#'
#'
#'plot_model(new_sim,sim, facet = FALSE)
#'
#'plot_model(new_sim, sim, facet = TRUE)
#'
#'## Passing in the simulations as a list
#'plot_model(list("Current" = new_sim, "Previous" = sim), facet = TRUE)

plot_model <- function(sim, prev_sim = NULL, model_labels = NULL,
                       facet = TRUE) {

  ## redefine is.list to not catch dataframes
  is.list <- function(l) {
    log <- any(class(l) %in% "list")

    return(log)
  }

  if (!is.null(prev_sim) && is.list(sim)) {
    stop("Both a previous simulation and a list of models have been passed.
         Only one of these options may be used at once.")
  }

  time <- NULL; Compartment <- NULL; Model <- NULL; Population <- NULL;
  ## Define default lables for multiple models
  if (is.null(model_labels)) {

    if (!is.null(prev_sim)) {
      model_labels <- c("Current", "Previous")
    }else if (is.list(sim)) {
      if (is.null(names(sim))) {
        model_labels <- as.character(1:length(sim))
      }else{
        model_labels <- names(sim)
      }
    }
  }

  if (is.list(sim)) {
    names(sim) <- model_labels
  }

  gather_columns_for_plot <- function(sim){
    order <- colnames(sim)[-1]

    tidy_sim <- sim %>%
      gather(key = "Compartment", value = "Population", -time) %>%
      mutate(Compartment = factor(Compartment, levels = order))

    return(tidy_sim)
  }

  if (is.list(sim)) {
    tidy_sim <- map_dfr(sim, gather_columns_for_plot, .id = "Model")
  }else{
    tidy_sim <- gather_columns_for_plot(sim)
  }


  ## Add in previous model simulation if present
  if (!is.null(prev_sim)) {
    if ("data.frame" %in% class(prev_sim)) {
      prev_sim <-  gather_columns_for_plot(prev_sim)

      tidy_sim <- tidy_sim %>%
        mutate(Model = model_labels[1]) %>%
        bind_rows(prev_sim %>%
                    mutate(Model = model_labels[2])) %>%
        mutate(Model = factor(Model, levels = model_labels))
    }else{
      stop("prev_sim must be a model simulation dataframe or not be specified.")
    }
  }

  if (!is.null(prev_sim) | is.list(sim)) {
    plot <- ggplot(tidy_sim, aes_string(x = "time", y = "Population", col = "Compartment", linetype = "Model"))
  }else{
    plot <- ggplot(tidy_sim, aes_string(x = "time", y = "Population", col = "Compartment"))
  }

  plot <- plot +
    geom_line(size = 1.1) +
    theme_minimal() +
    labs(x = "Year") +
    scale_color_viridis(discrete = TRUE, end = 0.9) +
    theme(legend.position = "top")

  if (facet) {
    plot <- plot +
      facet_wrap(~Compartment) +
      guides(col = FALSE)
  }

  ## Add facetting for previous simulation
  if (is.null(prev_sim)) {
    if (!is.list(sim)) {
      plot <- plot +
        guides(linetype = FALSE)
    }
  }

return(plot)
}

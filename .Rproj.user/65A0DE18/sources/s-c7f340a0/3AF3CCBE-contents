
#' Compare a single vector of output from two models
#'
#' @param model1_vector A numeric vector of model output.
#' @param model2_vector A numeric vector of model output.
#' @param theme Supply a ggplot theme function to customise appearance.
#' @param plot Logical, if TRUE returns a plot,
#' if FALSE returns the intermediate dataframe.
#' @param plot_titles A length 2 character vector containing the plot titles, defaults to
#' a.), and b.).
#' @param plots_as_list Logical, if TRUE plots are returned as a list object, if FALSE (the default) plots are
#' combined using grid.arrange.
#' @param model_1 A character string of the name to give the first model.
#' @param model_2 A character string of the name to give the second model.
#' @return A comparision plot between two model vectors
#' @export
#' @importFrom tibble data_frame
#' @import ggplot2
#' @importFrom gridExtra grid.arrange
#' @examples
#'
#' plot_model_comparision_internal(c(1,2,3), c(1,2.1,3.2))
#'
plot_model_comparision_internal <- function(model1_vector,
                                    model2_vector,
                                    theme = theme_minimal,
                                    plot = TRUE,
                                    plot_titles = c("a.)", "b.)"),
                                    plots_as_list = FALSE,
                                    model_1 = NULL,
                                    model_2 = NULL) {

  if (is.null(model_1)) {
    model_1 <- "Model 1"
  }

  if (is.null(model_2)) {
    model_2 <- "Model 2"
  }

  ## Compare trajectory of incidence with non vaccine model
  model_com <- data_frame(!!as.name(model_1) := model1_vector,
                          !!as.name(model_2) := model2_vector) %>%
    mutate(var1 = !!as.name(model_1), var2 = !!as.name(model_2)) %>%
    mutate(`Percentage Difference (%)` = (var1 - var2) / var1 * 100,
           Time = seq(1, n())) %>%
      select(-var1, -var2)

  if (plot) {
    model_1 <- paste0("`", model_1, "`")
    model_2 <- paste0("`", model_2, "`")

    plot1 <- model_com %>%
      ggplot(aes_string(x = model_2, y = model_1)) +
      geom_point(alpha = 0.6) +
      coord_fixed() +
      geom_abline(intercept = 0, slope = 1) +
      labs(title = plot_titles[1]) +
      theme()

    plot2 <- model_com %>%
      ggplot(aes(x = Time, y = `Percentage Difference (%)`)) +
      geom_point(alpha = 0.6) +
      labs(title = plot_titles[2]) +
      theme()

    if (!plots_as_list)
    {
      plot <- grid.arrange(plot1, plot2, nrow = 1)

      plot
    }else{
      return(list(plot1, plot2))
    }
  }else{
    return(model_com)
  }
}


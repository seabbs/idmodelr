
#' Compare Deterministic Model Trajectories and Stochastic Mean Field Dynamics
#'
#' @inherit plot_model_comparision_internal
#' @inherit model_df_to_vector
#' @param df1 A dataframe containing the vector to be compared
#'  for a model
#' @param df2 A dataframe containing the vector to be compared for a second model
#' @param sum_fn The summary function to use to summarise the stochastic simulation, defaults to the median.
#'
#' @importFrom dplyr enquo bind_rows
#' @importFrom tibble as_tibble
#' @export
#' @seealso  plot_model_comparision model_df_to_vector
#' @examples
#' library(tidyverse)
#' ## Example for unique model output
#' plot_model_comparision(iris, iris, Petal.Length)
#'
#' ## Example for simulated model output
#' df <- bind_rows(iris %>% mutate(sim = 1, id = 1:length(sim)),
#'  iris %>% mutate(sim = 2 , id = 1:length(sim)))
#'
#'  plot_model_comparision(df, iris, com_var = Petal.Length, id_var = "id")
plot_model_comparision <- function(df1, df2, com_var, id_var = NULL,
                                   sum_fn = median, theme = theme_minimal,
                                   plot = TRUE, plot_titles = c("a.)", "b.)"),
                                   plots_as_list = FALSE,
                                   model_1 = NULL,
                                   model_2 = NULL) {
  com_var <- enquo(com_var)

  df1 <- as_tibble(df1)
  df2 <- as_tibble(df2)

  vect1 <- model_df_to_vector(df1, com_var = !!com_var,
                              id_var = id_var, sum_fn = sum_fn)
  vect2 <- model_df_to_vector(df2, com_var = !!com_var,
                              id_var = id_var, sum_fn = sum_fn)

  p <- plot_model_comparision_internal(vect1, vect2, theme = theme, plot = plot,
                                 plot_titles = plot_titles,
                                 plots_as_list = plots_as_list,
                                 model_1 = model_1,
                                 model_2 = model_2)
 return(p)
}

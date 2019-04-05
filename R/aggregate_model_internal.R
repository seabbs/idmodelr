
#' An Internal Function to Aggregate Model Output Using other Idmodelr functions.
#' @inheritParams gather_strat_multi_variable
#' @param df A dataframe of Model Output.
#' @param aggregate_to A character vector specifying the aggregation function to apply possible values are;
#' disease, demographic, or incidence.
#' @param compartments A character vector specifying the unique compartments to aggregate.
#' @param strat The number of stratified groups in the model.
#' @param hold_out_var A character  vector specifying the unique compartments not to aggregate.
#' @param new_var A character vector specifying the new variable to add when aggregating incidence.
#' @param total_pop A logical (defaults to \code{TRUE}) indicating if the total population should be
#' calculated when summarsing the model demographics.
#' @param summary_var A logical (defaults to \code{FALSE}), specifiying whether to add an additional summary variable across
#' all stratified levels.
#' @return An aggregated dataframe.
#' @export
#' @seealso aggregate_model aggregate_model_internal combine_to_age_model combine_strat_model_output summarise_var_by_strat
#' @examples
#'
#' df <- data.frame(A1 = 1, B1 = 1, A2 = 1, B2 = 1, A3 = 1, B3 = 1)
#' aggregate_model_internal(df, aggregate_to = "incidence",
#'                          compartments = c("A", "B"), strat = 3,
#'                          summary_var = TRUE)
#'
aggregate_model_internal <- function(df, aggregate_to = NULL, compartments = NULL,
                                     strat = NULL, hold_out_var= NULL, new_var = "incidence",
                                     id_col = NULL, groups = NULL, total_pop = TRUE,
                                     summary_var = FALSE) {

  if (aggregate_to %in% "demographic") {
    df <- combine_to_age_model(df, age_com = strat, compartments = compartments,
                         hold_out_var = hold_out_var, total_pop = total_pop)

  }

  if (aggregate_to %in% "disease") {
    df <- combine_strat_model_output(df, strat = strat, compartments = compartments,
                               hold_out_var = hold_out_var)
  }

  if (aggregate_to %in% "incidence") {
    df <- summarise_var_by_strat(df, vars = compartments, strat = strat, new_var = new_var, summary_var = summary_var)
  }

  if (aggregate_to %in% "tidy") {
    df <- gather_strat_multi_variable(df, id_col = id_col , compartments = compartments, hold_out_var = hold_out_var,
                                                  strat = strat, groups = groups)
  }

  return(df)
}

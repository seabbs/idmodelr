
#' An Internal Function to Aggregate Model Output Using other Idmodelr functions.
#'
#' @param df A dataframe of Model Output.
#' @param aggregate_to A character vector specifying the aggregation function to apply possible values are;
#' disease, demographic, or incidence.
#' @param compartments A character vector specifying the unique compartments to aggregate.
#' @param strat The number of stratified groups in the model.
#' @param hold_out_var A character  vector specifying the unique compartments not to aggregate.
#' @param new_var A character vevtor specifying the new variable to add when aggregating incidence.
#' @param total_pop A logical vector (defaults to \code{TRUE}) indicating if the total population should be
#' calculated when summarsing the model demographics.
#'
#' @return An aggregated dataframe.
#' @export
#' @seealso aggregate_model aggregate_model_internal combine_to_age_model combine_strat_model_output summarise_var_by_strat
#' @examples
#'
aggregate_model_internal <- function(df, aggregate_to = NULL, compartments = NULL,
                                     strat = NULL, hold_out_var= NULL, new_var = "incidence", total_pop = TRUE) {

  if (aggregate_to %in% "demographic") {
    df <- combine_to_age_model(df, age_com = strat, compartments = compartments,
                         hold_out_var = hold_out_var, total_pop = total_pop)

  }

  if (aggregate_to %in% "disease") {
    df <- combine_strat_model_output(df, strat = strat, compartments = compartments,
                               hold_out_var = hold_out_var)
  }

  if (aggregate_to %in% "incidence") {
    df <- summarise_var_by_strat(df, vars = compartments, strat = strat, new_var = new_var)
  }

  return(df)
}

#' Sum a Stratified Variable
#'
#' @param df A dataframe of model output.
#' @param vars A character vector containing the unstratified variables to summarise
#' @param strat The number of stratifications present in the data set
#' @param new_var The name of the summarised variable
#' @return Returns the original dataframe with an additional summarised variable
#' @export
#' @import magrittr
#' @importFrom dplyr select bind_cols
#' @importFrom tibble enframe
#' @seealso summarise_var_by_strat
#' @examples
#' df <- dplyr::mutate(iris, Petal.Length1 = Petal.Length, Petal.Length2 = Petal.Length)
#' df <- tibble::as_tibble(df)
#'
#' summarise_strat_var(df, vars = c("Petal.Length"), strat = 2, new_var = "sum")
summarise_strat_var <- function(df, vars, strat = NULL, new_var = "sum") {

  if (!is.null(strat)) {
    var_strat <-  add_pointer_struct(vars, length = strat)
  }else {
    var_strat <- vars
  }

  new_var <- df %>%
    select(.vars = var_strat) %>%
    rowSums %>%
    enframe(name = NULL) %>%
    set_names(new_var)

  df <- bind_cols(new_var, df)

  return(df)
}

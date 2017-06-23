#' Sum a Stratified Variable
#'
#' @param df A dataframe of model output.
#' @param vars A character vector containing the unstratified variables to summarise
#' @param strat The number of stratifications present in the data set
#' @param new_var The name of the summarised variable
#'
#' @return Returns the original dataframe with an additional summarised variable
#' @export
#' @import magrittr
#' @importFrom dplyr select bind_cols
#' @importFrom tibble as_tibble
#' @seealso summarise_var_by_strat
#' @examples
#' library(dplyr)
#' df <- mutate(iris, Petal.Length1 = Petal.Length, Petal.Length2 = Petal.Length)
#'
#' summarise_strat_var(df, vars = c("Petal.Length"), strat = 2, new_var = "sum")
summarise_strat_var <- function(df, vars, strat, new_var = "sum") {
  var_strat <-  add_pointer_struct(vars, length = strat)

 new_var <- df %>%
    select(.vars = var_strat) %>%
    rowSums %>%
    as_tibble %>%
    set_names(new_var)

 df <- bind_cols(df, new_var)

 return(df)
}

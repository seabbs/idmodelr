#' Sum a Stratified Variable by Stratification Level
#'
#' @details Takes compartmental infectious disease output and adds summary statisitics for
#' each stratified population, adding a final summary statistic for the whole population.
#' @inherit summarise_strat_var
#' @import magrittr
#' @importFrom purrr map
#' @importFrom dplyr select bind_cols
#' @importFrom tibble as_tibble
#' @return An updated data frame containing the summarised variable for each stratified level
#' and for the whole population.
#' @export
#'
#' @examples
#'
#' df <- data.frame(A1 = 1, B1 = 1, A2 = 1, B2 = 1, A3 = 1, B3 = 1)
#' summarise_var_by_strat(df, vars = c("A", "B"), strat = 3, new_var = "C")
#'
summarise_var_by_strat <- function(df, vars, strat, new_var) {

  ## summary strat for each  level
  strat_sum <- 1:strat  %>% map(function(y, var_strat, df, new_var) {
    var_strat <- paste0(var_strat, y)

    new_var <- df %>%
      select(.vars = var_strat) %>%
      rowSums %>%
      as_tibble %>%
      set_names(paste0(new_var, y))

    return(new_var)
  }, vars, df, new_var) %>%
    bind_cols

  ## summary strat across all levels
  df <- summarise_strat_var(df = df, vars = vars,
                            strat = strat, new_var = new_var)

  df <- bind_cols(df, strat_sum)

  return(df)
}

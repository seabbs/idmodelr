#' Sum a Stratified Variable by Stratification Level
#'
#' @details Takes compartmental infectious disease output and adds summary statistics for
#' each stratified population, optionally adding a final summary statistic for the whole population.
#' @param summary_var A logical (defaults to \code{FALSE}), specifying whether to add an additional summary variable across
#' all stratified levels when aggregating incidence.
#' @inherit summarise_strat_var
#' @import magrittr
#' @importFrom purrr map
#' @importFrom dplyr select bind_cols
#' @importFrom tibble enframe
#' @return An updated data frame containing the summarised variable for each stratified level
#' and for the whole population.
#' @export
#'
#' @examples
#' df <- data.frame(A = 1, B = 2)
#' summarise_var_by_strat(df, vars = c("A", "B"), new_var = "C")
#'
#' df <- data.frame(A1 = 1, B1 = 1, A2 = 1, B2 = 1, A3 = 1, B3 = 1)
#' summarise_var_by_strat(df, vars = c("A", "B"), strat = 3, new_var = "C")
#' summarise_var_by_strat(df, vars = c("A", "B"), strat = 3, new_var = "C", summary_var = TRUE)
summarise_var_by_strat <- function(df, vars, strat = NULL, new_var, summary_var = FALSE) {

  ## summary strat across all levels
  if (is.null(strat) | summary_var) {
    df <- summarise_strat_var(df = df, vars = vars,
                              strat = strat, new_var = new_var)
  }


  if (!is.null(strat)) {
    ## summary strat for each  level
    strat_sum <- 1:strat  %>% map(function(y, var_strat, df, new_var) {
      var_strat <- paste0(var_strat, y)

      new_var <- df %>%
        select(.vars = var_strat) %>%
        rowSums %>%
        enframe(name = NULL) %>%
        set_names(paste0(new_var, y))

      return(new_var)
    }, vars, df, new_var) %>%
      bind_cols

    if (summary_var) {
      df <- bind_cols(df[new_var], strat_sum, df[!(colnames(df) %in% new_var)])
    }else {
      df <- bind_cols(strat_sum, df)
    }


  }

  return(df)
}

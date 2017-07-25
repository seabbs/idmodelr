#' A Function to Gather Multiple Stratified Variables into a Tidy Format
#'
#' @inheritParams gather_strat_variable
#' @inheritParams combine_strat_model_output
#' @return A dataframe of stratified model output with multiple Tidy variables.
#' @export
#' @importFrom purrr map
#' @importFrom dplyr full_join
#' @import magrittr
#' @examples
#'
#'
#' df <- tibble::tibble(time = 0, A1 = 1, A2 = 2, A3 = 3, B1 = 2, B2 = 3, B3 = 0)
#' gather_strat_multi_variable(df, id_col = "Age", compartment = c("A", "B"), hold_out_var = "time",
#'                             strat = 3, groups = c("Children", "Young adults", "Adults"))
#'
gather_strat_multi_variable <- function(df, id_col, compartments = NULL, hold_out_var = NULL,
                                        strat = NULL, groups = NULL) {
  if (is.null(hold_out_var)) {
    stop("hold_out_var must be specified")
  }

  df <- map(compartments, ~ gather_strat_variable(df = df, id_col = id_col,
                                                  compartment = ., hold_out_var = hold_out_var,
                                                  strat = strat, groups = groups))
  if (length(compartments) == 1) {
    df_com <- df[[1]]
  }else{
    for (i in 1:length(compartments)) {
      if (i == 1) {
        df_com <- df[[1]]
      }else {
        df_com <- df_com %>%
          full_join(df[[i]], by = c(hold_out_var, id_col))
      }
    }

  }

  return(df_com)
}

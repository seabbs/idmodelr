#' A Function to Gather a Stratified Variable into a Tidy Format
#' @inheritParams combine_strat_model_output
#' @param id_col A character string containing the name of the new id column.
#' @param compartment The compartment to dedimensionalise.
#' @param groups A character vector with length equal to the level of stratification. Used to name the stratified levels.
#' @return A dataframe of stratified model output with a single Tidy variable.
#' @export
#' @import magrittr
#' @importFrom tidyr gather
#' @importFrom purrr map_df
#' @importFrom rlang syms
#' @examples
#'
#' df <- tibble::tibble(time = 0, A1 = 1, A2 = 2, A3 = 3)
#' gather_strat_variable(df, id_col = "Age", compartment = "A",
#'                       strat = 3, groups = c("Children", "Young adults", "Adults"))
gather_strat_variable <- function(df,  id_col, compartment, hold_out_var = NULL, strat, groups = NULL) {
  compartments_long <- compartment %>%
    add_pointer_struct(length = strat)

  if (!is.null(hold_out_var)) {
    df <- df[, colnames(df) %in% c(hold_out_var, compartments_long)]
  }

  df <- df %>%
    gather(key = id_col, value = compartment , !!!syms(compartments_long))

  if (!is.null(groups)) {
    df <- map_df(1:strat, function(i, df_internal) {
      df_temp <- df_internal[df_internal[[id_col]] %in% compartments_long[i],]

      df_temp[[id_col]] <- groups[i]
      return(df_temp)
    }, df)
  }

  df[[id_col]] <- as.factor(df[[id_col]])

  return(df)
}

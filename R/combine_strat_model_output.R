#' Reduces the Dimensionality of a Stratifed Model
#'
#' @description Reduces the dimensions of stratified model output. Default behaviour is to remove stratification for all
#' variables. However, variables to dedimensionalise can be selected, as can variables to preserve with there structure intact.
#' @param df A data frame.
#' @param strat An integer specifying the number of stratifications to reduce.
#' @param compartments A character vector specifying the unique population compartments. If not set then defaults to all
#' all columns that are not in hold_out_var
#' @param hold_out_var A character vector specifying the variables to keep unchanged. Defaults to NULL
#' @importFrom purrr map
#' @importFrom dplyr select bind_cols
#' @importFrom tibble as_tibble
#' @import magrittr
#' @return A dataframe of model output that has its dimensionality reduced
#' @export
#' @seealso combine_to_age_model
#' @examples
#'
#' df <- data.frame(S1 = NA, S2 = NA, S3 = NA, time = NA)
#' combine_strat_model_output(df, 3, compartments = "S", hold_out_var = "time")
#'
combine_strat_model_output <- function(df, strat = NULL,
                               compartments = NULL,
                               hold_out_var = NULL
) {

  if (is.null(strat)) {
    stop("The level of stratification (strat) is required")
  }

  if (is.null(compartments)) {
    compartments_long <- colnames(df)[!colnames(df) %in% hold_out_var]
    compartments <- str_extract(compartments_long, "[aA-zZ]+") %>%
      unique
  }else{
    compartments_long <- compartments %>%
      add_pointer_struct(length = strat)
  }

  if (is.null(hold_out_var)) {
    hold_out_var <- colnames(df)[!colnames(df) %in% compartments_long]
  }



  tmp <- compartments %>%
    map(add_pointer_struct, strat) %>%
    map(~select(df, .dots = .)) %>%
    map(rowSums) %>%
    map(as_tibble) %>%
    bind_cols %>%
    set_names(compartments)

  tmp <- df %>%
    select(.dots = hold_out_var) %>%
    set_names(hold_out_var) %>%
    bind_cols(tmp)
  return(tmp)
}

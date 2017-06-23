#' Reduces the Dimensionality of a Stratifed Model
#'
#' @param df A data frame.
#' @param strat An integer specifying the number of stratifications to reduce.
#' @param compartments A character vector specifying the unique population compartments.
#' @param hold_out_var A character vector specifying the variables to keep unchanged.
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
combine_strat_model_output <- function(df, strat,
                               compartments = c("S", "S_v", "E", "E_v", "I", "I_v",
                                                "N_SI", "N_SI_v", "N_EI", "N_EI_v"),
                               hold_out_var = c("time", "traj")
) {
  tmp <- compartments %>%
    map(add_pointer_struct, strat) %>%
    map(~select(df, .dots = .)) %>%
    map(rowSums) %>%
    map(as_tibble) %>%
    bind_cols %>%
    set_names(compartments)

  tmp <- tmp %>%
    bind_cols(df %>%
                select(.dots = hold_out_var) %>%
                set_names(hold_out_var)
    )
  return(tmp)
}

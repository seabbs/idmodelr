
#' Combine an Infectious Disease Model To a Demographic Model
#'
#' @param df A dataframe of model output.
#' @param compartments A character vector of the disease model compartments to combine.
#' @param age_com The number of age compartments that have been modelled.
#' @param hold_out_var The variables that have not been stratified by age.
#' @param total_pop A logical indicating whether to calculate the total population. Defaults
#' to true.
#' @return A dataframe which summarises the demographic process of a model.
#' @export
#' @seealso combine_strat_model_output
#' @importFrom purrr map
#' @importFrom dplyr select bind_cols mutate
#' @importFrom tibble as_tibble
#' @import magrittr
#' @examples
#'
#'df <- data.frame(S1 = 1, S2 = 1, E1 = 1, E2 = 1, time = 1)
#'
#'combine_to_age_model(df, age_com = 2, compartments = c("S", "E"), hold_out_var = "time")
combine_to_age_model <- function(df, age_com = 3,
                                 compartments = c("S", "S_v", "E", "E_v", "I", "I_v"),
                                 hold_out_var = c("time", "traj"),
                                 total_pop = TRUE
) {

  tmp <- compartments %>%
    map(add_pointer_struct, age_com) %>%
    map(~select(df, .dots = .))

  ## Age group names
  age_group_names <- paste0("age_group_", 1:age_com)

  tmp2 <- 1:age_com %>% map(function(x) {
    tmp_ret <- tmp %>%
      map(x) %>%
      map(as_tibble) %>%
      bind_cols %>%
      rowSums %>%
      as_tibble
    return(tmp_ret)
  }) %>%
    bind_cols %>%
    set_names(age_group_names)

  tmp3 <- tmp2 %>%
    bind_cols(df %>%
                select(.dots = hold_out_var) %>%
                set_names(hold_out_var)
    )
  if (total_pop) {
    tmp3 <-  tmp3 %>% mutate(N = tmp2 %>%
                      select(.dots = age_group_names) %>%
                      rowSums)
  }

  return(tmp3)
}

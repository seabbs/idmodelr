#' Combine an Infectious Disease Model To a Demographic Model
#' @description  Similarly to \code{\link[idmodelr]{combine_strat_model_output}} this functions
#' dedimensionalises model output into just the demographic components.
#' @inherit combine_strat_model_output
#' @param df A dataframe of model output.
#' @param age_com Integer indicating the number of age compartments.
#' @param compartments A character vector of the disease model compartments to combine.
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
#'df <- data.frame(S1 = c(1,2), S2 = c(1, 3), E1 = c(4, 1), E2 = c(3, 4), time = c(1, 2))
#'
#'combine_to_age_model(df, age_com = 2, hold_out_var = "time")
combine_to_age_model <- function(df, age_com = NULL,
                                 compartments = NULL,
                                 hold_out_var = NULL,
                                 total_pop = TRUE
) {

  if (is.null(age_com)) {
    stop("The number of age compartments (age_com) is required")
  }

  if (is.null(compartments)) {
    compartments_long <- colnames(df)[!colnames(df) %in% hold_out_var]
    compartments <- str_extract(compartments_long, "[aA-zZ]+") %>%
      unique
  }else{
    compartments_long <- compartments %>%
      add_pointer_struct(length = age_com)
  }

  if (is.null(hold_out_var)) {
    hold_out_var <- colnames(df)[!colnames(df) %in% compartments_long]
  }


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

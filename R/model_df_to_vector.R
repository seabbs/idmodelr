#' Extracts a Single Column, Summarises if from Simulation
#'
#' @param df A data frame of dynamic system output.
#' @param com_var The vector to be compared; use unquoted name (NSE).
#' @param id_var A character string indicating the id variable to summarise over
#' if required.
#' @param sum_fn The summary function to be used, defaults to median.
#' @return Returns a numeric vector, summarised if required.
#' @export
#' @import magrittr
#' @importFrom dplyr enquo pull group_by summarise bind_rows
#' @importFrom stats median
#' @examples
#' library(dplyr)
#' ## Extract a vector with no repeats
#' model_df_to_vector(iris, Petal.Length)
#'
#' ## Extract a vector and summarise
#' df <- bind_rows(iris %>% mutate(sim = 1, id = 1:length(sim)),
#'  iris %>% mutate(sim = 2 , id = 1:length(sim)))
#'
#' model_df_to_vector(df, Petal.Length, "id", sum_fn = mean)
model_df_to_vector <- function(df, com_var, id_var = NULL,
                               sum_fn = NULL) {
  var <- NULL;
  if (is.null(sum_fn)) {
    sum_fn <- median
  }

  com_var <- enquo(com_var)

  if ((!is.null(id_var) &&
       any(colnames(df) %in% id_var))) {
    df_sum <- df %>%
      group_by(.dots = id_var) %>%
      dplyr::summarise(var = sum_fn(!!com_var)) %>%
      pull(var)
  }else{
    df_sum <- df %>%
      select(!!com_var) %>%
      unlist %>%
      unname
  }

  return(df_sum)
}

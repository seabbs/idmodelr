#'Adds Pointer Structure to R objects
#'
#' @param char A character vector.
#' @param length The length of the returned object.
#'
#' @return A character vector of the concatenated character string with sequential numbering
#' @export
#' @importFrom purrr map
#' @import magrittr
#' @examples
#'
#'## For a single variable
#'add_pointer_struct("S", 3)
#'
#'## For multiple variables
#'add_pointer_struct(c("S", "I"), length = 3)
add_pointer_struct <- function(char, length) {
  char <- char %>%
    map(~sprintf(paste0(., "%1d"), seq_len(length))) %>%
    unlist

  return(char)
}

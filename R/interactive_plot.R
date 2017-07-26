#' A Utility Function that Controls if a Plot is Returned/Printed and if it
#' is Interactive.
#'
#' @param plot A ggplot object
#' @param interactive A logicial indicating whether a interactive plot should be returned.
#' Defaults to \code{TRUE}
#' @param print A logical (defaults to \code{TRUE}) indicating if the plot should be printed.
#' @param return A logical (defaults to \code{FALSE}) indicating if the plot should be returned.
#' @return A ggplot or ggplotly object.
#' @export
#' @importFrom plotly ggplotly
#' @examples
#'
#'
interactive_plot <- function(plot,
                             interactive = TRUE,
                             print = TRUE,
                             return = FALSE) {
  if (interactive) {
    plot <- ggplotly(plot)
  }

  if (print) {
    print(plot)
  }

  if (return) {
    return(plot)
  }
}

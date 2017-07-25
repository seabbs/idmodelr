#' A Function to Save Results in Multiple Formats
#'
#' @param df The data to be saved in any R supported format.
#' @param message An optional character string specifying the message to print.
#' @param name A character string containing the name to save the data under.
#' @param path  A character string containing the file pathway to the folder into
#' which to save the data. If not specified data will be saved into the home directory.
#' @param format A character vector specifying the format/formats to save the data into, defaults to rds. Currently
#'   csv is also supported.
#' @param verbose A logical indicating whether progress messages shoud be printed.
#'
#' @return Returns nothing, saves data in the specified file formats
#' @export
#'
#' @examples
#'
#' ## save_data(cars, name = "cars")
save_data <- function(df, name = NULL, path = NULL,
                      format = "rds", message = NULL,
                      verbose = TRUE) {
  if (is.null(name)) {
    stop("name must be a non-empty character string")
  }

  if (is.null(path)) {
    path <- ""
  }else {
    if (!dir.exists(path)) {
      dir.create(path)
    }
  }

  if (path %in% "") {
    save_file_path <- name
  }else {
    save_file_path <- file.path(path, name)
  }

  if (verbose) {
    if (is.null(message)) {
      message <- paste0(name, " has been saved to: ")
    }
    message(message, save_file_path)
    message("Formated as", paste0(", ", format))
  }

  if ("rds" %in% format) {
    saveRDS(df, file = paste0(save_file_path, ".rds"))
  }

  if ("csv" %in% format) {
    write.csv(df, file = paste0(save_file_path, ".csv"))
  }
}

#' Clean a POLYMOD Like Mixing Matrix
#'
#' @description Reads in and cleans a POLYMOD like mixing matrix saved as a .csv file.
#' POLYMOD mixing matrices are available from the orginal [paper](https://doi.org/10.1371/journal.pmed.0050074)
#' @param file_path A character string containing the path to the POLYMOD file
#' @return A tidy data frame of POLYMOD like mixing data
#' @export
#' @importFrom readr read_csv
#' @import magrittr
#' @importFrom dplyr select rename
#' @examples
#'
clean_polymod <- function(file_path) {

  df <-  readr::read_csv(file_path, skip = 1)

  df <- df %>%
    select(-`age of contact`) %>%
    rename(age_group = X2)

  return(df)
}

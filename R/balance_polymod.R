#' Function to Balance a POLYMOD Like Matrix Using Population Data
#'
#' @param polymod A POLYMOD like mixing matrix as produced by\
#'  \code{\link[idmodelr]{clean_polymod}} as a dataframe.
#' @param population A matching dataframe of populations by age_group,
#'  with two variables: age_group and population.
#' @param scale A number to scale contacts by, defaults to null. With POLYMOD
#' style mixing matrices this gives contacts per day.
#'
#' @return A balanced POLYMOD style mixing matrix as a dataframe
#' @export
#' @import magrittr
#' @importFrom dplyr full_join mutate_at funs select mutate
#' @importFrom tibble as_tibble
#' @examples
#'
balance_polymod <- function(polymod, population, scale = NULL) {

  ## Join polymod with population data
  polymod_with_pop <- polymod %>%
    full_join(population, by = "age_group")

  ## Times by population size
  norm_polymod <- polymod_with_pop %>%
    mutate_at(.vars = colnames(polymod)[-1], .funs = funs(. * population))

  ## Make symmetric
  symmetric_polymod <- norm_polymod

  for (i in 1:nrow(norm_polymod)) {
    for (j in 1:(ncol(norm_polymod) - 2)) {
      symmetric_polymod[i,j + 1] <- (norm_polymod[[i,j + 1]] + norm_polymod[[j,i + 1]]) / 2
    }
  }

  ## Normalise by population
  ## Times by population size
  renorm_polymod <- symmetric_polymod %>%
    mutate_at(.vars = colnames(polymod)[-1], .funs = funs(. / population))

  ##Return a matrix
  balanced_polymod <- renorm_polymod %>%
    select(-age_group, -population) %>%
    as.matrix
  ## with names
  rownames(balanced_polymod) <- colnames(balanced_polymod)

  ##scale if required
  if (!is.null(scale)) {
    balanced_polymod <- balanced_polymod * scale
  }

  balanced_polymod <- as_tibble(balanced_polymod) %>%
    mutate(age_group = colnames(balanced_polymod)) %>%
    select(age_group, everything())

  return(balanced_polymod)
}


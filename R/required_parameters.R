#' Lookup the Details of Parameters Requiered by a Model
#'
#' @description This function simplifies the process of checking which parameters a given `idmodelr` model depends on.
#' It is effectively an interface to \code{\link[idmodelr]{parameter_details}} via \code{\link[idmodelr]{model_details}}. As
#' fuzzy matching has been used it can also given information of the parameter requirements of a subset of the available models.
#' @param model A character string containing the name of the model of interest. Defaults to \code{NULL}.
#'
#' @return A dataframe extracted from \code{\link[idmodelr]{parameter_details}} containing the details of the parameters
#' required by the model of interest.
#' @export
#'
#' @importFrom dplyr filter
#' @examples
#'
#' ##Check the parameters required by the "SIR_ode" model
#' required_parameters("SIR_ode")
#'
#'
#' ## Use fizzy matching to look at paramters for all SIR models
#' required_parameters("SIR")
required_parameters <- function(model = NULL) {

  parameters <- unlist(
    idmodelr::model_details[grepl(model, idmodelr::model_details$model), "parameters"]
    )

  parameter <- NULL

  lookup <- dplyr::filter(idmodelr::parameter_details, parameter %in% parameters)

  if (nrow(lookup) == 0)  {
    stop("No parameters found for this model")
  }

  return(lookup)
}

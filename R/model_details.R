
#' Model Details
#'
#'Details on models implemented in `idmodelr`.
#' @format A data frame with 22 rows and 14 variables.
#' \describe{
#'       \item{model}{Name of the model function.}
#'       \item{model_family}{Name of the model family (i.e. SIR)}
#'       \item{time}{Discrete or continuous time}
#'       \item{type}{Deterministic or stochastic}
#'       \item{recovered}{Does this model included a recovered population (yes/no).}
#'       \item{exposed}{Does this model included an exposed population (yes/no).}
#'       \item{treated}{Does this model included a treated population (yes/no).}
#'       \item{susceptible}{Does this model included a post infection susceptible population (yes/no).}
#'       \item{risk_stratified}{Is this model risk stratified (yes/no).}
#'       \item{non_exponential}{Does this model contain non-exponential rates (yes/no).}
#'       \item{simple_demographics}{Does this model contain simple (i.e. births = deaths) demographics (yes/no).}
#'       \item{vaccination}{Does this model include vaccination (yes/no).}
#'       \item{disease_example}{Which diseases (if any) can this model be used for.}
#'       \item{language}{What language is this model written in (i.e. R, C etc.).}
#'       \item{parameters}{A list of parameters required by the model}.
#' }
"model_details"

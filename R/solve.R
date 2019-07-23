#' A Simple Wrapper for lsoda
#'
#' @description This function acts as a simple wrapper for lsoda, allowing for multiple parameter sets and
#' initial conditions. It also allows \code{\link[deSolve]{lsoda}} to be used within the idmodelr framework.
#' @param model A model formatted as required by \code{\link[deSolve]{lsoda}}, see \code{\link[idmodelr]{SI_ode}} for an example.
#' @param inits The initial state (states) of the model. Can either be supplied as a named vector or as a matrix with each
#' row representing a parameter.
#' @param params A named vector or matrix of parameters. The matrix must have a row for each parameter and if \code{inits} is specified
#' as a matrix then \code{params} must have the same number of columns
#' @param times A numeric vector of the times for which explicit model estimates are required, this does not effect
#' the timestep used by the solver
#' @param as.data.frame A logical (defaults to \code{TRUE}) indicating if the results should be returned as a data frame.
#' @param ... Additional arguments to pass to \code{\link[deSolve]{lsoda}}.
#' @seealso \code{\link[deSolve]{lsoda}} \code{\link[idmodelr]{SI_ode}}
#' @return A dataframe or lsoda object containing a single or multiple model trajectories
#' @export
#' @importFrom purrr map
#' @importFrom tibble as_tibble
#' @importFrom dplyr bind_rows
#' @importFrom deSolve lsoda
#' @examples
#'## Intialise
#'N = 100000
#'I_0 = 1
#'S_0 = N - I_0
#'R_0 = 1.1
#'beta = R_0
#'
#' ##Time for model to run over
#'tbegin = 0
#'tend = 50
#'times <- seq(tbegin, tend, 1)
#'
#' ##Vectorise input
#'parameters <- as.matrix(c(beta = beta))
#'inits <- as.matrix(c(S = S_0, I = I_0))
#'
#'solve_ode(model = SI_ode, inits, parameters, times, as.data.frame = TRUE)


solve_ode <- function(model = NULL, inits = NULL, params = NULL, times = NULL, as.data.frame = TRUE, ...) {

  if ("matrix" %in% class(params)) {
    solved_ode <- map(1:ncol(params), function(i) {
      params_vect <- params[,i]
      if ("matrix" %in% class(inits)) {
        initial <- inits[, i]
      }else{
        initial <- inits
      }

      solved_ode <- deSolve::lsoda(initial, times, model, params_vect, ...)

      if (as.data.frame) {
        solved_ode <- as.data.frame(solved_ode)
        solved_ode <- as_tibble(solved_ode)
        if (ncol(params) != 1) {
          solved_ode$traj <- i
        }
      }
      return(solved_ode)
    })

    if (as.data.frame) {
      solved_ode <- do.call(bind_rows, solved_ode)
      }
  }else{
    solved_ode <- deSolve::lsoda(inits, times, model, params, ...)

    if (as.data.frame) {
      solved_ode <- as.data.frame(solved_ode)
      solved_ode <- as_tibble(solved_ode)
    }
  }

  return(solved_ode)
}

#' A Function to Simulate a Model from a Generic Simulation Function, with Pre and Post Processing
#' @inheritParams aggregate_model
#' @param model A model compatible with your \code{sim_fn}. A \code{\link[pomp]{pomp}} model object is
#' recommended.
#' @param sim_fn A generic simulation function, with the first arguement as the model object,
#' a \code{params} arguement, and a \code{as.data.frame} arguement. Tested to work with \code{trajectory}
#'   and \code{simulate} from the \code{\link[pomp]{pomp}} package.
#' @param params A dataframe of parameters, with each parameter as a variable. Optionally a named vector can be used.
#' @param as_tibble Logical (defaults to \code{TRUE}) indicating if the output
#'  should be returned as a tibble, otherwise returned as the default \code{sim_fn} output.
#' @param ... Additional arguments to pass to \code{sim_fn}
#' @seealso aggregate_model
#' @return Trajectories as a tibble, optionally returns the default \code{sim_fn} output.
#' @export
#' @importFrom tibble as_tibble
#'
#' @examples
#'
model_simulate <- function(model, sim_fn, params, as_tibble = TRUE,
                           aggregate_to = NULL, compartments = NULL,
                           strat = NULL, hold_out_var = NULL, new_var = "incidence",
                           total_pop = TRUE, summary_var, ...) {

  if ("data.frame" %in% class(params)) {
    params_as_matrix <- t(as.matrix(params))
  } else if ("numeric" %in% class(params)) {
    params_as_matrix <- params
  }else {
    stop("The parameters must be formated as a dataframe or named vector. Not as a",
         paste0(", ", class(params)))
  }

  if (!is.null(aggregate_to)) {
    as_tibble <- TRUE
  }

  sim <- sim_fn(model, params = params_as_matrix, as.data.frame = as_tibble, ...)


  if (as_tibble) {
    sim <- as_tibble(sim)
  }

  if (!is.null(aggregate_to)) {
    sim <- aggregate_model(sim, aggregate_to = aggregate_to, compartments = compartments,
                           strat = strat, hold_out_var = hold_out_var, new_var = new_var,
                           total_pop = total_pop)

  }

  return(sim)
}

#' A Function to Simulate a Model from a Generic Simulation Function, with Pre and Post Processing
#' @inheritParams aggregate_model
#' @param model A model compatible with your \code{sim_fn}. A \code{\link[pomp]{pomp}} model object is
#' recommended.
#' @param sim_fn A generic simulation function, with the first arguement as the model object,
#' a \code{params} arguement, and a \code{as.data.frame} arguement. Tested to work with \code{trajectory}
#'   and \code{simulate} from the \code{\link[pomp]{pomp}} package.
#' @param inits A dataframe of initial conditions, optionally a named vector can be used.
#' @param params A dataframe of parameters, with each parameter as a variable. Optionally a named vector can be used.
#' @param times A vector of the times to sample the model for, from a starting time to a final time.
#' @param as_tibble Logical (defaults to \code{TRUE}) indicating if the output
#'  should be returned as a tibble, otherwise returned as the default \code{sim_fn} output.
#' @param by_row Logical (defaults to \code{FALSE}) indicating if inputed parameters should be inputed as a block to \code{sim_fn}
#' or individually. If \code{TRUE} then function will always return a tibble. Does not currently work with sim_fn that produces
#' multiple simulations for a single parameter set - for this scenario a block based approach or post processing is required.
#' @param verbose Logical (defaults to \code{FALSE}) indicating if progress information should be printed to the console.
#' @param ... Additional arguments to pass to \code{sim_fn}
#' @seealso aggregate_model
#' @return Trajectories as a tibble, optionally returns the default \code{sim_fn} output.
#' @export
#' @importFrom tibble as_tibble
#' @importFrom purrr map_df
#' @examples
#'
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
#'parameters <- data.frame(beta = beta)
#'inits <- data.frame(S = S_0, I = I_0)
#'
#'SI_sim <- simulate_model(model = SI_ode, sim_fn = solve_ode, inits, parameters, times)
simulate_model <- function(model, sim_fn, inits = NULL, params = NULL, times = NULL,
                           as_tibble = TRUE, by_row = FALSE, aggregate_to = NULL, compartments = NULL,
                           strat = NULL, hold_out_var = NULL, new_var = "incidence",
                           total_pop = TRUE, summary_var = FALSE, verbose = FALSE, ...) {
  if ("data.frame" %in% class(params)) {
    params_as_matrix <- t(as.matrix(params))
  } else if ("numeric" %in% class(params)) {
    params_as_matrix <- params
  }else {
    stop("The parameters must be formated as a dataframe or named vector. Not as a",
         paste0(", ", class(params)))
  }

    if ("data.frame" %in% class(inits)) {
      inits_as_matrix <- t(as.matrix(inits))

      if (nrow(inits) != nrow(params) && "data.frame" %in% class(params)) {
        stop("There must be the same number of parameter sets as initial conditions")
      }

    } else if ("numeric" %in% class(inits) | is.null(inits)) {
      inits_as_matrix <- inits
    }else {
      stop("The initial conditions must be formated as a dataframe or named vector. Not as a",
           paste0(", ", class(inits)))
    }

    if (!is.null(aggregate_to)) {
      as_tibble <- TRUE
    }
  if (by_row) {
    as_tibble <- TRUE

    if (is.null(times)) {
      sim <- map_df(1:ncol(params_as_matrix), function(i) {
        traj <- sim_fn(model, inits = inits_as_matrix[, i], params = params_as_matrix[, i], as.data.frame = as_tibble, ...)
        traj$traj <- i

        if (verbose) {
          message("Parameter set: ", i)
        }
        return(traj)
      })
    }else {
      sim <- map_df(1:ncol(params_as_matrix), function(i) {
        traj <- sim_fn(model, inits = inits_as_matrix[,i], params = params_as_matrix[,i], as.data.frame = as_tibble, ...)
        traj$traj <- i

        if (verbose) {
          message("Parameter set: ", i)
        }
        return(traj)
      })
    }
  }else{
    if (is.null(times)) {
      sim <- sim_fn(model, inits = inits_as_matrix, params = params_as_matrix, as.data.frame = as_tibble, ...)
    }else {
      sim <- sim_fn(model, inits = inits_as_matrix, params = params_as_matrix, times = times, as.data.frame = as_tibble, ...)
    }
  }
  if (is.null(times)) {
    sim <- sim_fn(model, inits = inits_as_matrix, params = params_as_matrix, as.data.frame = as_tibble, ...)
  }else {
    sim <- sim_fn(model, inits = inits_as_matrix, params = params_as_matrix, times = times, as.data.frame = as_tibble, ...)
  }


  if (as_tibble && !"tbl_df" %in% class(sim)) {
    sim <- as_tibble(sim)
  }

  if (!is.null(aggregate_to)) {
    sim <- aggregate_model(sim, aggregate_to = aggregate_to, compartments = compartments,
                           strat = strat, hold_out_var = hold_out_var, new_var = new_var,
                           total_pop = total_pop, summary_var = summary_var)

  }

  return(sim)
}

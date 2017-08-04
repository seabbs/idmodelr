#' A Simple Susceptible-Infected Infectious Disease Model
#'
#' @param t The timestep overwhich to calculate derivatives
#' @param x A numeric vector of compartment populations.
#' @param params A named vector of paramter values.
#'
#' @return A vector of derivatives
#' @export
#'
#' @examples
#' ##Model Input
#' S_0 <- 999
#' I_0 <- 1
#' beta <- 3
#' dt <- 1
#' parameters <- c(beta = beta)
#' inits <- c(S = S_0, I = I_0)
#'
#' SI_ode(1, inits, parameters)


SI_ode <- function(t, x, params) {

  ## Specify model compartments
  S <- x[1]
  I <- x[2]

  with(as.list(params),{

    ## Specify total population
    N = S + I

    ## Derivative Expressions
    dS = -beta * S * I / N
    dI = +beta * S * I / N

    ## output
    derivatives <- c(dS, dI)

    list(derivatives)
  })
}


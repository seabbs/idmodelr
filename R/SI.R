#' Susceptible-Infected Model
#'
#' @param t The timestep over which to calculate derivatives
#' @param x A numeric vector of compartment populations.
#' @param params A named vector of parameter values.
#'
#' @return A vector of derivatives
#' @export
#'
#' @examples
#' ##Model Input
#' S_0 <- 999
#' I_0 <- 1
#' beta <- 3
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

#' Susceptible-Infected Model with Simple Demographics
#'
#' @inherit SI_ode
#' @return A vector of derivatives
#' @export
#'
#' @examples
#' ##Model Input
#' S_0 <- 999
#' I_0 <- 1
#' beta <- 3
#' mu <- 1/81
#'
#' parameters <- c(beta = beta, mu = mu)
#' inits <- c(S = S_0, I = I_0)
#'
#' SI_demographics_ode(1, inits, parameters)


SI_demographics_ode <- function(t, x, params) {

  ## Specify model compartments
  S <- x[1]
  I <- x[2]

  with(as.list(params),{

    ## Specify total population
    N = S + I

    ## Derivative Expressions
    dS = -beta * S * I / N - mu * S + mu * N
    dI = +beta * S * I / N - mu * I

    ## output
    derivatives <- c(dS, dI)

    list(derivatives)
  })
}


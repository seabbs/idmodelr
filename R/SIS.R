#' Susceptible-Infected-Susceptible Model
#'
#' @param t The timestep overwhich to calculate derivatives
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
#' chi <- 2
#' parameters <- c(beta = beta, chi = chi)
#' inits <- c(S = S_0, I = I_0)
#'
#' SIS_ode(1, inits, parameters)


SIS_ode <- function(t, x, params) {

  ## Specify model compartments
  S <- x[1]
  I <- x[2]

  with(as.list(params),{

    ## Specify total population
    N = S + I

    ## Derivative Expressions
    dS = -beta * S * I / N + chi * I
    dI = +beta * S * I / N - chi * I

    ## output
    derivatives <- c(dS, dI)

    list(derivatives)
  })
}

#' Susceptible-Infected-Susceptible Model with Simple Demographics
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
#' chi <- 2
#' mu <- 1/81
#' dt <- 1
#' parameters <- c(beta = beta, mu = mu)
#' inits <- c(S = S_0, I = I_0)
#'
#' SIS_demographics_ode(1, inits, parameters)


SIS_demographics_ode <- function(t, x, params) {

  ## Specify model compartments
  S <- x[1]
  I <- x[2]

  with(as.list(params),{

    ## Specify total population
    N = S + I

    ## Derivative Expressions
    dS = -beta * S * I / N + chi * I - mu * S + mu * N
    dI = +beta * S * I / N - chi * I - mu * I

    ## output
    derivatives <- c(dS, dI)

    list(derivatives)
  })
}


#' Susceptible-Exposed-Infected-Susceptible Model
#'
#' @inherit SI_ode
#' @export
#'
#' @examples
#' ##Model Input
#' S_0 <- 989
#' E_0 <- 10
#' I_0 <- 1
#' beta <- 3
#' chi <- 0.5
#' gamma = 1/2
#' parameters <- c(beta = beta, gamma = gamma, chi = chi)
#' inits <- c(S = S_0, E = E_0, I = I_0)
#'
#' SEIS_ode(1, inits, parameters)


SEIS_ode <- function(t, x, params) {

  ## Specify model compartments
  S <- x[1]
  E <- x[2]
  I <- x[3]

  with(as.list(params),{

    ## Specify total population
    N = S + E + I

    ## Derivative Expressions
    dS = -beta * S * I / N + chi * I
    dE = +beta * S * I / N - gamma * E
    dI = +gamma * E - chi * I

    ## output
    derivatives <- c(dS, dE, dI)

    list(derivatives)
  })
}


#' Susceptible-Exposed-Infected-Susceptible Model with Simple Demographics
#' @inherit SI_ode
#' @export
#'
#' @examples
#' ##Model Input
#' S_0 <- 989
#' E_0 <- 10
#' I_0 <- 1
#' beta <- 3
#' chi <- 0.5
#' gamma <- 1/2
#' mu <- 1/81
#' parameters <- c(beta = beta, gamma = gamma,
#'  chi = chi, mu = mu)
#' inits <- c(S = S_0, E = E_0, I = I_0)
#'
#' SEIS_demographics_ode(1, inits, parameters)

SEIS_demographics_ode <- function(t, x, params) {

  ## Specify model compartments
  S <- x[1]
  E <- x[2]
  I <- x[3]

  with(as.list(params),{

    ## Specify total population
    N = S + E + I

    ## Derivative Expressions
    dS = -beta * S * I / N + chi * I - mu * S + mu * N
    dE = +beta * S * I / N - gamma * E - mu * E
    dI = +gamma * E - chi * I - mu *I

    ## output
    derivatives <- c(dS, dE, dI)

    list(derivatives)
  })
}



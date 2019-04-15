#' Susceptible-Exposed-Infected-Recovered-Susceptible Model
#'
#' @inherit SI_ode
#' @export
#'
#' @examples
#' ##Model Input
#' S_0 <- 989
#' E_0 <- 10
#' I_0 <- 1
#' R_0 <- 0
#' beta <- 3
#' gamma <-  1/2
#' tau <- 2
#' chi <- 0.5
#' dt <- 1
#'
#' parameters <- c(beta = beta, gamma = gamma,
#'                 chi = chi, tau = tau)
#' inits <- c(S = S_0, E = E_0, I = I_0, R_0 = R_0)
#'
#' SEIRS_ode(1, inits, parameters)
SEIRS_ode <- function(t, x, params) {

  ## Specify model compartments
  S <- x[1]
  E <- x[2]
  I <- x[3]
  R <- x[4]

  with(as.list(params),{

    ## Specify total population
    N = S + E + I + R

    ## Derivative Expressions
    dS = - beta * S * I / N + chi * R
    dE = beta * S * I / N - gamma * E
    dI = gamma * E - tau * I
    dR = tau * I - chi * R

    ## output
    derivatives <- c(dS, dE, dI, dR)

    list(derivatives)
  })
}


#' Susceptible-Exposed-Infected-Recovered-Susceptible Model with Simple Demographics
#'
#' @inherit SEIRS_ode
#' @export
#'
#' @examples
#' ##Model Input
#' S_0 <- 989
#' E_0 <- 10
#' I_0 <- 1
#' R_0 <- 0
#' beta <- 3
#' gamma <-  1/2
#' tau <- 2
#' chi <- 0.5
#' mu <- 1/81
#' dt <- 1
#'
#' parameters <- c(beta = beta, gamma = gamma, chi = chi,
#'                 tau = tau, mu = mu)
#' inits <- c(S = S_0, E = E_0, I = I_0, R_0 = R_0)
#'
#' SEIRS_demographics_ode(1, inits, parameters)
SEIRS_demographics_ode <- function(t, x, params) {

  ## Specify model compartments
  S <- x[1]
  E <- x[2]
  I <- x[3]
  R <- x[4]

  with(as.list(params),{

    ## Specify total population
    N = S + E + I + R

    ## Derivative Expressions
    dS = - beta * S * I / N + chi * R - mu * S + mu * N
    dE = beta * S * I / N - gamma * E - mu * E
    dI = gamma * E - tau * I - mu * I
    dR = tau * I - chi * R - mu * R

    ## output
    derivatives <- c(dS, dE, dI, dR)

    list(derivatives)
  })
}

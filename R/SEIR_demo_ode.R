#' A Simple Susceptible-Exposed-Infected-Recovered Infectious Disease Model  with Simple Demographics
#'
#' @inherit SEIR_ode
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
#' mu <- 1/81
#' dt <- 1
#'
#' parameters <- c(beta = beta, gamma = gamma, tau = tau, mu = mu)
#' inits <- c(S = S_0, E = E_0, I = I_0, R_0 = R_0)
#'
#' SEIR_demo_ode(1, inits, parameters)
SEIR_demo_ode <- function(t, x, params) {

  ## Specify model compartments
  S <- x[1]
  E <- x[2]
  I <- x[3]
  R <- x[4]

  with(as.list(params),{

    ## Specify total population
    N = S + E + I + R

    ## Derivative Expressions
    dS = - beta * S * I / N - mu * S + mu * N
    dE = beta * S * I / N - gamma * E - mu * E
    dI = gamma * E - tau * I - mu * I
    dR = tau * I - mu * R

    ## output
    derivatives <- c(dS, dE, dI, dR)

    list(derivatives)
  })
}

#' A Simple Susceptible-Exposed-Infected-Recovered Infectious Disease Model
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
#' dt <- 1
#'
#' parameters <- c(beta = beta, gamma = gamma, tau = tau)
#' inits <- c(S = S_0, E = E_0, I = I_0, R_0 = R_0)
#'
#' SEIR_ode(1, inits, parameters)
SEIR_ode <- function(t, x, params) {

  ## Specify model compartments
  S <- x[1]
  E <- x[2]
  I <- x[3]
  R <- x[4]

  with(as.list(params),{

    ## Specify total population
    N = S + E + I + R

    ## Derivative Expressions
    dS = - beta * S * I / N
    dE = beta * S * I / N - gamma * E
    dI = gamma * E - tau * I
    dR = tau * I

    ## output
    derivatives <- c(dS, dE, dI, dR)

    list(derivatives)
  })
}

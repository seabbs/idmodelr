#' A Simple Susceptible-Infected-Recovered Infectious Disease Model  with Simple Demographics
#'
#' @inherit SIR_ode
#' @export
#'
#' @examples
#' ##Model Input
#' S_0 <- 989
#' I_0 <- 1
#' R_0 <- 0
#' beta <- 3
#' tau <- 2
#' mu <- 1/81
#' dt <- 1
#'
#' parameters <- c(beta = beta, tau = tau, mu = mu)
#' inits <- c(S = S_0, I = I_0, R_0 = R_0)
#'
#' SIR_demo_ode(1, inits, parameters)
SIR_demo_ode <- function(t, x, params) {

  ## Specify model compartments
  S <- x[1]
  I <- x[2]
  R <- x[3]

  with(as.list(params),{

    ## Specify total population
    N = S + I + R

    ## Derivative Expressions
    dS = - beta * S * I / N - mu * S + mu * N
    dI = beta * S * I / N - tau * I - mu * I
    dR = tau * I - mu * R

    ## output
    derivatives <- c(dS, dI, dR)

    list(derivatives)
  })
}

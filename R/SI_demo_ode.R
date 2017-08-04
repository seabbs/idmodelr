#' A Susceptible-Infected Infectious Disease Model with Simple Demographics
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
#' dt <- 1
#' parameters <- c(beta = beta, mu = mu)
#' inits <- c(S = S_0, I = I_0)
#'
#' SI_demo_ode(1, inits, parameters)


SI_demo_ode <- function(t, x, params) {

  ## Specify model compartments
  S <- x[1]
  I <- x[2]

  with(as.list(parameters),{

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


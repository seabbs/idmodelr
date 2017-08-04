#' A Susceptible-Exposed-Infected Infectious Disease Model with Simple Demographics
#' @inherit SI_ode
#' @export
#'
#' @examples
#' ##Model Input
#' S_0 <- 989
#' E_0 <- 10
#' I_0 <- 1
#' beta <- 3
#' gamma <- 1/2
#' mu <- 1/81
#' dt <- 1
#' parameters <- c(beta = beta, gamma = gamma, mu = mu)
#' inits <- c(S = S_0, E = E_0, I = I_0)
#'
#' SEI_demo_ode(1, inits, parameters)


SEI_demo_ode <- function(t, x, params) {

  ## Specify model compartments
  S <- x[1]
  E <- x[2]
  I <- x[3]

  with(as.list(params),{

    ## Specify total population
    N = S + E + I

    ## Derivative Expressions
    dS = -beta * S * I / N - mu * S + mu * N
    dE = +beta * S * I / N - gamma * E - mu * E
    dI = +gamma * E - mu *I

    ## output
    derivatives <- c(dS, dE, dI)

    list(derivatives)
  })
}


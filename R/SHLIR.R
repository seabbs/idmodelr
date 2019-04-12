#' Susceptible-High-risk-latent-Low-risk-latent-Infected-Recovered Model
#'
#' @inherit SEIR_ode
#' @export
#'
#' @examples
#' ##Model Input
#' S_0 <- 989
#' H_0 <- 10
#' L_0 <- 0
#' I_0 <- 1
#' R_0 <- 0
#' beta = 3 # Rate of transmission
#' gamma_H = 1/5 # Rate of progression to active symptoms from high risk latent
#' nu = 1/2 #Rate of progression from high to low risk latent
#' gamma_L = 1/100 # Rate of progression to active symptoms for low risk latent
#' tau = 1/2 # Rate of recovery
#' dt <- 1
#'
#' parameters <- c(beta = beta, gamma_H = gamma_H, gamma_L = gamma_L, nu = nu, tau = tau)
#' inits <- c(S = S_0, H = H_0, L = L_0, I = I_0, R_0 = R_0)
#'
#' SHLIR_ode(1, inits, parameters)
SHLIR_ode <- function(t, x, params) {

  ## Specify model compartments
  S <- x[1]
  H <- x[2]
  L <- x[3]
  I <- x[4]
  R <- x[5]

  with(as.list(params),{

    ## Specify total population
    N = S + H + L + I + R

    ## Derivative Expressions
    dS = - beta * S * I / N
    ## These are the new equations - fill in the remaining terms
    dH = beta * (S + L) * I / N - gamma_H * H - nu * H
    dL = nu * H - beta * L * I / N - gamma_L * L
    ## Hint terms are missing from this equation as well
    dI = gamma_H * H + gamma_L * L - tau * I
    dR = tau * I

    ## output
    derivatives <- c(dS, dH, dL, dI, dR)

    list(derivatives)
  })
}


#' Susceptible-High-risk-latent-Low-risk-latent-Infected-Recovered Model with Simple Demographics
#'
#' @inherit SHLIR_ode
#' @export
#'
#' @examples
#' ##Model Input
#' S_0 <- 989
#' H_0 <- 10
#' L_0 <- 0
#' I_0 <- 1
#' R_0 <- 0
#' beta = 3 # Rate of transmission
#' gamma_H = 1/5 # Rate of progression to active symptoms from high risk latent
#' nu = 1/2 #Rate of progression from high to low risk latent
#' gamma_L = 1/100 # Rate of progression to active symptoms for low risk latent
#' tau = 1/2 # Rate of recovery
#' mu = 1/81 # Rate of natural mortality
#' dt <- 1
#'
#' parameters <- c(beta = beta, gamma_H = gamma_H, gamma_L = gamma_L, nu = nu, tau = tau, mu = mu)
#' inits <- c(S = S_0, H = H_0, L = L_0, I = I_0, R_0 = R_0)
#'
#' SHLIR_demographics_ode(1, inits, parameters)
SHLIR_demographics_ode <- function(t, x, params) {

  ## Specify model compartments
  S <- x[1]
  H <- x[2]
  L <- x[3]
  I <- x[4]
  R <- x[5]

  with(as.list(params),{

    ## Specify total population
    N = S + H + L + I + R

    ## Derivative Expressions
    dS = - beta * S * I / N - mu * S + mu * N
    ## These are the new equations - fill in the remaining terms
    dH = beta * (S + L) * I / N - gamma_H * H - nu * H - mu * H
    dL = nu * H - beta * L * I / N - gamma_L * L - mu * L
    ## Hint terms are missing from this equation as well
    dI = gamma_H * H + gamma_L * L - tau * I - mu * I
    dR = tau * I - mu * R

    ## output
    derivatives <- c(dS, dH, dL, dI, dR)

    list(derivatives)
  })
}

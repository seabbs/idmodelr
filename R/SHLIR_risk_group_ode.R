#' A Simple Susceptible-High-risk-latent-Low-risk-latent-Infected-Recovered Infectious Disease Model with demographics
#'
#' @description A more complex SHLIR model flow diagram, with risk groups, treatment, and reinfection for those who have recovered from active disease
#' @inherit SEIR_ode
#' @export
#'
#' @examples
#'
#' ## initialise
#' inits <- c(
#' # General population
#' S = 800,
#' H = 0,
#' L = 0,
#' I = 0,
#' Tr = 0,
#' R = 0,
#' ## High risk population
#' S_H = 199,
#' H_H = 0,
#' L_H = 0,
#' I_H = 1,
#' Tr_H = 0,
#' R_H = 0
#' )
#'
#' parameters <- c(
#' beta = 3, # Rate of transmission
#' beta_H = 6, # High risk rate of transmission
#' gamma_H = 1/5, # Rate of progression to active symptoms from high risk latent
#' nu = 1/2, #Rate of progression from high to low risk latent
#' gamma_L = 1/100, # Rate of progression to active symptoms for low risk latent
#' epsilon = 1/3, # Rate of treatment
#' tau = 1/2, # Rate of recovery
#' mu = 1/81, # Rate of natural mortality
#' p = 0.2, # proportion of new births that are high risk
#' M = 0.2 # Between group mixing
#' )
#'
#' SHLIR_risk_group_ode(1, inits, parameters)
#'
SHLIR_risk_group_ode <- function(t, x, params) {

  ## Specify model compartments - new model compartments need to be added here
  ## Add compartments in the order they appear in your model flow diagram
  ## Don't forget to update indexing for x. Compare the previous two models for a hint.
  S <- x[1]
  H <- x[2]
  L <- x[3]
  I <- x[4]
  Tr <- x[5]
  R <- x[6]

  S_H <- x[7]
  H_H <- x[8]
  L_H <- x[9]
  I_H <- x[10]
  Tr_H <- x[11]
  R_H <- x[12]

  with(as.list(params),{

    ## Specify total population - add new compartments here
    ## If this isn't working you simulations will likely blow up over time!
    N = S + H + L + I + Tr + R + S_H + H_H + L_H + I_H + Tr_H + R_H

    # Force of infection
    foi <- beta  * I / N + M * beta_H * I_H / N
    foi_H <-  M * beta  * I / N +  beta_H * I_H / N

    ## Derivative Expressions
    # Again new compartments here along with new model terms
    # Don't forget to add any new model terms for existing compartments
    ## General population
    dS = - S * foi - mu * S + (1 - p) * mu * N
    dH = (S + L + R) * foi - gamma_H * H - nu * H - mu * H
    dL = nu * H - L * foi - gamma_L * L - mu * L
    dI = gamma_H * H + gamma_L * L - epsilon * I - mu * I
    dTr = epsilon * I - tau * Tr - mu * Tr
    dR = tau * Tr - R * foi - mu * R

    ## High risk population
    dS_H = - S_H * foi_H - mu * S_H + p * mu * N
    dH_H = (S_H + L_H + R_H) * foi_H - gamma_H * H_H - nu * H_H - mu * H_H
    dL_H = nu * H_H - L_H * foi_H - gamma_L * L_H - mu * L_H
    dI_H = gamma_H * H_H + gamma_L * L_H - epsilon * I_H - mu * I_H
    dTr_H = epsilon * I_H - tau * Tr_H - mu * Tr_H
    dR_H = tau * Tr_H - R_H * foi_H - mu * R_H

    ## output
    # Finally  add your new derivative equations here
    # These need to be in the same order as you specified for the model compartments!
    # If this is wrong it is likely your results will look nothing like the previous models!
    derivatives <- c(dS, dH, dL, dI, dTr, dR, dS_H, dH_H, dL_H, dI_H, dTr_H, dR_H)

    list(derivatives)
  })
}




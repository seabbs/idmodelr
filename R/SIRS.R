#' Susceptible-Infected-Recovered-Susceptible Model
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
#' chi <- 0.5
#' tau <- 2
#' dt <- 1
#'
#' parameters <- c(beta = beta, tau = tau, chi = chi)
#' inits <- c(S = S_0, I = I_0, R_0 = R_0)
#'
#' SIRS_ode(1, inits, parameters)
SIRS_ode <- function(t, x, params) {

  ## Specify model compartments
  S <- x[1]
  I <- x[2]
  R <- x[3]

  with(as.list(params),{

    ## Specify total population
    N = S + I + R

    ## Derivative Expressions
    dS = - beta * S * I / N + chi * R
    dI = beta * S * I / N - tau * I
    dR = tau * I - chi * R

    ## output
    derivatives <- c(dS, dI, dR)

    list(derivatives)
  })
}

#' Susceptible-Infected-Recovered-Susceptible Model with Simple Demographics
#'
#' @inherit SIRS_ode
#' @export
#'
#' @examples
#' ##Model Input
#' S_0 <- 989
#' I_0 <- 1
#' R_0 <- 0
#' beta <- 3
#' chi <- 0.5
#' tau <- 2
#' mu <- 1/81
#' dt <- 1
#'
#' parameters <- c(beta = beta, tau = tau, mu = mu)
#' inits <- c(S = S_0, I = I_0, R_0 = R_0)
#'
#' SIRS_demographics_ode(1, inits, parameters)
SIRS_demographics_ode <- function(t, x, params) {

  ## Specify model compartments
  S <- x[1]
  I <- x[2]
  R <- x[3]

  with(as.list(params),{

    ## Specify total population
    N = S + I + R

    ## Derivative Expressions
    dS = - beta * S * I / N + chi * R - mu * S + mu * N
    dI = beta * S * I / N - tau * I - mu * I
    dR = tau * I - chi * R - mu * R

    ## output
    derivatives <- c(dS, dI, dR)

    list(derivatives)
  })
}


#' Susceptible-Infected-Recovered-Susceptible Model with Vaccination
#'
#' @inherit SIRS_ode
#' @export
#'
#' @examples
#' ##Model Input
#' S_u_0 <- 989
#' I_u_0 <- 1
#' R_u_0 <- 0
#' S_v_0 <- 0
#' I_v_0 <- 0
#' R_v_0 <- 0
#' beta <- 3
#' chi <- 0.5
#' tau <- 2
#' lambda <- 0.7
#' dt <- 1
#'
#' parameters <- c(beta = beta, tau = tau,
#'                 chi = chi, lambda = 0.7)
#' inits <- c(S_u = S_u_0, I_u = I_u_0, R_u_0 = R_u_0,
#'            S_v = S_v_0, I_v = I_v_0, R_v_0 = R_v_0)
#'
#' SIRS_vaccination_ode(1, inits, parameters)
SIRS_vaccination_ode <- function(t, x, params) {

  ## Specify model compartments
  S_u <- x[1]
  I_u <- x[2]
  R_u <- x[3]
  S_v <- x[4]
  I_v <- x[5]
  R_v <- x[6]

  with(as.list(params),{

    ## Specify total population
    N = S_u + I_u + R_u + S_v + I_v + R_v

    ## Derivative Expressions
    dS_u = - beta * S_u * (I_u + I_v) / N + chi * R_u
    dI_u = beta * S_u * (I_u + I_v) / N - tau * I_u
    dR_u = tau * I_u - chi * R_u

    dS_v = - (1 - lambda) * beta * S_v * (I_u + I_v) / N + chi * R_v
    dI_v = (1 - lambda) * beta * S_v * (I_u + I_v) / N - tau * I_v
    dR_v = tau * I_v - chi * R_v

    ## output
    derivatives <- c(dS_u, dI_u, dR_u, dS_v, dI_v, dR_v)

    list(derivatives)
  })
}


#' Susceptible-Infected-Recovered-Susceptible Model with Simple Demographics and Vaccination
#'
#' @inherit SIRS_ode
#' @export
#'
#' @examples
#' ##Model Input
#' S_u_0 <- 989
#' I_u_0 <- 1
#' R_u_0 <- 0
#' S_v_0 <- 0
#' I_v_0 <- 0
#' R_v_0 <- 0
#' beta <- 3
#' tau <- 2
#' chi <- 0.5
#' mu <- 1/81
#' alpha <- 0.8
#' lambda <- 0.7
#' dt <- 1
#'
#' parameters <- c(beta = beta, tau = tau, mu = mu,
#'                 alpha = 0.8, lambda = 0.7)
#' inits <- c(S_u = S_u_0, I_u = I_u_0, R_u_0 = R_u_0,
#'            S_v = S_v_0, I_v = I_v_0, R_v_0 = R_v_0)
#'
#' SIRS_vaccination_demographics_ode(1, inits, parameters)
SIRS_vaccination_demographics_ode <- function(t, x, params) {

  ## Specify model compartments
  S_u <- x[1]
  I_u <- x[2]
  R_u <- x[3]
  S_v <- x[4]
  I_v <- x[5]
  R_v <- x[6]

  with(as.list(params),{

    ## Specify total population
    N = S_u + I_u + R_u + S_v + I_v + R_v

    ## Derivative Expressions
    dS_u = - beta * S_u * (I_u + I_v) / N + chi * R_u - mu * S_u + (1 - alpha) * mu * N
    dI_u = beta * S_u * (I_u + I_v) / N - tau * I_u - mu * I_u
    dR_u = tau * I_u - chi * R_u - mu * R_u

    dS_v = - (1 - lambda) * beta * S_v * (I_u + I_v) / N + chi * R_v - mu * S_v + alpha * mu * N
    dI_v = (1 - lambda) * beta * S_v * (I_u + I_v) / N - tau * I_v - mu * I_v
    dR_v = tau * I_v - chi * R_v - mu * R_v

    ## output
    derivatives <- c(dS_u, dI_u, dR_u, dS_v, dI_v, dR_v)

    list(derivatives)
  })
}

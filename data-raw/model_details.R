library(tibble)
library(dplyr)



## Implemented SI models
model_details <- tibble(
  model = c("SI_ode", "SI_demographics_ode"),
  model_family = "SI",
  time = "continuous",
  type = "deterministic",
  recovered = "no",
  exposed = "no",
  treated = "no",
  susceptible = "no",
  risk_stratified = "no",
  non_exponential = "no",
  simple_demographics = c("no", "yes"),
  vaccination = "yes",
  disease_example = "none",
  language = "R",
  parameters = list(c("beta"), c("beta", "mu"))
)

## Add implemented SIR models
model_details <- model_details %>%
  bind_rows(
    tibble(
      model = c("SIR_ode", "SIR_demographics_ode", "SIR_vaccination_ode", "SIR_vaccination_demographics_ode"),
      model_family = "SIR",
      time = "continuous",
      type = "deterministic",
      recovered = "no",
      exposed = "no",
      treated = "no",
      susceptible = "no",
      risk_stratified = "no",
      non_exponential = "no",
      simple_demographics = c("no", "yes", "no", "yes"),
      vaccination = c("no", "no", "yes", "yes"),
      disease_example = "none",
      language = "R",
      parameters = list(c("beta", "tau"), c("beta", "tau", "mu"),
                        c("beta", "tau", "lambda"), c("beta", "tau", "lambda", "alpha", "mu"))
    )
 )

## Add implemented SIS models
model_details <- model_details %>%
  bind_rows(
    tibble(
      model = c("SIS_ode", "SIS_demographics_ode"),
      model_family = "SIS",
      time = "continuous",
      type = "deterministic",
      recovered = "no",
      exposed = "no",
      treated = "no",
      susceptible = "yes",
      risk_stratified = "no",
      non_exponential = "no",
      simple_demographics = c("no", "yes"),
      vaccination = c("no", "no"),
      disease_example = "none",
      language = "R",
      parameters = list(c("beta", "chi"), c("beta", "chi", "mu"))
    )
  )


## Add implemented SEI models
model_details <- model_details %>%
  bind_rows(
    tibble(
      model = c("SEI_ode", "SEI_demographics_ode"),
      model_family = "SEI",
      time = "continuous",
      type = "deterministic",
      recovered = "no",
      exposed = "yes",
      treated = "no",
      susceptible = "no",
      risk_stratified = "no",
      non_exponential = "no",
      simple_demographics = c("no", "yes"),
      vaccination = "no",
      disease_example = "none",
      language = "R",
      parameters = list(c("beta", "gamma"), c("beta", "gamme", "mu"))
    )
  )

## Add implemented SEIR models
model_details <- model_details %>%
  bind_rows(
    tibble(
      model = c("SEIR_ode", "SEIR_demographics_ode"),
      model_family = "SEIR",
      time = "continuous",
      type = "deterministic",
      recovered = "yes",
      exposed = "yes",
      treated = "no",
      susceptible = "no",
      risk_stratified = "no",
      non_exponential = "no",
      simple_demographics = c("no", "yes"),
      vaccination = "no",
      disease_example = "none",
      language = "R",
      parameters = list(c("beta", "gamma", "tau"), c("beta", "gamme", "tau", "mu"))
    )
  )

## Add implemented SEIS models
model_details <- model_details %>%
  bind_rows(
    tibble(
      model = c("SEIS_ode", "SEIS_demographics_ode"),
      model_family = "SEIS",
      time = "continuous",
      type = "deterministic",
      recovered = "no",
      exposed = "yes",
      treated = "no",
      susceptible = "yes",
      risk_stratified = "no",
      non_exponential = "no",
      simple_demographics = c("no", "yes"),
      vaccination = "no",
      disease_example = "none",
      language = "R",
      parameters = list(c("beta", "gamma", "chi"), c("beta", "gamme", "chi", "mu"))
    )
  )

## Add implemented SEIRS models
model_details <- model_details %>%
  bind_rows(
    tibble(
      model = c("SEIRS_ode", "SEIRS_demographics_ode"),
      model_family = "SEIRS",
      time = "continuous",
      type = "deterministic",
      recovered = "yes",
      exposed = "yes",
      treated = "no",
      susceptible = "yes",
      risk_stratified = "no",
      non_exponential = "no",
      simple_demographics = c("no", "yes"),
      vaccination = "no",
      disease_example = "none",
      language = "R",
      parameters = list(c("beta", "gamma", "tau", "chi"), c("beta", "gamma", "tau", "chi", "mu"))
    )
  )

## Add implemented SHLIR models
model_details <- model_details %>%
  bind_rows(
    tibble(
      model = c("SHLIR_ode", "SHLIR_demographics_ode"),
      model_family = "SHLIR",
      time = "continuous",
      type = "deterministic",
      recovered = "yes",
      exposed = "yes",
      treated = "no",
      susceptible = "no",
      risk_stratified = "no",
      non_exponential = "yes",
      simple_demographics = c("no", "yes"),
      vaccination = "no",
      disease_example = "none",
      language = "R",
      parameters = list(c("beta", "gamma_H", "nu", "gamma_L", "tau"),
                        c("beta", "gamma_H", "nu", "gamma_L", "tau", "mu"))
    )
  )

## Add implemented SHLITR models
model_details <- model_details %>%
  bind_rows(
    tibble(
      model = c("SHLITR_ode", "SHLIR_demographics_ode", "SHLITR_risk_ode", "SHLITR_risk_demographics_ode"),
      model_family = "SHLITR",
      time = "continuous",
      type = "deterministic",
      recovered = "yes",
      exposed = "yes",
      treated = "yes",
      susceptible = "no",
      risk_stratified = c("no", "no", "yes", "yes"),
      non_exponential = "no",
      simple_demographics = c("no", "yes", "no", "yes"),
      vaccination = "no",
      disease_example = "tuberculosis",
      language = "R",
      parameters = list(c("beta", "gamma_H", "nu", "gamma_L", "epsilon", "tau"),
                        c("beta", "gamma_H", "nu", "gamma_L", "epsilon", "tau", "mu"),
                        c("beta", "beta_H", "M", "gamma_H", "nu", "gamma_L", "epsilon", "tau"),
                        c("beta", "beta_H", "M", "gamma_H", "nu", "gamma_L", "epsilon", "tau", "p", "mu"))
    )
  )

## Add to package
usethis::use_data(model_details, overwrite = TRUE)
saveRDS(model_details, "model_details.rds")


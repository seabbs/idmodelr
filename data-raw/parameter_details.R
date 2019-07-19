library(tibble)
library(dplyr)



## Implemented SI models
parameter_details <- tibble(
  parameter = c("beta", "beta_H", "M", "gamma", "gamma_H", "gamma_L",
                "nu", "epsilon", "tau", "chi", "lambda", "alpha", "p", "mu"),
  parameter_family = c("transmission", "transmission", "mixing", rep("activation", 4), "treatment",
                       "recovery", "resusceptibility", rep("vaccination", 2), "risk stratification", "demographics"),
  description = c("Transmission rate = the transmission probability per contact * the number of contacts each individual has.",
                  "High risk transmission rate. Transmission rate impacting the high risk population. See beta for further details.",
                  "A mixing constant defining mixing between two stratified populations.",
                  "Activation rate. The reciprocal of latent/exposed period.",
                  "High risk activation rate. The reciprocal of the high risk latent/exposed period.",
                  "Low risk activation rate. The reciprocal of the low risk latent/exposed periord.",
                  "The rate of transition between high and low risk latency.",
                  "Successful treatment rate. The reciprocal of the treatment time.",
                  "Recovery rate. The reciprocal of the time infectious.",
                  "The rate of transition between recovery and susceptibility. The reciprocal of the time recovered but not susceptible.",
                  "The effectiveness of vaccination at preventing initial infection.",
                  "The coverage of vaccination.",
                  "The proportion of the population that are in the high risk population.",
                  "The natural mortality rate. The reciprocal of the average lifespan. (for simple demographics this is also the birth rate."),
  type = c("rate", "rate", "probability", rep("rate", 7), "probability", "proportion", "proportion", "rate"),
  risk_stratified = c("no", "yes", "yes", rep("no", 9), "yes", "no"),
  non_exponential = "no"
)


## Add to package
usethis::use_data(parameter_details, overwrite = TRUE)
saveRDS(parameter_details, "parameter_details.rds")


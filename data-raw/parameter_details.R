library(tibble)
library(dplyr)



## Implemented SI models
parameter_details <- tibble(
  parameter = c("beta", "beta_H", "M", "gamma", "gamma_H", "gamma_L",
                "nu", "epsilon", "tau", "chi", "lambda", "alpha", "p", "mu"),
  parameter_family = c("transmission", "transmission", "mixing", rep("activation", 4), "treatment",
                       "recovery", "resusceptibility", rep("vaccination", 2), "risk stratification", "demographics"),
  description = ""

                  ,
  type = c("rate", "rate", "probability", rep("rate", 7), "probability", "proportion", "proportion", "rate"),
  risk_stratified = c("no", "yes", "yes", rep("no", 9), "yes", "no"),
  non_exponential = "no"
)


## Add to package
usethis::use_data(parameter_details, overwrite = TRUE)
saveRDS(parameter_details, "parameter_details.rds")


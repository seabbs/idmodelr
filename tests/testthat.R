Sys.setenv("R_TESTS" = "")

library(testthat)
library(idmodelr)

test_check("idmodelr")

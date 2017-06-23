context("combine_strat_model_output.R")
library(tibble)

df <- data_frame(time = c(1, 2), S1 = c(1, 2),
                 S2 = c(1, 2), S3 = c(1, 3),
                 C1 = c(1, 7), C2 = c(2, 4),
                 C3 = c(3, 9))
df_without_strat <- data_frame(time = c(1, 2), S = c(3, 7),
                               C = c(6, 20))



test_that("Requires input of stratification level (strat)",{
  expect_error(combine_strat_model_output(df))
})

test_that("Correctly dedimensionalises successful with default arguements", {

  expect_equal(df_without_strat[, -1], combine_strat_model_output(df[,-1], strat  = 3))
})

test_that("Holding out of time works as expected", {
  expect_equal(df_without_strat, combine_strat_model_output(df, strat  = 3,
                                                            hold_out_var = "time"))
})


test_that("Specifying compartments, automatically specifies hold out variables", {
  expect_equal(df_without_strat, combine_strat_model_output(df, strat  = 3,
                                                           compartments = c("S", "C")))
})


test_that("Specifying hold out variables, automatically specifies compartments", {
  expect_equal(df_without_strat, combine_strat_model_output(df, strat  = 3,
                                                            hold_out_var = "time"))
})

context("combine_to_age_model.R")
library(tibble)

## Data frames for tests
df <- data_frame(time = c(1, 2), S1 = c(1,2), S2 = c(1, 3), E1 = c(4, 1), E2 = c(3, 4))


df_age <- data_frame(time = c(1, 2), age_group_1 = c(5, 3),
                               age_group_2 = c(4, 7), N = c(9, 10))


combine_to_age_model(df, age_com = 2, hold_out_var = "time")


test_that("Requires input of age compartments (age_com)",{
  expect_error(combine_to_age_model(df))
})

test_that("Correctly dedimensionalises successful with default arguements", {

  expect_equal(df_age[, -1], combine_to_age_model(df[,-1], age_com  = 2))
})

test_that("Holding out of time works as expected", {
  expect_equal(df_age, combine_to_age_model(df, age_com = 2,
                                            hold_out_var = "time"))
})


test_that("Specifying compartments, automatically specifies hold out variables", {
  expect_equal(df_age, combine_to_age_model(df, age_com  = 2,
                                                            compartments = c("S", "E")))
})


test_that("Specifying hold out variables, automatically specifies compartments", {
  expect_equal(df_age, combine_to_age_model(df, age_com  = 2,
                                                            hold_out_var = "time"))
})

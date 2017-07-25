context("gather_strat_variable")


df <- tibble::tibble(time = 0, A1 = 1, A2 = 2, A3 = 3)
df_results <- gather_strat_variable(df, id_col = "Age", compartment = "A",
                                    strat = 3, groups = c("Children", "Young adults", "Adults"))
df_check <- tibble::tibble(time = 0, Age = factor(c("Children", "Young adults", "Adults")),
                           A = c(1, 2, 3))


test_that("gather_strat_variable can gather a stratified variable when hold_out_var is not set", {
  expect_equal(df_check, df_results)
})

test_that("gather_strat_variable can gather a stratified variable when hold_out_var is set", {
  df_results <- gather_strat_variable(df, id_col = "Age", compartment = "A", hold_out_var = "time",
                                      strat = 3, groups = c("Children", "Young adults", "Adults"))
  expect_equal(df_check, df_results)
})

test_that("gather_strat_variable can gather a stratified variable when the new compartment groups are not set", {
  df_results <- gather_strat_variable(df, id_col = "Age", compartment = "A", strat = 3)
  df_check <- tibble::tibble(time = 0, Age = factor(c("A1", "A2", "A3")),
                                         A = c(1, 2, 3))
  expect_equal(df_check, df_results)
})

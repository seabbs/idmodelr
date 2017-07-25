context("gather_strat_multi_variable")

df <- tibble::tibble(time = 1, A1 = 1, A2 = 2, A3 = 3, B1 = 2, B2 = 3, B3 = 0)
df_results <- gather_strat_multi_variable(df, id_col = "Age", compartment = c("A", "B"), hold_out_var = "time",
                      strat = 3, groups = c("Children", "Young adults", "Adults"))
df_check <- tibble::tibble(time = 1, Age = factor(c("Children", "Young adults", "Adults")),
                           A = c(1, 2, 3), B = c(2, 3, 0))


test_that("gather_strat_multi_variable can gather multiple stratified variables.", {
  expect_equal(df_check, df_results)
})

test_that("gather_strat_variable requires hold_out_var to be set.", {
  expect_error(gather_strat_multi_variable(df, id_col = "Age", compartment = c("A", "B"),
                                           strat = 3, groups = c("Children", "Young adults", "Adults")))
})

test_that("gather_strat_variable can gather a single stratified variable", {
  df_results <- gather_strat_multi_variable(df, id_col = "Age", compartment = c("A"), hold_out_var = "time",
                                            strat = 3, groups = c("Children", "Young adults", "Adults"))
  df_check <- tibble::tibble(time = 1, Age = factor(c("Children", "Young adults", "Adults")),
                             A = c(1, 2, 3))
  expect_equal(df_check, df_results)
})

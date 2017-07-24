context("summarise_var_by_strat")

df <- data.frame(A1 = 1, B1 = 1, A2 = 1, B2 = 1, A3 = 1, B3 = 1)

df_results <- summarise_var_by_strat(df, vars = c("A", "B"), strat = 3, new_var = "C")

df_check <- dplyr::bind_cols(tibble::tibble(C = 6, C1 = 2, C2 = 2, C3 = 2), df)


test_that("summarise_var_by_strat correctly summarises a data frame", {
  expect_equal(df_check, df_results)
})

test_that("summarise_strat_var errors when no varibales to stratify are supplied", {
  expect_error(summarise_var_by_strat(df, strat = 2, new_var = "sum"))
})

test_that("summarise_strat_var correctly summarises a dataframe when no stratification is present", {
  df <- tibble::tibble(A = 1, B = 2)
  df_results <- tibble::tibble(C = 3, A = 1, B = 2)
  expect_equal(df_results, summarise_var_by_strat(df, vars = c("A", "B"), new_var = "C"))
})

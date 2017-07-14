context("summarise_strat_var")


df <- dplyr::mutate(iris[1:5,], Petal.Length1 = Petal.Length, Petal.Length2 = Petal.Length)

df_results <- summarise_strat_var(df, vars = c("Petal.Length"), strat = 2, new_var = "sum")

test_that("summarise_strat_var correctly summarises a numbered vector", {

  expect_equal(c(2.8, 2.8, 2.6, 3.0, 2.8), df_results$sum)
})

test_that("summarise_strat_var errors when no varibales to stratofy are supplied", {
  expect_error(summarise_strat_var(df, strat = 2, new_var = "sum"))
})

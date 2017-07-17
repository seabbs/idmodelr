context("model_df_to_vector")


test_that("model_df_to_vector works when dataframe has no repeats", {
  answer <- c(1.4, 1.4, 1.3, 1.5, 1.4)
  vector_no_repeats <- model_df_to_vector(iris[1:5,], Petal.Length)
  expect_equal(answer, vector_no_repeats)
})

test_that( "model_df_vector works when dataframe does have repeats but they are the same", {
  df <- dplyr::bind_rows(dplyr::mutate(iris[1:5,], sim = 1, id = 1:length(sim)),
                         dplyr::mutate(iris[1:5,], sim = 2 , id = 1:length(sim)))

  answer <- c(1.4, 1.4, 1.3, 1.5, 1.4)
  df_results <- model_df_to_vector(df, Petal.Length, "id", sum_fn = mean)
  expect_equal(answer, df_results)
})


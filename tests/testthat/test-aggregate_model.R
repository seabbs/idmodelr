context("aggregate_model")



df <- data.frame(A1 = 1, B1 = 1, A2 = 1, B2 = 1, A3 = 1, B3 = 1, time = 1)


test_that("aggregate_model can correctly process the inputs without aggregation.", {
  test <- aggregate_model(df, aggregate_to = "incidence",
                          compartments = c("A", "B"), strat = 3,
                          summary_var = TRUE, test = TRUE)

  expect_known_output(test, file = "../../tests/test-files/aggregate_model/test-01.rds")
})


test_that("aggregate_model can correctly process the inputs without aggregation,
          with a hold out variable.", {
  test <- aggregate_model(df, aggregate_to = "incidence",
                          compartments = c("A", "B"), strat = 3,
                          summary_var = TRUE, test = TRUE, hold_out_var = "time")

  expect_known_output(test, file = "../../tests/test-files/aggregate_model/test-02.rds")
})


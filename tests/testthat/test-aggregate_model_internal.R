context("aggregate_model_internal")


df <- data.frame(time = 1, A1 = 1, B1 = 1, A2 = 1, B2 = 1, A3 = 1, B3 = 1)


test_that("aggregate_model_internal can correctly aggregate incidence ", {

  inc <- aggregate_model_internal(df, aggregate_to = "incidence",
                                  compartments = c("A", "B"), strat = 3,
                                  summary_var = TRUE)
  skip_on_cran()
  expect_known_output(inc, file = "../../tests/test-files/aggregate_model_internal/test-01.rds")
})


test_that("aggregate_model_internal can correctly aggregate demographics", {

  demo <- aggregate_model_internal(df, aggregate_to = "demographic",
                                  compartments = c("A", "B"), strat = 3,
                                  summary_var = TRUE)
  skip_on_cran()
  expect_known_output(demo, file = "../../tests/test-files/aggregate_model_internal/test-02.rds")
})


test_that("aggregate_model_internal can correctly aggregate disease", {

  dis <- aggregate_model_internal(df, aggregate_to = "disease",
                                   compartments = c("A", "B"), strat = 3,
                                   summary_var = TRUE)
  skip_on_cran()
  expect_known_output(demo, file = "../../tests/test-files/aggregate_model_internal/test-03.rds")
})

test_that("aggregate_model_internal can correctly aggregate to long format (i.e tidy)", {

  tidy <- aggregate_model_internal(df, aggregate_to = "tidy",
                                  compartments = c("A", "B"), hold_out_var = "time", strat = 3,
                                  summary_var = TRUE, id_col = "Age",
                                  groups = c("Children", "Young adults", "Adults"))
  skip_on_cran()
  expect_known_output(tidy, file = "../../tests/test-files/aggregate_model_internal/test-04.rds")
})


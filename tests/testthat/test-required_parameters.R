context("required_parameters")

test_that("required_parameters can retrieve parameter details for the SIR model", {

  output <- required_parameters("SIR_ode")

  target <- dplyr::filter(idmodelr::parameter_details,
                          parameter %in% unlist(idmodelr::model_details[grepl("SIR_ode", idmodelr::model_details$model), "parameters"]))
  expect_equal(output, target)
})

test_that("required_parameters fails informatively when an unknown model is searched for", {

  expect_error(required_parameters("HFHHF"))
})

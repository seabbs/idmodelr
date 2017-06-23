context("add_pointer_struct.R")


test_that("add_pointer_struct adds the correct structure", {
  expect_equal(c("S1", "S2", "S3") , add_pointer_struct("S", length = 3))
})

test_that("add_pointer_struct can handle vector input", {
  expect_equal(c("S1", "S2", "S3", "I1", "I2", "I3"),
               add_pointer_struct(c("S", "I"), length = 3))
})

context("Validity checks for settings")

test_that("Valid options are accepted", {
  expect_type(new_settings(xlim = 10), "list")
  expect_type(new_settings(min_distance = 1), "list")
})

test_that("Invalid options are refused", {
  expect_error(new_settings(new_parameter = T))
  expect_error(new_settings(min_dist = 10))
})



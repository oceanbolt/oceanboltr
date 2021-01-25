test_that("Output format is correct", {
  expect_true("data.table" %in% class(getPortCalls()))
})

test_that("Parameters are handled correctly", {
  expect_error(getPortCalls(imo = "wrong_value"))
  expect_error(getPortCalls(portId = "wrong_value"))
  expect_error(getPortCalls(unlocode = "wrong_value"))

  expect_true(nrow(getPortCalls(page = 1, limit = 50)) == 50)

  expect_warning(getPortCalls(limit = 5000))
  expect_error(getPortCalls(token = "<WRONG_TOKEN>"))
  expect_error(getPortCalls(token = NULL))
})

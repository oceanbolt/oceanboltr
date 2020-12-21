test_that("Output format is correct", {
  expect_true(all(names(listPorts()) %in% c(
    "portId",
    "portName",
    "countryCode",
    "region",
    "unlocode"
  )))
  expect_true("data.table" %in% class(listPorts()))
  expect_true(nrow(listPorts()) > 0)
})

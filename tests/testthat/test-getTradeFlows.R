test_that("Output format is correct", {
  expect_true("data.table" %in% class(getTradeFlows()))
})

test_that("Parameters are handled correctly", {
  expect_error(getTradeFlows(commodity = "wrong_commodity"))
  expect_error(getTradeFlows(segment = "wrong_segment"))
  expect_identical(
    getTradeFlows(segment = "handysize", page = 1)[order(flowId)],
    getTradeFlows(subSegment = c(
      "small_handysize",
      "large_handysize"
    ), page = 1)[order(flowId)]
  )

  expect_identical(
    getTradeFlows(direction = c(), page = 1)[order(flowId)],
    getTradeFlows(direction = c("export", "import"), page = 1)[order(flowId)]
  )
  expect_error(getTradeFlows(direction = "wrong_direction"))

  expect_error(getTradeFlows(subSegment = "wrong_sub_segment"))
  expect_error(getTradeFlows(fromDate = "wrong_date"))
  expect_true(
    nrow(getTradeFlows(fromDate = "2020-01-01", page = 1, limit = 50)) <= 50
  )
  expect_true(
    nrow(getTradeFlows(
      fromDate = as.Date("2020-01-01"),
      page = 1, limit = 50
    )) <= 50
  )
  expect_error(getTradeFlows(toDate = "wrong_date"))

  expect_error(getTradeFlows(fromCountryCode = "WRONG"))
  expect_error(getTradeFlows(toCountryCode = "WRONG"))
  expect_error(getTradeFlows(fromRegion = "WRONG"))
  expect_error(getTradeFlows(toRegion = "WRONG"))
  expect_true(
    nrow(getTradeFlows(fromRegion = "BALTIC", fromCountryCode = "UA")) == 0
  )

  expect_error(getTradeFlows(excludeIntraCountry = "wrong type"))
  expect_error(getTradeFlows(excludeUnknownDestinations = "wrong type"))

  expect_identical(
    getTradeFlows(page = 1)[order(flowId)],
    getTradeFlows(segment = c(), subSegment = c(), page = 1)[order(flowId)]
  )

  expect_warning(getTradeFlows(limit = 5000))
  expect_error(getTradeFlows(token = "<WRONG_TOKEN>"))
  expect_error(getTradeFlows(token = NULL))
})

test_that("Output format is correct", {
  expect_true("data.table" %in% class(getTradeFlowsTimeseries()))
})

test_that("Parameters are handled correctly", {
  expect_error(getTradeFlowsTimeseries(range = "wrong_range"))
  expect_error(getTradeFlowsTimeseries(group = "wrong_group"))
  expect_error(getTradeFlowsTimeseries(commodity = "wrong_commodity"))
  expect_error(getTradeFlowsTimeseries(segment = "wrong_segment"))
  expect_identical(
    getTradeFlowsTimeseries(segment = "handysize")[order(date, group)],
    getTradeFlowsTimeseries(subSegment = c(
      "small_handysize",
      "large_handysize"
    ))[order(date, group)]
  )

  expect_identical(
    getTradeFlowsTimeseries(direction = c())[order(date, group)],
    getTradeFlowsTimeseries(direction = c("export", "import"))[
      order(date, group)
    ]
  )
  expect_error(getTradeFlowsTimeseries(direction = "wrong_direction"))

  expect_error(getTradeFlowsTimeseries(subSegment = "wrong_sub_segment"))
  expect_error(getTradeFlowsTimeseries(fromDate = "wrong_date"))
  expect_error(getTradeFlowsTimeseries(toDate = "wrong_date"))

  expect_error(getTradeFlowsTimeseries(fromCountryCode = "WRONG"))
  expect_error(getTradeFlowsTimeseries(toCountryCode = "WRONG"))
  expect_error(getTradeFlowsTimeseries(fromRegion = "WRONG"))
  expect_error(getTradeFlowsTimeseries(toRegion = "WRONG"))
  expect_true(
    nrow(
      getTradeFlowsTimeseries(fromRegion = "BALTIC", fromCountryCode = "UA")
    ) == 0
  )

  expect_error(getTradeFlowsTimeseries(excludeIntraCountry = "wrong type"))
  expect_error(
    getTradeFlowsTimeseries(excludeUnknownDestinations = "wrong type")
  )

  expect_identical(
    getTradeFlowsTimeseries()[order(date, group)],
    getTradeFlowsTimeseries(segment = c(), subSegment = c())[order(date, group)]
  )

  expect_error(getTradeFlowsTimeseries(token = "<WRONG_TOKEN>"))
  expect_error(getTradeFlowsTimeseries(token = NULL))
})

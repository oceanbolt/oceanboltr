test_that("Output format is correct", {
  expect_true("data.table" %in% class(getCongestionTimeseries()))
})

test_that("Parameters are handled correctly", {
  expect_error(getCongestionTimeseries(regionId = "wrong_region"))
  expect_error(getCongestionTimeseries(countryCode = "wrong_country_code"))
  expect_error(getCongestionTimeseries(commodity = "wrong_commodity"))
  expect_error(
    getCongestionTimeseries(commodityGroup = "wrong_commodity_group")
  )
  expect_error(getCongestionTimeseries(segment = "wrong_segment"))
  expect_error(getCongestionTimeseries(subSegment = "wrong_sub_segment"))
  expect_error(getCongestionTimeseries(lastLoadCountry = "wrong_country_code"))
  expect_error(getCongestionTimeseries(operation = "wrong_operation"))

  expect_equal(
    getCongestionTimeseries(),
    getCongestionTimeseries(segment = c(), subSegment = c())
  )

  expect_true(
    nrow(getCongestionTimeseries(regionId = "BALTIC", countryCode = "UA")) == 0
  )

  expect_true(
    nrow(getCongestionTimeseries(segment = c(),
                           subSegment = c("large_handysize"))) > 0
  )

  expect_error(getCongestionTimeseries(token = "<WRONG_TOKEN>"))
  expect_error(getCongestionTimeseries(token = NULL))
})

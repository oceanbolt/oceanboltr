test_that("Output format is correct", {
  expect_true("data.table" %in% class(getCongestionLive()))
})

test_that("Parameters are handled correctly", {
  expect_error(getCongestionLive(regionId = "wrong_region"))
  expect_error(getCongestionLive(countryCode = "wrong_country_code"))
  expect_error(getCongestionLive(commodity = "wrong_commodity"))
  expect_error(getCongestionLive(commodityGroup = "wrong_commodity_group"))
  expect_error(getCongestionLive(segment = "wrong_segment"))
  expect_error(getCongestionLive(subSegment = "wrong_sub_segment"))
  expect_error(getCongestionLive(lastLoadCountry = "wrong_country_code"))
  expect_error(getCongestionLive(operation = "wrong_operation"))

  expect_equal(
    getCongestionLive(),
    getCongestionLive(segment = c(), subSegment = c())
  )

  expect_true(
    nrow(getCongestionLive(regionId = "BALTIC", countryCode = "UA")) == 0
  )

  expect_true(
    nrow(getCongestionLive(segment = c(),
                           subSegment = c("large_handysize"))) > 0
  )

  expect_error(getCongestionLive(token = "<WRONG_TOKEN>"))
  expect_error(getCongestionLive(token = NULL))
})

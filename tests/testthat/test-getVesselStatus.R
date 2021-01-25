test_that("Output format is correct", {
  expect_true("data.table" %in% class(getVesselStatus()))
})

test_that("Parameters are handled correctly", {
  expect_error(getVesselStatus(direction = "wrong_direction"))
  expect_error(getVesselStatus(ladenStatus = "wrong_laden_status"))
  expect_error(getVesselStatus(imo = "wrong_imo"))
  expect_error(getVesselStatus(loadCountry = "wrong_country"))
  expect_error(getVesselStatus(destinationCountry = "wrong_country"))
  expect_error(getVesselStatus(loadRegion = "wrong_region"))
  expect_error(getVesselStatus(destinationRegion = "wrong_region"))
  expect_error(getVesselStatus(zoneId = "wrong_zone"))
  expect_error(getVesselStatus(segment = "wrong_segment"))
  expect_error(getVesselStatus(subSegment = "wrong_sub_segment"))
  expect_error(getVesselStatus(excludeUnknownDestinations = "wrong_boolean"))
  expect_error(getVesselStatus(excludeMpv = "wrong_boolean"))

  expect_equal(
    getVesselStatus()[order(imo)],
    getVesselStatus(segment = c(), subSegment = c())[order(imo)]
  )

  expect_true(
    nrow(getVesselStatus(segment = c(),
                         subSegment = c("large_handysize"))) > 0
  )

  expect_true(
    nrow(getVesselStatus(loadRegion = "BALTIC",
                         loadCountry = "BY",
                         destinationCountry = "ZW")) == 0
  )

  expect_error(getVesselStatus(token = "<WRONG_TOKEN>"))
  expect_error(getVesselStatus(token = NULL))
})

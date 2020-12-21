test_that("Output format is correct", {
  expect_true("data.table" %in% class(getTonnageZoneCount()))
})

test_that("Parameters are handled correctly", {
  expect_error(getTonnageZoneCount(zoneId = -1))
  expect_error(getTonnageZoneCount(segment = "wrong_segment"))
  expect_identical(
    getTonnageZoneCount(),
    getTonnageZoneCount(segment = c(), subSegment = c())
  )
  expect_identical(
    getTonnageZoneCount(segment = "handysize"),
    getTonnageZoneCount(subSegment = c(
      "small_handysize",
      "large_handysize"
    ))
  )
  expect_error(getTonnageZoneCount(subSegment = "wrong_sub_segment"))
  expect_identical(
    getTonnageZoneCount(direction = c()),
    getTonnageZoneCount(direction = c(
      "NNE", "ENE", "ESE", "SSE",
      "SSW", "WSW", "WNW", "NNW"
    ))
  )
  expect_error(getTonnageZoneCount(direction = "wrong_direction"))
  expect_error(getTonnageZoneCount(ladenStatus = "wrong_laden_status"))
  expect_error(getTonnageZoneCount(portStatus = "wrong_port_status"))

  expect_error(getTonnageZoneCount(token = "<WRONG_TOKEN>"))
  expect_error(getTonnageZoneCount(token = NULL))
})

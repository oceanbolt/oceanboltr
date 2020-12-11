test_that("Output format is correct", {
  expect_true("data.table" %in% class(getFleetSpeed()))
})

test_that("Parameters are handled correctly", {
  expect_error(getFleetSpeed(zoneId = -1))
  expect_error(getFleetSpeed(segment = "wrong_segment"))
  expect_identical(
    getFleetSpeed(),
    getFleetSpeed(segment = c(), subSegment = c())
  )
  expect_identical(
    getFleetSpeed(segment = "handysize"),
    getFleetSpeed(subSegment = c(
      "small_handysize",
      "large_handysize"
    ))
  )
  expect_error(getFleetSpeed(subSegment = "wrong_sub_segment"))
  expect_identical(
    getFleetSpeed(direction = c()),
    getFleetSpeed(direction = c(
      "NNE", "ENE", "ESE", "SSE",
      "SSW", "WSW", "WNW", "NNW"
    ))
  )
  expect_error(getFleetSpeed(direction = "wrong_direction"))
  expect_error(getFleetSpeed(ladenStatus = "wrong_laden_status"))
  expect_error(getFleetSpeed(portStatus = "wrong_port_status"))

  expect_error(getFleetSpeed(token = "<WRONG_TOKEN>"))
  expect_error(getFleetSpeed(token = NULL))
})

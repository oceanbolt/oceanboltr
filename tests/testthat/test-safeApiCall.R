test_that("Parameters are handled correctly", {
  expect_error(safeApiCall())
  expect_error(safeApiCall(""))
  expect_error(safeApiCall(NA))
  expect_error(safeApiCall(NULL))
  expect_error(safeApiCall(GET))
  expect_error(safeApiCall(GET, "not-an-url"))
  expect_error(safeApiCall(GET, url = "not-an-url"))
})

test_that("Output is correct", {
  expect_true({
    response <- safeApiCall(GET, baseApiUrl)
    class(response) == "response"
  })

  expect_true({
    response <- safeApiCall(GET, baseApiUrl)
    status_code(response) == 404
  })

  expect_true({
    token <- Sys.getenv("OCEANBOLT_TOKEN")
    response <- safeApiCall(
      GET,
      paste0(baseApiUrl, "/entities/countries"),
      add_headers(Authorization = paste0("Bearer ", token))
    )
    status_code(response) == 200
  })
})

test_that("Rate limiting is working correctly", {
  expect_true({
    token <- Sys.getenv("OCEANBOLT_TOKEN")
    for (i in 1:125) {
      response <- safeApiCall(
        GET,
        paste0(baseApiUrl, "/entities/countries"),
        add_headers(Authorization = paste0("Bearer ", token))
      )
    }
    status_code(response) == 200
  })
})

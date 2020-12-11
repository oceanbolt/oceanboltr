test_that("Parameters are handled correctly", {
  expect_error(registerToken())
  expect_error(registerToken(""))
  expect_error(registerToken(NA))
  expect_error(registerToken(NULL))
  expect_error(registerToken(Sys.getenv("OCEANBOLT_TOKEN"), type = NA))
  expect_error(registerToken(Sys.getenv("OCEANBOLT_TOKEN"), type = NULL))
  expect_warning(registerToken(Sys.getenv("OCEANBOLT_TOKEN"),
    type = c("plain", "keyring")
  ))
  expect_error(registerToken(Sys.getenv("OCEANBOLT_TOKEN"),
    type = c("wrong_type")
  ))
  expect_true({
    if (!requireNamespace("keyring", quietly = TRUE)) {
      TRUE
    } else {
      registerToken(Sys.getenv("OCEANBOLT_TOKEN"), type = "keyring")
    }
  })
})

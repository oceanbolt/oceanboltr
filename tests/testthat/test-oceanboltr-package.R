test_that("Package loads correctly", {
  expect_silent(library(oceanboltr))
  expect_null(oceanboltr:::.onLoad())
  expect_equal(class(Sys.getenv("OCEANBOLT_TOKEN")), "character")
  expect_true({
    if (requireNamespace("keyring", quietly = TRUE)) {
      keys <- suppressWarnings(keyring::key_list("OCEANBOLT_TOKEN"))
      "service" %in% names(keys)
    } else {
      TRUE
    }
  })
})

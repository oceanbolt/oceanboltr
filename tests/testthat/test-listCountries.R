test_that("Output format is correct", {
  expect_true(all(names(listCountries()) %in% c("country", "countryCode")))
  expect_true("data.table" %in% class(listCountries()))
  expect_true(nrow(listCountries()) > 0)
})

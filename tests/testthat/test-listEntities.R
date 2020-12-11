test_that("Output format is correct", {
  expect_true("data.table" %in% class(listEntities("countries")))
  expect_true(nrow(listEntities("countries")) > 0)
})

test_that("Parameters are handled correctly", {
  expect_error(listEntities())
  expect_error(listEntities("reports"))
  expect_warning(listEntities(c("countries", "regions")))

  expect_error(listEntities("countries", token = NULL))
  expect_error(listEntities("countries", token = NA))
  expect_error(listEntities("countries", token = "Wrong token"))
})

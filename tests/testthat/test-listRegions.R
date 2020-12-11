test_that("Output format is correct", {
  expect_true(all(names(listRegions()) %in% c("regionId", "regionName")))
  expect_true("data.table" %in% class(listRegions()))
  expect_true(nrow(listRegions()) > 0)
})

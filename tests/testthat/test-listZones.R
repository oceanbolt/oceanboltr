test_that("Output format is correct", {
  expect_true(all(names(listZones()) %in% c("zoneId", "zoneName")))
  expect_true("data.table" %in% class(listZones()))
  expect_true(nrow(listZones()) > 0)
})

test_that("Output format is correct", {
  expect_true(all(names(listSegments()) %in% c(
    "segment",
    "subSegment",
    "segmentInt",
    "subSegmentInt",
    "subSegmentKey",
    "segmentKey",
    "cutoffHigh",
    "cutoffLow"
  )))
  expect_true("data.table" %in% class(listZones()))
  expect_true(nrow(listZones()) > 0)
})

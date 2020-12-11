test_that("Output format is correct", {
  expect_true(all(names(listCommodities()) %in% c(
    "commodity",
    "commodityValue",
    "commodityGroup",
    "commodityId"
  )))
  expect_true("data.table" %in% class(listCommodities()))
  expect_true(nrow(listCommodities()) > 0)
})

test_that("name doesn't exist", {
  vars <- c("diameter", "weight", "red", "green", "cat", "nameBin")
  expect_error(corrHeatmap(citrus, vars))
})

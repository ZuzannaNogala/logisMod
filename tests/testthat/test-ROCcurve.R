model <- createModels(citrus, nameBin ~  diameter + red + green)
models <- createModels(citrus, nameBin ~  diameter + red + green, nameBin ~ weight + blue)

test_that("too long sequance", {
  expect_error(drawROCsForEachModel(seq(0, 2, by = 0.01), "nameBin", model), 
               regexp = "thresholds_sequance")
})

test_that("wrong spelling", {
  expect_error(drawROCsForEachModel(seq(0, 1, by = 0.05), "namBin", model),
               regexp = "check name")
})


test_that("is drawing working?", {
  expect_no_error(drawROCsForEachModel(seq(0, 1, by = 0.05), "nameBin", models))
})
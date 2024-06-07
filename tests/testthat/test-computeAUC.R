models <-  createModels(citrus, nameBin ~ weight + red + green, nameBin ~ weight + red)
roc_stats <- sapply(1:2, function(i) pROC::roc(citrus[, nameBin], models[[i]]$fitted.values)$auc)

test_that("computing AUC works", {
  expect_equal(unlist(computeAUC("nameBin", citrus, models)$AUC), roc_stats)
})

test_that("wrong spelling Y name", {
  expect_error(computeAUC("namBin", citrus, models)$AUC,
               regexp = "check name")
})

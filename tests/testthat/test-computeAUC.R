test_that("computing AUC works", {
  models <-  createModels(citrus, nameBin ~ weight + red + green, nameBin ~ weight + red)
  roc_stats <- sapply(1:2, function(i) pROC::roc(citrus[, nameBin], model[[i]]$fitted.values)$auc)
  
  expect_equal(unlist(computeAUC("nameBin", citrus, models)$AUC), roc_stats)
})


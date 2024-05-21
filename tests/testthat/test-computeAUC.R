test_that("computing AUC works", {
  model <-  glm(nameBin ~ diameter + green + blue, data = citrus, family = binomial("logit"))
  roc_stats <- pROC::roc(citrus[, nameBin], model$fitted.values)
  
  expect_equal(computeAUC(model, "nameBin"), head(roc_stats$auc))
})

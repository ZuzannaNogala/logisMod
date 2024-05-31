test_that("Does computing elements of Eroor Matrix and model Stats works?", {
  model <- model <- logisMod(nameBin ~ diameter + green + red, citrus)
  preds <- resp_prediction_for_model(model)
  
  
  
  expect_TP <- length(which(preds >= 0.7 & citrus$nameBin == 1))
  expect_TN <- length(which(preds < 0.7 & citrus$nameBin == 0))
  expect_FP <- length(which(preds >= 0.7 & citrus$nameBin == 0))
  expect_FN <- length(which(preds < 0.7 & citrus$nameBin == 1))
  
  expected_Stats <- list("ErrorMatrix" = data.table(" " = c("actual_T", "actual_N"),
                                  "pred_T" = c(expect_TP, expect_FP),
                                  "pred_N" = c(expect_FN, expect_TN)),
                         "Accuracy" = (expect_TP + expect_TN) / 
                           (expect_TP + expect_TN + expect_FP + expect_FN), 
                         "Sensitivity" = expect_TP / (expect_TP + expect_FP),
                         "Specificity" = expect_TN / (expect_FP + expect_TN))
  
  actual_Stats <- CreateErrorMatrixStats(0.7, preds, citrus$nameBin)
  
  expect_equal(actual_Stats, expected_Stats)
    
})

test_that("No errors to visualization of Error Matrix", {
  model <- logisMod(nameBin ~ diameter + green + red + green, citrus)
  preds <- resp_prediction_for_model(model)
  
  expect_no_error(visualErrorMatrix(0.6, preds, citrus$nameBin))
})

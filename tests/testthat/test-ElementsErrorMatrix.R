test_that("Does computing elements of Eroor Matrix works?", {
  model <- model <- logisMod(nameBin ~ diameter + green + red, citrus)
  preds <- resp_prediction_for_model(model)
  
  expect_TP <- length(which(preds >= 0.7 & citrus$nameBin == 1))
  expect_TN <- length(which(preds < 0.6 & citrus$nameBin == 0))
  expect_FP <- length(which(preds >= 0.5 & citrus$nameBin == 0))
  expect_FN <- length(which(preds < 0.4 & citrus$nameBin == 1))
  
  expect_list <- list(expect_TP, expect_TN, expect_FP, expect_FN)
  actual_list <- list(True_Positive(0.7, preds, citrus$nameBin),
                      True_Negative(0.6, preds, citrus$nameBin),
                      False_Positive(0.5, preds, citrus$nameBin),
                      False_Negative(0.4, preds, citrus$nameBin))
  
  expect_equal(actual_list, expect_list)
    
})

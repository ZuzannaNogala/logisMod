test_that("Does computing elements of Eroor Matrix and model Stats works?", {
  models <- createModels(citrus, nameBin ~ diameter + green + red, nameBin ~ green + red)
  
  preds <- lapply(models, predictBasedData, citrus)
  
  prepare_Stats <- lapply(preds, function(pred_vec){
    expect_TP <- length(which(pred_vec >= 0.7 & citrus$nameBin == 1))
    expect_FN <- length(which(pred_vec < 0.7 & citrus$nameBin == 1))
    expect_FP <-  length(which(pred_vec >= 0.7 & citrus$nameBin == 0))
    expect_TN <- length(which(pred_vec < 0.7 & citrus$nameBin == 0))
    
    ErrorMatrix <- data.frame("pred_T" = c(expect_TP, expect_FP),
                              "pred_N" = c(expect_FN, expect_TN))
    
    rownames(ErrorMatrix) <- c("actual_T", "actual_N")
    
    list("ErrorMatrix" = ErrorMatrix,
         "Accuracy" = (expect_TP + expect_TN) / 
           (expect_TP + expect_TN + expect_FP + expect_FN), 
         "Sensitivity" = expect_TP / (expect_TP + expect_FP),
         "Specificity" = expect_TN / (expect_FP + expect_TN))
  })
  
  expected_Stats <- list(lapply(prepare_Stats, function(lst) lst$ErrorMatrix),
                         lapply(prepare_Stats, function(lst) lst$Accuracy),
                         lapply(prepare_Stats, function(lst) lst$Sensitivity),
                         lapply(prepare_Stats, function(lst) lst$Specificity))
  
  names(expected_Stats) <- c("ErrorMatrix", "Accuracy", "Sensitivity", "Specificity")
  

  actual_Stats <- CreateErrorMatrixStats(0.7, citrus$nameBin, models)
  
  expect_equal(actual_Stats, expected_Stats)
    
})

test_that("No errors to visualization of Error Matrix", {
   model <- createModels(citrus, nameBin ~ diameter + green + red + green)
   preds <- predictBasedData(model[[1]], citrus)
   
   expect_no_error(visualErrorMatrix(0.6, citrus$nameBin, model))
 })

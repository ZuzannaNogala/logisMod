#' Elements of Confusion / Error Matrix - True Positive
#' 
#' \code{.True_Positive} returns value of True Positives (TP) - number of good predictions of successes
#'  by model (actual success was predicted by model as succes)
#' 
#' @param thres_prob numeric from 0 to 1, threshold of success' probability - if predicted
#' probability of dependent variable is higher than treshold, the event is counted as a success
#' @param resp_pred_vec numeric vector of transformed predictions (by inverse link function) of dependent variable
#' from data set; output of function resp_prediction_for_model()
#' @param Y_vec dependent variable Y which takes the values 0 and 1
#' 
#' @importFrom stats glm
#' 
#' @return integer, a number of True Positives
#' @keywords internal
.True_Positive <- function(thres_prob, resp_pred_vec, Y_vec){
  T_from_data <- which(Y_vec == 1)
  T_from_pred <-  which(resp_pred_vec >= thres_prob)
  
  sum(T_from_data %in% T_from_pred)
}
#' Elements of Confusion / Error Matrix - False Positive
#' 
#'\code{.False_Positive} returns a value of False Positives (FP) - number of bad predictions of 
#'failures by model (actual failure was predicted by model as success)
#' 
#' @param thres_prob numeric from 0 to 1, threshold of success' probability - if predicted
#' probability of dependent variable is higher than treshold, the event is counted as a success
#' @param resp_pred_vec numeric vector of transformed predictions (by inverse link function) of dependent variable
#' from data set; output of function resp_prediction_for_model()
#' @param Y_vec dependent variable Y which takes the values 0 and 1
#' 
#' @importFrom stats glm
#' 
#' @return integer, a number of False Positives
#' @keywords internal
.False_Positive <- function(thres_prob, resp_pred_vec, Y_vec){
  F_from_data <- which(Y_vec == 0)
  T_from_pred <-  which(resp_pred_vec >= thres_prob)
  
  sum(F_from_data %in% T_from_pred)
}
#' Elements of Confusion / Error Matrix - True Negative
#' 
#'  \code{.True_Negative} value of True Negative (TN) - number of good predictions of failures
#'  by model (actual failure was predicted by model as failure)
#' 
#' @param thres_prob numeric from 0 to 1, threshold of success' probability - if predicted
#' probability of dependent variable is higher than treshold, the event is counted as a success
#' @param resp_pred_vec numeric vector of transformed predictions (by inverse link function) of dependent variable
#' from data set; output of function resp_prediction_for_model()
#' @param Y_vec dependent variable Y which takes the values 0 and 1
#' 
#' @importFrom stats glm
#' 
#' @return integer, a number of True Negative
#' @keywords internal
.True_Negative <- function(thres_prob, resp_pred_vec, Y_vec){
  F_from_data <- which(Y_vec == 0)
  F_from_pred <-  which(resp_pred_vec < thres_prob)
  
  sum(F_from_data %in% F_from_pred)
}
#' Elements of Confusion / Error Matrix -False Negative
#' 
#'  \code{.False_Negative} value of False Negative (FN) - number of bad predictions of successes
#'  by model (actual success was predicted by model as failure)
#' 
#' @param thres_prob numeric from 0 to 1, threshold of success' probability - if predicted
#' probability of dependent variable is higher than treshold, the event is counted as a success
#' @param resp_pred_vec numeric vector of transformed predictions (by inverse link function) of dependent variable
#' from data set; output of function resp_prediction_for_model()
#' @param Y_vec dependent variable Y which takes the values 0 and 1
#' 
#' @importFrom stats glm
#' 
#' @return integer, a number of False Negatives
#' @keywords internal
.False_Negative <- function(thres_prob, resp_pred_vec, Y_vec){
  T_from_data <- which(Y_vec == 1)
  F_from_pred <-  which(resp_pred_vec < thres_prob)
  
  sum(T_from_data %in% F_from_pred)
}

#' Create the Confusion / Error Matrix
#'
#' function returns list which cointains Confussion / Error Matrix (i.e sum of: True Positives (TP), 
#' False Positives (FP), True Negative (TN), False Negative (FN)), Accuracy of model,
#' Sensitivity and Specificity.
#' 
#' @param thres_prob numeric from 0 to 1, threshold of success' probability - if predicted
#' probability of dependent variable is higher than treshold, the event is counted as a success
#' @param Y_vec dependent variable Y which takes the values 0 and 1
#' @param model1 a fitted object of class inheriting from "logisMod"
#' @param ... addiction objects of class" logisMod"; theirs ROC curves will be compare with the curve of 
#' model1 
#' 
#' @return list with Error Matrix, Accuracy of model, Sensitivity and Specificity for each
#' vector of transformed model's predictions (for each regression )
#' @examples
#' model1 <- createModels(citrus, nameBin ~ diameter + green + blue)
#' model2 <- createModels(citrus, nameBin ~ diameter + red + blue + green)
#' p <- 0.6
#' 
#' CreateErrorMatrixStats(p, citrus$nameBin, model1, model2)
#' 
#' @export
CreateErrorMatrixStats <- function(thres_prob, Y_vec, model1, ...){
  list_of_models <- list(model1, ...)
  list_of_response_prediction <- lapply(list_of_models, function(model) predictBasedData(model1, citrus))
  
  elementsErrorMatrix_list <- lapply(list_of_response_prediction, function(pred_vec){
    TP <- .True_Positive(thres_prob, pred_vec, Y_vec) 
    FN <- .False_Negative(thres_prob, pred_vec, Y_vec)
    FP <- .False_Positive(thres_prob, pred_vec, Y_vec)
    TN <- .True_Negative(thres_prob, pred_vec, Y_vec) 
    
    list("TP" = TP, "FN" = FN, "FP" = FP, "TN" = TN)
  })
  
  
  Stats_list <- lapply(elementsErrorMatrix_list, function(lst){
    ErrorMatrix <- data.frame("pred_T" = c(lst$TP, lst$FP),
                              "pred_N" = c(lst$FN, lst$TN))
    
    rownames(ErrorMatrix) <- c("actual_T", "actual_N")
    
    Accuracy <- (lst$TP + lst$TN) / (do.call(sum, lst))
    
    Sensitivity <- lst$TP / (lst$TP + lst$FP)
    
    Specificity  <- lst$TN / (lst$FP + lst$TN)
    
    list("ErrorMatrix" = ErrorMatrix, 
         "Accuracy" = Accuracy, 
         "Sensitivity" = Sensitivity,
         "Specificity" = Specificity)
  })
  
  names(Stats_list) <- paste0("model", 1:length(list_of_models))
  
  if(length(list_of_models) == 1) return(Stats_list[[1]])
  else{
    Stats_list_transpose <- list(lapply(Stats_list, function(lst) lst$ErrorMatrix),
                                 lapply(Stats_list, function(lst) lst$Accuracy),
                                 lapply(Stats_list, function(lst) lst$Sensitivity),
                                 lapply(Stats_list, function(lst) lst$Specificity))
    
    names(Stats_list_transpose) <- c("ErrorMatrix", "Accuracy", "Sensitivity", "Specificity")
    Stats_list_transpose
  }
  
}

#' Visualization of Confusion / Error Matrix
#'
#' function displays Confusion / Error Matrix.
#' 
#' @param thres_prob numeric from 0 to 1, threshold of success' probability - if predicted
#' probability of dependent variable is higher than treshold, the event is counted as a success
#' @param Y_vec dependent variable Y which takes the values 0 and 1
#' @param model a fitted object of class inheriting from "logisMod"
#' 
#' @import ggplot2
#' 
#' @return plot which represents Confusion / Error Matrix for model
#' @examples
#' model <- createModels(nameBin ~ diameter + green + blue, data = citrus)
#' p <- 0.6
#' 
#' visualErrorMatrix(p, citrus$nameBin, model)
#' 
#' @export
visualErrorMatrix <- function(thres_prob, Y_vec, model){
  EMStats <- CreateErrorMatrixStats(thres_prob, Y_vec, model)
  ErrorMatrix <- cbind(" " = rownames(EMStats$ErrorMatrix), as.data.table(EMStats$ErrorMatrix))
  meltedErrorMatrix <- melt(ErrorMatrix, id.vars = c(" "))
  
  ggplot(data =  meltedErrorMatrix, mapping = aes(x = variable, y = ` `)) +
    geom_tile(aes(fill = value), colour = "white") +
    geom_text(aes(label = value), vjust = 1) +
    scale_fill_gradient(low = "#DF9C75", high = "#00C1B2") +
    labs(x = " ") +
    scale_x_discrete(labels = c('Predicted Positive', 'Predicted Negative')) +
    scale_y_discrete(labels = c('Actual Negative', 'Actual Positive')) +
    theme_minimal() + 
    theme(legend.position = "none",
          line = element_blank())
  
}


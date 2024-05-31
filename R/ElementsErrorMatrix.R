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
#' @param resp_pred_vec numeric vector of transformed predictions (by inverse link function) of dependent variable
#' from data set; output of function resp_prediction_for_model()
#' @param Y_vec dependent variable Y which takes the values 0 and 1
#' 
#' @importFrom stats glm
#' 
#' @return list with Error Matrix, Accuracy of model, Sensitivity and Specificity.
#' @examples
#' mdl <- glm(nameBin ~ diameter + green + blue, data = citrus, family = binomial("logit"))
#' p <- 0.6
#' 
#' resp_prediction_vec <- resp_prediction_for_model(mdl)
#' CreateErrorMatrixStats(p, resp_prediction_vec, mdl$data$nameBin)
#' @export
CreateErrorMatrixStats <- function(thres_prob, resp_pred_vec, Y_vec){
  TP <- .True_Positive(thres_prob, resp_pred_vec, Y_vec) 
  FN <- .False_Negative(thres_prob, resp_pred_vec, Y_vec)
  FP <- .False_Positive(thres_prob, resp_pred_vec, Y_vec)
  TN <- .True_Negative(thres_prob, resp_pred_vec, Y_vec)
  
  ErrorMatrix <- data.table(" " = c("actual_T", "actual_N"),
                            "pred_T" = c(TP, FP),
                            "pred_N" = c(FN, TN))
  
  Accuracy <- (TP + TN) / (TP + TN + FP + FN)
  
  Sensitivity <- TP / (TP + FP)
  
  Specificity  <- TN / (FP + TN)
  
  list("ErrorMatrix" = ErrorMatrix, 
       "Accuracy" = Accuracy, 
       "Sensitivity" = Sensitivity,
       "Specificity" = Specificity)
}

#' Visualization of Confusion / Error Matrix
#'
#' function displays Confusion / Error Matrix.
#' 
#' @param thres_prob numeric from 0 to 1, threshold of success' probability - if predicted
#' probability of dependent variable is higher than treshold, the event is counted as a success
#' @param resp_pred_vec numeric vector of transformed predictions (by inverse link function) of dependent variable
#' from data set; output of function resp_prediction_for_model()
#' @param Y_vec dependent variable Y which takes the values 0 and 1
#' 
#' @import ggplot2
#' 
#' @return plot which represents Confusion / Error Matrix
#' @examples
#' mdl <- glm(nameBin ~ diameter + green + blue, data = citrus, family = binomial("logit"))
#' p <- 0.6
#' 
#' resp_prediction_vec <- resp_prediction_for_model(mdl)
#' visualErrorMatrix(p, resp_prediction_vec, mdl$data$nameBin)
#' @export
visualErrorMatrix <- function(thres_prob, resp_pred_vec, Y_vec){
  EMStats <- CreateErrorMatrixStats(thres_prob, resp_pred_vec, Y_vec)
  meltedEroorMatrix <- melt(EMStats$ErrorMatrix, id.vars = c(" "))
  
  ggplot(data =  meltedEroorMatrix, mapping = aes(x = variable, y = ` `)) +
    geom_tile(aes(fill = value), colour = "white") +
    geom_text(aes(label = sprintf("%1.0f", value)), vjust = 1) +
    scale_fill_gradient(low = "#DF9C75", high = "#00C1B2") +
    labs(x = " ") +
    scale_x_discrete(labels = c('Predicted Positive', 'Predicted Negative')) +
    scale_y_discrete(labels = c('Actual Negative', 'Actual Positive')) +
    theme_minimal() + 
    theme(legend.position = "none",
          line = element_blank())
}

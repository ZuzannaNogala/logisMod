#' Elements of Confusion / Error Matrix
#'
#' functions return elements of Error Matrix i.e sum of: True Positives (TP), False Positives (FP),
#' True Negative (TN), False Negative (FN)
#' 
#' @param thres_prob numeric from 0 to 1, threshold of success' probability - if predicted
#' probability of dependent variable is higher than treshold, the event is counted as a success
#' @param resp_pred_vec numeric vector of transformed predictions (by inverse link function) of dependent variable
#' from data set; output of function resp_prediction_for_model()
#' @param Y_vec dependent variable Y which takes the values 0 and 1
#' @name ElementsOfErrorMatrix
#' @importFrom stats glm
#' 
#' @return integer, sum of TP, FP, TN and FN
#' @examples
#' mdl <- glm(nameBin ~ diameter + green + blue, data = citrus, family = binomial("logit"))
#' p <- 0.6
#' 
#' resp_prediction_for_model <- resp_prediction_for_model(mdl)
#' 
#' True_Negative(thres_prob = p, resp_prediction_for_model, citrus$nameBin)
#' True_Positive(thres_prob = p, resp_prediction_for_model, citrus$nameBin)
#' False_Negative(thres_prob = p, resp_prediction_for_model, citrus$nameBin)
#' False_Positive(thres_prob = p, resp_prediction_for_model, citrus$nameBin)
#' @export
True_Positive <- function(thres_prob, resp_pred_vec, Y_vec){
  T_from_data <- which(Y_vec == 1)
  T_from_pred <-  which(resp_pred_vec >= thres_prob)
  
  sum(T_from_data %in% T_from_pred)
}
#' @rdname ElementsOfErrorMatrix
#' @export
False_Positive <- function(thres_prob, resp_pred_vec, Y_vec){
  F_from_data <- which(Y_vec == 0)
  T_from_pred <-  which(resp_pred_vec >= thres_prob)
  
  sum(F_from_data %in% T_from_pred)
}
#' @rdname ElementsOfErrorMatrix
#' @export
True_Negative <- function(thres_prob, resp_pred_vec, Y_vec){
  F_from_data <- which(Y_vec == 0)
  F_from_pred <-  which(resp_pred_vec < thres_prob)
  
  sum(F_from_data %in% F_from_pred)
}
#' @rdname ElementsOfErrorMatrix
#' @export
False_Negative <- function(thres_prob, resp_pred_vec, Y_vec){
  T_from_data <- which(Y_vec == 1)
  F_from_pred <-  which(resp_pred_vec < thres_prob)
  
  sum(T_from_data %in% F_from_pred)
}

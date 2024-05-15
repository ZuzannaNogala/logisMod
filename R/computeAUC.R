#' Computing AUC 
#'
#' function finds the Area Under the Receiver Operating Characterisric
#' curve (AUC). The area under curve lies between 0 to 1. The greater value of AUC
#' denotes better model performance. If AUC equals 0.5, the model is random 
#' classiffier. It means the model randomly decides that if event is success or not.
#' 
#' @param model glm.object, logistic model
#' @param strNameY characteristic, name of dependent variable which takes values
#' 0 and 1
#' 
#' @return numeric from 0 to 1, represents area under ROC curve
#' 
#' @importFrom pROC roc
#' 
#' @examples
#' model <- glm(nameBin ~ diameter + green + blue, data = citrus, family = binomial("logit"))
#' computeAUC(model, "nameBin")
#' @export
computeAUC <- function(model, strNameY){
  roc_model <- pROC::roc(model$data[, get(strNameY)], model$fitted.values)
  roc_model$auc
}

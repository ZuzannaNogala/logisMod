#' Computing AUC 
#'
#' function finds the Area Under the Receiver Operating Characterisric
#' curve (AUC). The area under curve lies between 0 to 1. The greater value of AUC
#' denotes better model performance. If AUC equals 0.5, the model is random 
#' classiffier. It means the model randomly decides that if event is success or not.
#' 
#' @param strNameY characteristic, name of dependent variable which takes values
#' 0 and 1
#' @param model1 a fitted object of class inheriting from "logisMod"
#' @param ... addiction object of class "logisMod"; theirs AUC will be compare with the AUC for the 
#' model1 
#' 
#' @return numeric from 0 to 1, represents area under ROC curve for each model
#' 
#' @importFrom pROC roc
#' @importFrom utils head
#' 
#' @examples
#' model1 <- createModels(citrus, nameBin ~ diameter + blue + red)
#' model2 <- createModels(citrus, nameBin ~ weight + green + red)
#' 
#' computeAUC("nameBin", model1, model2)
#' 
#' @export
computeAUC <- function(strNameY, model1, ...){
  list_of_models <- list(model1, ...)
  names(list_of_models) <- paste0("model", 1:length(list_of_models))
  
  auc_for_models <- lapply(list_of_models, function(model){
    roc_stats <- pROC::roc(model$data[, get(strNameY)], model$fitted.values)
    head(roc_stats$auc)
  })
  
  data.table("model" = names(list_of_models), "AUC" = auc_for_models)
}
#' Computing AUC 
#'
#' function finds the Area Under the Receiver Operating Characteristic
#' curve (AUC). The area under curve lies between 0 to 1. The greater value of AUC
#' denotes better model performance. If AUC equals 0.5, the model is random 
#' classifier. It means the model randomly decides that if event is success or not.
#' 
#' @param strNameY characteristic, name of dependent variable which takes values
#' 0 and 1
#' @param data a data.table or data.frame object, from which data for models is taken 
#' @param models a fitted object or objects' list of class inheriting from "logisMod"
#' 
#' @return a data.table object with area under ROC curve for each model
#' 
#' @importFrom pROC roc
#' @importFrom utils head
#' 
#' @examples
#' models <- createModels(citrus, nameBin ~ diameter +blue + red, nameBin ~ weight + green)
#' model <- createModels(citrus, nameBin ~ diameter + blue + red)
#' 
#' computeAUC("nameBin", citrus, models)
#' computeAUC("nameBin", citrus, model)
#' 
#' @export
computeAUC <- function(strNameY, data, models){
  if(!(strNameY %in% names(data))) 
    stop(paste0("In data ", strNameY, " doesn't exist! Please check name again."))
  data <- as.data.table(data)

  auc_for_models <- lapply(models, function(model){
    roc_stats <- pROC::roc(data[, get(strNameY)], model$fitted.values)
    head(roc_stats$auc)
  })
  
  data.table("model" = names(models), "AUC" = auc_for_models)
}

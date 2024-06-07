#' Prediction of new values
#' 
#' @param object a fitted object of class inheriting from "logisMod'
#' @param pred_data data frame in which to look for variables with which to predicts
#' 
#' @importFrom stats predict.glm
#' 
#' @examples
#' library(data.table)
#' model <- createModels(citrus, nameBin ~ red)
#' predData <- citrus[["red"]]
#' newData <- list()
#' newData[["red"]] <- seq(min(predData), max(predData), 0.1)
#' newData <- as.data.table(newData)
#' 
#' predictBasedData(model[[1]], newData)
#' 
#' @details
#' This function is fixed form of \code{stats::predict.glm}. For more details or 
#' different version see \link[stats]{predict.glm}.
#' 
#' @export
predictBasedData <- function(object, pred_data){
  stats::predict.glm(object, newdata = pred_data, type = "response")
}

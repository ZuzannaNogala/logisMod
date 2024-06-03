#' Prediction of new values
#' 
#' @param object a fitted object of class inheriting from "logisMod'
#' @param newdata data frame in which to look for variables with which to predict
#' @param ... other parameters
#' 
#' @importFrom stats predict.glm
#' 
#' @examples
#' library(data.table)
#' model <- logisMod(nameBin ~ red, citrus)
#' predData <- citrus[["red"]]
#' newData <- list()
#' newData[["red"]] <- seq(min(predData), max(predData), 0.1)
#' newData <- as.data.table(newData)
#' 
#' predict(model, newData)
#' 
#' @details
#' This function is fixed form of \code{stats::predict.glm}. For more details or 
#' different version see \link[stats]{predict.glm}.
#' 
#' @export
predict.logisMod <- function(object, newdata, ...){
  stats::predict.glm(object, newdata = newData, type = "response")
}
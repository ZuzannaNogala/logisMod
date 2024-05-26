resPlot <- function(x, ...){
  UseMethod("resPlot")
}

#' Plot of predicted probabilities
#' 
#' Function makes model for given data table and its variables, one is name 
#' (character) of dependent variable, other - predictor's name (character). Then
#' plots predicted probabilities in comparison to true values. Default version 
#' when we don't need model but want to plot predicted probabilities.
#' 
#' 
#' @param x data.frame or data.table, from which data for model is taken
#' @param ... character, names of dependent variable and predictor
#' @return plot of predicted probabilities for one dependent variable
#' 
#' @importFrom stats glm
#' @importFrom stats predict.glm
#' @import data.table
#' 
#' @examples
#' resPlot(citrus, "nameBin", "diameter")
#' @export
resPlot.default <- function(x, ...){
  varNames <- list(...)
  strNameY <- varNames[[1]]
  strNameX <- varNames[[2]]
  
  formula <- .getModelFormula(strNameY, strNameX)
  model <- stats::glm(formula, x, family = "binomial")
  
  predData <- x[[strNameX]]
  newData <- list()
  newData[[strNameX]] <- seq(min(predData), max(predData), 0.1)
  newData <- as.data.table(newData)
  
  resValues <- stats::predict.glm(model, newdata = newData, type = "response")
  
  plotData <- newData[, resValues := resValues]
  
  ggplot(x, aes(x = get(strNameX), y = get(strNameY))) +
    geom_point() +
    geom_line(plotData, mapping = aes(x = get(strNameX), y = resValues), color = "#ed583f") +
    labs(x = strNameX,
         y = strNameY,
         title = paste0("Probabilities of ", strNameY, " as function of ", strNameX))
}

#' Plot of predicted probabilities
#' 
#' Function plots predicted probabilities in comparison to true values. For use
#' when we created model and want to plot it.
#' 
#' @param x model of class LogisMod
#' @return plot of predicted probabilities for one dependent variable
#' 
#' @importFrom stats glm
#' @importFrom stats predict.glm
#' @importFrom stringr str_split
#' @importFrom stringr str_trim
#' @import data.table
#' 
#' @examples
#' model <- buildModel(nameBin ~ red, citrus)
#' resPlot(model)
#' @export
resPlot.LogisMod <- function(x, ...){
  data <- x$data
  formula <- x$formula
  formulaTXT <- deparse(formula)
  varNames <- stringr::str_split(formulaTXT, "~", simplify = TRUE)
  
  if(ncol(varNames) > 2){
    stop("You can't plot more than two variables!")
  }
  
  strNameY <- stringr::str_trim(varNames[1])
  strNameX <- stringr::str_trim(varNames[2])
  
  predData <- data[[strNameX]]
  newData <- list()
  newData[[strNameX]] <- seq(min(predData), max(predData), 0.1)
  newData <- as.data.table(newData)
  
  resValues <- stats::predict.glm(model, newdata = newData, type = "response")
  
  plotData <- newData[, resValues := resValues]
  
  ggplot(data, aes(x = get(strNameX), y = get(strNameY))) +
    geom_point() +
    geom_line(plotData, mapping = aes(x = get(strNameX), y = resValues), color = "#ed583f") +
    labs(x = strNameX,
         y = strNameY,
         title = paste0("Probabilities of ", strNameY, " as function of ", strNameX))
}

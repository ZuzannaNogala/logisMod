#' Generic function for plotting probabilities
#' 
#' @param x object of proper class
#' @param ... other parameters
#' 
#' @export
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
#' @param strNameY character, name of dependent variable Y which takes the values 0 and 1
#' @param strNameX character, predictor's name, must be column of \code{data}
#' @param ... other parameters
#' @return plot of predicted probabilities for one dependent variable
#' 
#' @details
#' This method uses \code{stats::glm(formula, x, family = "binomial")}, please
#' take care of proper type of the arguments.
#'
#' @importFrom stats glm
#' @importFrom stats predict.glm
#' @import data.table
#' 
#' @examples
#' resPlot(citrus, "nameBin", "diameter")
#' @export
resPlot.default <- function(x, strNameY, strNameX, ...){
  formula <- .getModelFormula(strNameY, strNameX)
  model <- stats::glm(formula, x, family = "binomial")
  
  predData <- x[[strNameX]]
  newData <- list()
  newData[[strNameX]] <- seq(min(predData), max(predData), 0.1)
  newData <- as.data.table(newData)
  
  resValues <- stats::predict.glm(model, newdata = newData, type = "response")
  
  plotData <- newData[, resValues := resValues]
  
  .resPlotInside(x, plotData, strNameY, strNameX, resValues)
}

#' Plot of predicted probabilities
#' 
#' Function plots predicted probabilities in comparison to true values. For use
#' when we created model and want to plot it.
#' 
#' @param x model of class logisMod
#' @param ... other parameters
#' @return plot of predicted probabilities for one dependent variable
#' 
#' @details
#' This method uses \code{stats::glm(formula, x, family = "binomial")}, please
#' take care of proper type of the arguments.
#' 
#' @importFrom stats glm
#' @importFrom stats predict.glm
#' @importFrom stringr str_split
#' @importFrom stringr str_trim
#' @import data.table
#' 
#' @examples
#' model <- logisMod(nameBin ~ red, citrus)
#' resPlot(model)
#' @export
resPlot.logisMod <- function(x, ...){
  data <- x$data
  formula <- x$formula
  formulaTXT <- deparse(formula)
  varNames <- stringr::str_split(formulaTXT, "~", simplify = TRUE)
  
  if(grepl("\\+", varNames[2])){
    stop("You can't plot more than two variables!")
  }
  
  strNameY <- stringr::str_trim(varNames[1])
  strNameX <- stringr::str_trim(varNames[2])
  
  predData <- data[[strNameX]]
  newData <- list()
  newData[[strNameX]] <- seq(min(predData), max(predData), 0.1)
  newData <- as.data.table(newData)
  
  resValues <- stats::predict.glm(x, newdata = newData, type = "response")
  
  plotData <- newData[, resValues := resValues]
  
  .resPlotInside(data, plotData, strNameY, strNameX, resValues)
}

#' Plotting part of \code{resPlot}
#' 
#' Responsilbe for creating plot inside of \code{resPlot} function.
#' 
#' @param data data.frame or data.table, from which data for model is taken
#' @param plotData data.table with prediction and X variable
#' @param strNameY character, name of dependent variable Y which takes the values 0 and 1
#' @param strNameX character, predictor's name, must be column of \code{data}
#' @param resValues numeric vector of predicted probabilities
#' 
#' @return plot of probabilities
#' 
#' @details
#' \code{resPlot} makes data appropriate for this method.
#' 
#' @import ggplot2
#' 
#' @keywords internal
.resPlotInside <- function(data, plotData, strNameY, strNameX, resValues){
  ggplot(data, aes(x = get(strNameX), y = get(strNameY))) +
    geom_point() +
    geom_line(plotData, mapping = aes(x = get(strNameX), y = resValues), color = "#ed583f") +
    labs(x = strNameX,
         y = strNameY,
         title = paste0("Probabilities of ", strNameY, " as function of ", strNameX))
}

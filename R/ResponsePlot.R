#' Plot of predicted probabilities
#' 
#' Function makes model for given data table and its variables, one is name 
#' (character) of dependent variable, other - predictor's name (character). Then
#' plots predicted probabilities in comparison to true values.
#' 
#' 
#' @param data data.frame or data.table, from which data for model is taken
#' @param strNameY character, name of dependent variable Y which takes the values 0 and 1
#' @param strNameX character, predictor's name, must be column of \code{data}
#' @return plot of predicted probabilities for one dependent variable
#' 
#' @importFrom stats glm
#' @importFrom stats predict.glm
#' @import data.table
#' 
#' @examples
#' resPlot(citrus, "nameBin", "diameter")
#' @export
resPlot <- function(data, strNameY, strNameX){
  formula <- .getModelFormula(strNameY, strNameX)
  model <- stats::glm(formula, data, family = "binomial")
  
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

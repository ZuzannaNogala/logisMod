#' Model Building Function
#' 
#' Function makes model for given data table and its variables, one is name 
#' (character) of dependent variable, other - predictors' names (as character
#' vector).
#' 
#' @param data data.frame or data.table, from which data for model is taken
#' @param strNameY character, name of dependent variable Y which takes the values 0 and 1
#' @param pred character vector, vector of predictors' names, 
#' must be columns of \code{data}
#' @return summary of model
#' 
#' @importFrom stats glm
#' 
#' @examples
#' possiblePred <- colnames(citrus)[2:6]
#' buildModel(citrus, "nameBin", possiblePred)
#' @export
buildModel <- function(data, strNameY, pred){
  formula <- .getModelFormula(strNameY, pred)
  model <- stats::glm(formula, data, family = "binomial")
  
  summary(model)
}

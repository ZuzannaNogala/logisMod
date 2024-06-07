#' All combinations of a vector
#' 
#' Function gives us list of all possible combinations of a given vector
#' 
#' @param vec vector of values
#' @return list of combinations
#' 
#' @importFrom utils combn
#' 
#' @keywords internal
.getAllCombination <- function(vec){
  numRep <- length(vec)
  
  result <- lapply(0:numRep, function(iter){
    comb <- utils::combn(vec, iter)
    
    lst <- apply(comb, 2, function(item){
      list(item)
    })
    
    unlist(lst, F)
  })
  
  unlist(result, F)
}

#' Formula for making model
#' 
#' Function gives us formula to create a model. Created from name of dependent
#' variable and list of names of possible predictors.
#' 
#' @param strNameY character, name of dependent variable Y which takes the values 0 and 1
#' @param possPred character vector, vector of possible predictors' names, 
#' must be columns of data table from which model is creating
#' @param intercept default TRUE, set FALSE if you do not want to include intercept into model
#' @return formula for model
#' 
#' @importFrom stats as.formula
#' 
#' @keywords internal
.getModelFormula <- function(strNameY, pred, intercept = TRUE){
  
  if(length(pred) == 0){
    formulaTXT <- paste0(strNameY, "~ 1")
  }else if(intercept){
    formulaTXT <- paste0(strNameY, "~", paste(pred, collapse = "+"))
  }else{
    formulaTXT <- paste0(strNameY, "~ -1 + ", paste(pred, collapse = "+"))
  }
  
  stats::as.formula(formulaTXT)
}

#' AIC/BIC for all possible models 
#' 
#' This function let us count values of AIC and BIC for models created by
#' formulas based on all possible predictors' combinations.
#' 
#' @param data data.frame or data.table, from which data for model is taken
#' @param strNameY character, name of dependent variable Y which takes the values 0 and 1
#' @param possPred character vector, vector of possible predictors' names, 
#' must be columns of \code{data}
#' @return data.table containing all possible formulas and values of AIC/BIC 
#' for models based on them
#' 
#' @import data.table
#' @importFrom stats glm
#' @importFrom stats AIC
#' @importFrom stats BIC
#' 
#' @examples
#' possiblePred <- colnames(citrus)[2:6]
#' countInfCrit(citrus, "nameBin", possiblePred)
#' @export
countInfCrit <- function(data, strNameY, possPred){
  
  if(length(possPred) > 8){
    stop("Too much models for comparison.")
  }
  
  if(!(strNameY %in% names(data)) || !all(possPred %in% names(data))){
    stop("One or more of given variables doesn't exist in dataset! Please check names again.")
  }
  
  possPredComb <- .getAllCombination(possPred)
  
  result <- lapply(possPredComb, function(item){
    values <- list()
    formula <- .getModelFormula(strNameY, item)
    model <- stats::glm(formula, data, family = "binomial")
    
    values[["formula"]] <- deparse(formula)
    values[["AIC"]] <- stats::AIC(model)
    values[["BIC"]] <- stats::BIC(model)
    values
  })
  
  data.table::rbindlist(result)
}


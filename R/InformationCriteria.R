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

.getModelFormula <- function(strNameY, pred){
  
  if(length(pred) == 0){
    formulaTXT <- paste0(strNameY, "~ 1")
  }else{
    formulaTXT <- paste0(strNameY, "~", paste(pred, collapse = "+"))
  }
  
  as.formula(formulaTXT)
}

countInfCrit <- function(data = citrus, strNameY = "nameBin", possPred){
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



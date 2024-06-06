#' Model Building Function
#' 
#' Function makes models based on given formulas, created using variable's names
#' from analised data table.
#' 
#' @param data data.frame or data.table, from which data for model is taken
#' @param ...  objects of class "formula", a symbolic description of the 
#' model to be fitted, same as in \link[stats]{glm}
#' @return list of logistic regression models, with class glm and also new logisMod class
#' 
#' @importFrom stats glm
#' 
#' @examples
#' createModels(citrus, nameBin ~ diameter)
#' 
#' # even when an error occurs it is possible to continue, but we will get 
#' # a warning about it
#' createModels(citrus, nameBin ~ diameter, red ~ diameter, nameBin ~ weight)
#' @export
createModels <- function(data, ...){
  formulas <- list(...)
  
  models <- lapply(formulas, function(form){
    tryCatch({
      model <- stats::glm(form, data, family = "binomial")
      class(model) <- c("logisMod", class(model))
      result <- list(model)
      names(result) <- deparse(form)
      result
    }, error = function(e){
      warning("An error occured: ", e, "Check model formulas.")
      
      result <- list(NA)
      names(result) <- deparse(form)
      result
    })
  })
  
  unlist(models, F, T)
}

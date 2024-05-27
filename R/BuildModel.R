#' Model Building Function
#' 
#' Function makes model based on given formula, created using variable's names
#' from analised data table.
#' 
#' @param formula an object of class "formula", a symbolic description of the 
#' model to be fitted, same as in \link[stats]{glm}
#' @param data data.frame or data.table, from which data for model is taken
#' @return logistic model of class glm and also new logisMod class
#' 
#' @importFrom stats glm
#' 
#' @examples
#' logisMod(nameBin ~ red, citrus)
#' @export
logisMod <- function(formula, data){
  model <- stats::glm(formula, data, family = "binomial")
  
  class(model) <- c(class(model), "logisMod")
  
  model
}

#' Transformation prediction by inverse link function of dependent variable
#'
#' function returns transformed prediction of dependent variable based on model and values
#' of predictors 
#' 
#' @param model object glm, logistic model
#' @return numeric vetor
#' @import data.table
#' @importFrom stats glm
#' @importFrom stats predict.glm
#' @examples
#' mdl <- glm(nameBin ~ diameter + green + blue, data = citrus, family = binomial("logit"))
#' summary(mdl)
#' head(resp_prediction_for_model(mdl))
#' @export
resp_prediction_for_model <- function(model){
  data <- as.data.table(model$data)
  names_preds <- names(model$coefficients[-1])
  data_pred <- data[, lapply(1:length(names_preds), function(i) get(names_preds[i]))]
  names(data_pred) <- names_preds
  
  predict.glm(model, data_pred, type = "response")
}
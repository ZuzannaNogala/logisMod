#' Deviance test for two logistic models
#'
#' function returns deviance test summary to verify null hypothesis H0: predictors
#' from set A is insignificant vs alternative H1: significant predictor exists in set A (full model)
#' 
#' @param models fitted objects' list of class inheriting from "logisMod". One model is from null 
#' hypothesis (models with less predictors are supported) and second one is alternative 
#' (models with more predictors are supported)
#' @param alpha numeric, significance level of test, value from 0 to 1
#' 
#' @importFrom stats pchisq
#' @importFrom stats qchisq
#' @importFrom stats logLik
#' @importFrom utils head
#' 
#' @return list of testing predictors' names, value of deviance statistic, 
#' degrees of freedom, critical value of deviance statistic and p-value. 
#' 
#' @examples
#' models <- createModels(citrus, nameBin ~ diameter + blue + weight, nameBin ~ weight)
#'                  
#' deviance_test(models, alpha = 0.1)
#' 
#' @export
deviance_test <- function(models, alpha = 0.05){
  coeffs <- lapply(models, function(model){
    names_coeffs <- setdiff(names(model$coefficients), "(Intercept)")
    list("names_coeffs" = names_coeffs)
  })
  names(coeffs) <- NULL
  
  if(length(coeffs) != 2) stop("models has to be 2-element list of models.")
  if(length(coeffs[[1]]$names_coeffs) == length(coeffs[[2]]$names_coeffs)) 
    stop("models cannot have equal amount of predictors.")
  
  num_model_H0 <- which.min(lapply(coeffs, function(lst) length(lst$names_coeffs)))
  model_H0 <- models[[num_model_H0]]
  model_H1 <- models[[setdiff(1:2, num_model_H0)]]
  
  if(sum(!(setdiff(names(model_H0$coefficients), "(Intercept)")
    %in% 
    setdiff(names(model_H1$coefficients), "(Intercept)"))) > 0) 
    stop("Change model from null hipothesis, its predictors has to be in model from alternative.")
  
  deviance_stat <- head(- 2 * (logLik(model_H0) - logLik(model_H1)))
  df <- length(model_H1$coefficients) - length(model_H0$coefficients)
  p_val <- 1 - pchisq(deviance_stat, df)
  
  params_names <- union(names(model_H0$coefficients), names(model_H1$coefficients))
  testing_params_names <- setdiff(params_names, names(model_H0$coefficients))
  
  list("Tested_predictors" = testing_params_names, 
       "Deviance_statistic" = round(deviance_stat, digits = 3), 
       "df" = df,
       "Critical_value" = qchisq(1 - alpha, df),
       "p_value" = p_val)
}

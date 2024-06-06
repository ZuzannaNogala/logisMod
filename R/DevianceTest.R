#' Deviance test for two logistic models
#'
#' function returns deviance test summary to verify null hypothesis H0: predictors
#' from set A is insignificant vs alternative H1: significant predictor exists in set A (full model)
#' 
#' @param model_H0a fitted object of class inheriting from "logisMod" from null 
#' hypothesis (models with less predictors are supported) 
#' @param model_H1a fitted object of class inheriting from "logisMod" from 
#' alternative hypothesis (models with more predictors are supported)
#' @param alpha numeric, significance level of test
#' 
#' @importFrom stats pchisq
#' @importFrom stats qchisq
#' @importFrom stats logLik
#' @importFrom utils head
#' 
#' @return list of names of testing predictors, value of deviance statistic, 
#' degrees of freedom, critical value of deviance statistic and p value. 
#' 
#' @examples
#' model_full <- createModels(citrus, nameBin ~ diameter + green + blue + weight)
#' model_unfull <- createModels(citrus, nameBin ~ weight + green)
#'                  
#' deviance_test(model_H0 = model_unfull, model_H1 = model_full, alpha = 0.1)
#' 
#' @export
deviance_test <- function(model_H0, model_H1, alpha = 0.05){
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



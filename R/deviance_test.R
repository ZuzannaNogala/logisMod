#' Deviance test for two logistic models
#'
#' function returns deviance test summary to verify null hypothesis H0: predictors
#' from set A is insignificant vs alternative H1: significant predictor exists in set A (full model)
#' 
#' @param model_H0 glm object, logistic model from null hypothesis (models with less 
#' predictors are supported) 
#' @param model_H1 glm object, logistic model from alternative hypothesis (models with more 
#' predictors are supported)
#' @param alpha numeric,significance level of test
#' @return list of names of testing predictors, value of deviance statistic, 
#' degrees of freedom, critical value of deviance statistic and p value. 
#' @examples
#'mdl_full <- glm(nameBin ~ diameter + green + blue + weight , data = citrus, family = binomial("logit"))
#'mdl_unfull <- glm(nameBin ~ diameter + weight + green, data = citrus, family = binomial("logit"))
#'deviance_test(model_H0 = mdl_unfull, model_H1 = mdl_full, alpha = 0.1)
#' @export
deviance_test <- function(model_H0, model_H1, alpha = 0.05){
  deviance_stat <- model_H0$deviance - model_H1$deviance
  df <- length(model_H1$coefficients) - length(model_H0$coefficients)
  p_val <- 1 - pchisq(deviance_stat, df)
  num_testing_params <- setdiff(1:length(model_H1$coefficients), 
                                which(names(model_H0$coefficients) %in% names(model_H1$coefficients)))
  
  list("Tested_predictors" = c(names(model_H1$coefficients)[num_testing_params]), 
       "Deviance_statistic" = deviance_stat, 
       "df" = df,
       "Critical_value" = qchisq(1 - alpha, df),
       "p_value" = p_val)
}

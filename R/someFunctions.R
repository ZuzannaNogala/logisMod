#' sse of scaled data for linear model
#'
#' function scales given data, then counts sse using that data and given 
#' vector of observations
#' 
#' @param observations numeric, vector of dependent variable
#' @param data numeric, plan matrix for model
#' @param parameters numeric, beta values
#' @return numeric, sse of scaled data
#' @example firstCaseExample.R
#' @export
firstCase <- function(observations, data, parameters){
  scaledData <- scale(data)
  
 sum((observations - scaledData %*% parameters)^2)
}

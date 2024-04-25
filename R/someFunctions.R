#' sse of scaled data for linear model
#'
#' function scales given data, then counts sse using that data and given 
#' vector of observations
#' 
#' @param observations numeric, vector of dependent variable
#' @param data numeric, plan matrix for model
#' @param parameters numeric, beta values
#' @return numeric, sse of scaled data
#' @examples
#' n <- 88
#' planMatrix <- matrix(rnorm(3*n), nrow = n, ncol = 3)
#' beta <- c(1, 2, 1)
#' Y <- planMatrix %*% beta + rnorm(n, sd = 0.01)
#' firstCase(Y, planMatrix, beta)
#' @export
firstCase <- function(observations, data, parameters){
  scaledData <- scale(data)
  
 sum((observations - scaledData %*% parameters)^2)
}

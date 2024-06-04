#' Splitting function
#' 
#' \code{.splitSet} gives partition of shuffled indexes of data table, to use 
#' when subsets of data are needed
#' 
#' @param data data.frame or data.table we want to split
#' @param K integer, number if partitions
#' 
#' @return list of groups of indexes for creating subsets of given data
#' 
#' @keywords internal
.splitSet <- function(data, K){
  rowNum <- nrow(data)
  
  if(rowNum < K){
    stop(paste0("Number of rows: ", rowNum, " is smaller than ", K))
  }else if(rowNum %% K != 0){
    warning(paste0("Number of rows: ", rowNum, " is not divided by: ", K, 
                   ". Your data isn't evenly divided."))
  }
  
  indexes <- 1:rowNum
  shuffledRows <- sample(indexes, rowNum)
  
  partitionIdx <- split(indexes, cut(indexes, K))
  names(partitionIdx) <- NULL
  
  splittedSet <- lapply(partitionIdx, function(idx){
    shuffledRows[idx]
  })
  
  splittedSet
}

#' K-Fold Cross Validation
#' 
#' Implementation of K-Fold Cross Validation technique for logistic regression. 
#' In this technique, the parameter K refers to the number of different subsets 
#' that the given data set is to be split into. Further, K-1 subsets are used 
#' to train the model and the left out subsets are used as a validation set. For
#' each subset we count accuracy, then we get accuracy score by applying mean 
#' to all the accuracies received for all folds.
#' 
#' @param data data.table, set of data we want apply logistic regresion to
#' @param K integer, number of folds
#' @param threshold numeric from 0 to 1, threshold of success' probability - 
#' if predicted probability of dependent variable is higher than treshold, the 
#' event is counted as a success, if vector then for each accuracy score is computed
#' @param strNameY character, name of dependent variable Y which takes the values 0 and 1
#' @param pred character vector, vector of predictors' names, 
#' must be columns of \code{data}
#' 
#' @return data.table with choosen threshold and its accuracy score
#' 
#' @import data.table
#' @importFrom stats glm
#' 
#' @examples
#' kFoldCV(citrus, 4, 0.5, "nameBin", c("red", "blue"))
#' kFoldCV(citrus, 4, c(0.1, 0.5), "nameBin", c("red", "blue"))
#' 
#' @export
kFoldCV <- function(data, K, threshold, strNameY, pred){
  
  if(!is.numeric(threshold)){
    stop("Threshold must be numeric!")
  }
  
  if(!all(between(threshold, 0, 1))){
    stop("Threshold must be value from (0,1)!")
  }
  
  rowNum <- nrow(data)
  formula <- .getModelFormula(strNameY, pred)
  modelData <- data[, .SD, .SDcols = c(strNameY, pred)]
  
  result <- lapply(threshold, function(t){
    splittedIdx <- .splitSet(modelData, K)
    
    calc <- lapply(splittedIdx, function(idx){
      test <- data[idx]
      training <- data[-idx]
      sizeTest <- length(idx)
      
      trueValues <- test[[strNameY]]
      
      model <- stats::glm(formula, training, family = "binomial")
      prediction <- predict.glm(model, test, type = "response")
      prediction <- ifelse(prediction > t, 1, 0)
      
      acc <- sum(trueValues == prediction) / sizeTest
      acc
    })
    
    list("threshold" = t, 
         "accScore" = mean(unlist(calc)))
  })
  
  rbindlist(result)
}

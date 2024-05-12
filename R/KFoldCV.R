.splitSet <- function(data, K){
  rowNum <- nrow(data)
  
  if(rowNum < K){
    stop(paste0("Number of rows: ", rowNum, " is samller than ", K))
  }else if(rowNum %% K != 0){
    warning(paste0("Number of rows: ", rowNum, " is not divided by: ", K, 
                   ". Your data won't be evenly divided."))
  }
  
  indexes <- 1:rowNum
  shuffledRows <- sample(indexes, rowNum)
  
  partitionIdx <- split(indexes, cut(indexes, K))
  
  splittedSet <- lapply(partitionIdx, function(idx){
    shuffledRows[idx]
  })
  
  splittedSet
}



#' Finding points on the ROC curve corresponding to a chosen threshold
#'
#' \code{.findPointsOnTheROC} computes a point which represents the False 
#' Positive Rate (FPR) and the True Positive Rate (TPR, sensitivity) at each threshold
#' setting
#' 
#' @param threshold_sequence sequence of numeric values from 0 to 1, thresholds 
#' of success' probability - if predicted probability of dependent variable is higher than threshold, the 
#' event is counted as a success
#' @param model glm.object, logistic model build from data, which is data.table 
#' object
#' @param strNameY characteristic, name of dependent variable which takes values
#' 0 and 1
#' 
#' @return a list of pairs represents FPR and TPR for each threshold probability
#' 
#' @import data.table
#'
#' @keywords internal
.findPointsOnTheROC <- function(threshold_sequence, model, strNameY){
  if(!(strNameY %in% names(model$data))) 
    stop(paste0("In data ", strNameY, " doesn't exist! Please check name again."))
  if(sum(threshold_sequence > 1) != 0) stop("threshold_sequence is from 0 to 1!")
  
  data <- as.data.table(model$data)
  
  coefs <- model$coefficients
  preds <- names(coefs)[-1]
  Fails <- data[get(strNameY) == 0, ..preds]
  Successes <- data[get(strNameY) == 1, ..preds]
  
  resp_vec <- predictBasedData(model, data)
  
  FP <- sapply(threshold_sequence, 
               function(p) .False_Positive(p, resp_vec, data[, get(strNameY)]))
  TP <- sapply(threshold_sequence, 
               function(p) .True_Positive(p, resp_vec, data[, get(strNameY)]))
  
  
  FPR <- FP / nrow(Fails)
  TPR <- TP / nrow(Successes)
  
  points_list <- lapply(1:length(TPR), function(i) c(FPR[i], TPR[i]))
  return(points_list)
}

#' The ROC curve visualization
#' 
#' A receiver operating characteristic curve (ROC curve) is a graphical plot which
#' illustrates the performance of a binary classifier model for different choices of 
#' threshold of success' probability. Each point on curve represents False Positive 
#' Rate (FPR) against True Positive Rate (TPR) at each threshold setting. 
#' 
#' @param threshold_sequence sequence of numeric values from 0 to 1, thresholds 
#' of success' probability - if predicted probability of dependent variable is higher than threshold, the 
#' event is counted as a success
#' @param strNameY characteristic, name of dependent variable which takes values
#' 0 and 1
#' @param models a fitted object or objects' list of class inheriting from "logisMod"
#' 
#' @import ggplot2
#' @import data.table
#' 
#' @returns one plot represents ROC curves for each model
#' 
#' @examples
#' model1 <- createModels(citrus, nameBin ~ diameter + blue + red)
#' models <- createModels(citrus, nameBin ~ diameter + blue + red, nameBin ~ diameter, nameBin ~ weight + green)
#' 
#' drawROCsForEachModel(seq(0, 1, by = 0.05), "nameBin", model1) 
#' drawROCsForEachModel(seq(0, 1, by = 0.05), "nameBin", models) 
#' 
#' @export
drawROCsForEachModel <- function(threshold_sequence, strNameY, models){
  roc_points_list <- lapply(models, function(model) .findPointsOnTheROC(threshold_sequence, model, strNameY))
  roc_points_dt_list <- lapply(roc_points_list, function(lst){
    roc_points_dt <- transpose(as.data.table(lst))
    names(roc_points_dt) <- c("FPR", "TPR")
    roc_points_dt
  }) 
  
  types_names <- rep(c("random classiffier" ,paste("model", 1:length(models))), 
                     each = length(threshold_sequence))
  
  visual_dt <- data.table(x = c(threshold_sequence, unlist(lapply(roc_points_dt_list, function(lst) lst$FPR))),
                          y = c(threshold_sequence, unlist(lapply(roc_points_dt_list, function(lst) lst$TPR))),
                          type = types_names)
  
  ggplot() +
    geom_point(data=visual_dt[type != "random classiffier", ], aes(x, y, col = type), size = 0.9, alpha = 0.5) +
    geom_path(data=visual_dt, aes(x, y, group = type, col = type), linewidth = 0.6) +
    labs(title= paste0("ROC curve"), 
         x = "False Positive Rate",
         y = "True Positive Rate")+ 
    theme_bw()+
    scale_color_brewer(palette = "Set1")
}


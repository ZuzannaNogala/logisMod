#' Finding points on the ROC curve coressponding to a choosen threshold
#'
#' \code{.findPointsOnTheROC} computes a point which represents the False 
#' Positive Rate (FPR) and the True Positive Rate (TPR, sensitivity) at each threshold
#' setting
#' 
#' @param thresholds_sequance sequance of numeric values from 0 to 1, thresholds 
#' of success' probability - if predicted probability of dependent variable is higher than treshold, the 
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
.findPointsOnTheROC <- function(thresholds_sequance, model, strNameY){
  coefs <- model$coefficients
  preds <- names(coefs)[-1]
  Fails <- model$data[get(strNameY) == 0, ..preds]
  Successes <- model$data[get(strNameY) == 1, ..preds]
  
  resp_vec <- resp_prediction_for_model(model)
  
  FP <- sapply(thresholds_sequance, 
               function(p) False_Positive(p, resp_vec, model$data[, get(strNameY)]))
  TP <- sapply(thresholds_sequance, 
               function(p) True_Positive(p, resp_vec, model$data[, get(strNameY)]))
  
  
  FPR <- FP / nrow(Fails)
  TPR <- TP / nrow(Successes)
  
  points_list <- lapply(1:length(TPR), function(i) c(FPR[i], TPR[i]))
  return(points_list)
}

#' The ROC curve visualization
#' 
#' A receiver operating characteristic curve (ROC curve) is a graphical plot which
#' illustrates the performance of a binary classifier model for diffrent choices of 
#' threshold of success' probability. Each point on curve represents False Positive 
#' Rate (FPR) against True Positive Rate (TPR) at each threshold setting. 
#' 
#' @param threshold_sequance sequance of numeric values from 0 to 1, thresholds 
#' of success' probability - if predicted probability of dependent variable is higher than treshold, the 
#' event is counted as a success
#' @param roc_points_list object of function .findpPointsOnTheROC, list of pairs 
#' represents FPR and TPR for each threshold probability
#' @param model object glm, logistic model
#' @param strNameY characteristic, name of dependent variable which takes values
#' 0 and 1
#' 
#' @import ggplot2
#' @import data.table
#' 
#' @returns plot represents ROC curve
#' 
#' @examples
#' model <- glm(nameBin ~ diameter + blue + red, data = citrus, family = binomial("logit"))
#' 
#' roc_points_list <- .findPointsOnTheROC(seq(0, 1, by = 0.05), model, "nameBin")
#' drawROC(seq(0, 1, by = 0.05), roc_points_list, model, "nameBin")
#' @export
drawROC <- function(threshold_sequance, roc_points_list, model, strNameY){
  roc_points_dt <- transpose(as.data.table(roc_points_list))
  names(roc_points_dt) <- c("FPR", "TPR")
  
  visual_dt <- data.table("x" = c(threshold_sequance, roc_points_dt$FPR),
                          "y" = c(threshold_sequance, roc_points_dt$TPR),
                          "type" = c(rep("random classiffier", length(threshold_sequance)), 
                                     rep("ROC curve of model", length(threshold_sequance))))
  
  ggplot() +
    geom_point(data=visual_dt[22:42, ], aes(x, y), alpha = 0.5, size = 0.9) +
    geom_path(data=visual_dt, aes(x, y, group = type, col = type), size = 0.6) +
    labs(title= paste0("ROC curve"), 
         x = "False Positive Rate",
         y = "True Positive Rate")+ 
    theme_bw() +
    scale_color_manual(values = c("red", "#2574A9")) +
    annotate(geom="text", label = paste("AUC - ", round(computeAUC(model, strNameY), digits = 3)),
             x=0.9, y=0.05, size = 4.5)
}

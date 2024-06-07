#' Box plot visualization for logistic model
#'
#' function displays distribition data based on its five statistic: 
#' minimum, first quartile Q1, median, third quartile Q3 and maximum
#' 
#' @param data data table or data frame, cointains names of variables
#' @param strNameY character, name of dependent variable Y which takes the values 0 and 1
#' @param strNameX character, name of independent variable/predictor X
#' @param plot_title character, optional, title of box plot
#' 
#' @import ggplot2
#' @import patchwork
#' @details
#' package patchwork is available to simplify displaying graphs
#' @examples
#' citrus
#' str(citrus)
#' p1 <- visual_boxplot(data = citrus, strNameY = "nameBin", strNameX ="diameter")
#' p2 <- visual_boxplot(data = citrus, strNameY = "nameBin", strNameX ="green")
#' p1 + p2
#' 
#' @export
visual_boxplot <- function(data, strNameY, strNameX, plot_title = paste("Box plot of", strNameX, "versus", strNameY)){
  if(strNameY %notin% names(data) || strNameX %notin% names(data)) 
    stop(paste0("In data ", strNameY, " or ", strNameX, " doesn't exist! Please check name again."))
  
  ggplot(data, aes(x = as.character(get(strNameY)), 
                   y = get(strNameX), 
                   fill = as.character(get(strNameY)))) +
    geom_boxplot() +
    xlab(strNameY) +
    ylab(strNameX) +
    ggtitle(plot_title) +
    theme_minimal() +
    scale_fill_brewer(palette="Set2") +
    theme(legend.position="none")
}

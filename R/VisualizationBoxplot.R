#' Box plot visualization for logistic model
#'
#' function displays distribition data based on its five statistic: 
#' minimum, first quartile Q1, median, third quartile Q3 and maximum
#' 
#' @param data data table or data frame, cointains names of variables
#' @param str_name_Y character, name of dependent variable Y which takes the values 0 and 1
#' @param str_name_X character, name of independent variable/predictor X
#' 
#' @import ggplot2
#' @examples
#' citrus
#' str(citrus)
#' visual_boxplot(data =citrus, str_name_Y = "nameBin", str_name_X ="diameter")
#' @export
visual_boxplot <- function(data, str_name_Y, str_name_X){
  ggplot(data, aes(x = as.character(get(str_name_Y)), y = get(str_name_X), fill = as.character(get(str_name_Y)))) +
    geom_boxplot() +
    xlab(str_name_Y) +
    ylab(str_name_X) +
    ggtitle(paste("Box plot of", str_name_X, "versus", str_name_Y)) +
    theme_minimal() +
    scale_fill_brewer(palette="Set2") +
    guides(fill=guide_legend(title=str_name_Y))
}

#' Visualisation of Pearson correlation coefficient
#' 
#' Function let us create table of pearson correlation coefficient between 
#' chosen columns in data table. Displays it as a heatmap for better understanding.
#' 
#' @param data data.table of data we want to visualise
#' @param variables character vector, names of columns in \code{data}, for which 
#' we want to compute correlation coefficient
#' 
#' @returns Pearson correlation coefficients heatmap
#' 
#' @import ggplot2
#' @import data.table
#' @importFrom stats cor
#' 
#' @examples
#' vars <- c("diameter", "weight", "red", "green", "blue", "nameBin")
#' corrHeatmap(citrus, vars)
#' 
#' @export
corrHeatmap <- function(data, variables){
  corr <- stats::cor(data[, ..variables])
  corrDT <- as.data.table(corr, keep.rownames = TRUE)
  corrLong <- melt(corrDT, id.vars = "rn", measure.vars = variables)
  
  ggplot(corrLong, aes(x = rn, y = variable)) +
    geom_tile(aes(fill = value)) +
    geom_text(aes(label = round(value, 1))) +
    theme_minimal() +
    scale_fill_gradient(low = "white", high = "red") +
    scale_x_discrete(limits = rev(variables)) +
    labs(x = "variable", y = "variable", 
         title = "Correlation heatmap",
         fill = "correlation")
}



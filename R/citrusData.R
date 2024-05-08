#' Diameter, weight, and mean RGB values for citrus assumed to be either grapefruit or oranges.
#'
#' This dataset takes the color, weight, and diameter of an "average" orange and grapefruit.
#' Good for binary classification.
#'
#' @format A data frame with 9998 rows and 7 variables:
#' \describe{
#'   \item{name}{Orange or grapefruit}
#'   \item{diameter}{Diameter of the citrus fruit in centimeters}
#'   \item{weight}{Diameter of the citrus fruit in grams}
#'   \item{red}{Average red reading from an RGB scan. Values should be from 0 to 255}
#'   \item{green}{Average green reading from an RGB scan. Values should be from 0 to 255}
#'   \item{blue}{Average blue reading from an RGB scan. Values should be from 0 to 255}
#'   \item{nameBin}{Binary classification; 1 - grapefruit, 0 - orange}
#' }
#' @source \url{https://www.kaggle.com/datasets/joshmcadams/oranges-vs-grapefruit}
"citrus"
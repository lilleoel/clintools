# ==== DOCUMENTATION ====

#' find median of rows similar to rowMeans (rowMedians)
#'
#' `rowMedians()` converts a dataframe to a list of row medians.
#'
#' @name rowMedians
#'
#' @usage rowMedians(x, na.rm=FALSE)
#'
#' @param x a dataframe where median of row should be calculated. (`data.frame`)
#'
#' @param na.rm Should missing values be omitted fro the calculations? (`boolian`)
#'
#' @return Returns a list of the median of each row in the inputted dataframe.
#'
#' @examples
#' \dontrun{
#'    rowMedians(df[,c("test1","test2","test3")])
#' }
#'
#' @export
#
# ==== FUNCTION ====

rowMedians <- function(x, na.rm=FALSE){
   apply(x,1,FUN=function(x)median(as.numeric(x),na.rm=na.rm))
}

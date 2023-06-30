# ==== DOCUMENTATION ====

#' Reorder column (recol)
#'
#' `recol()` is a small function which can rename columns and change factors to relevant input
#'
#' @name recol
#'
#' @usage recol(df, old, new, factors=NULL, remove=TRUE, na=NA)
#'
#' @param df dataframe. (`df`)
#'
#' @param old old column name. (`string`)
#'
#' @param new new column name. (`string`)
#'
#' @param factors Named list of factors. If no names then just in alphabetical order (`named list`)
#'
#' @param remove remove old column from dataframe (`boolian`)
#'
#' @param na the na.strings (`list`)
#'
#' @return Returns the dataframe with new columns.
#'
#' @examples
#' \dontrun{
#'    # NUMERIC VARIABLE
#'    df <- recol(df, "X5d_55_Alcohol_127", "Alcohol")
#'    # FACTOR VARIABLE
#'    df <- recol(df, "X5d_55_Alcohol_127", "Alcohol", factors=c("No","Yes"))
#' }
#'
#' @export
#
# ==== FUNCTION ====

recol <- function(df, old, new, factors=NULL, remove=TRUE, na=NA){
   if(any(colnames(df) == new)){
      stop("New column already exists, please choose a new name")
   }
   df[[new]] <- df[[old]]
   if(!is.null(factors)){

      if(!is.null(names(factors))){
         df[[new]] <- as.character(df[[new]])
         for(i in names(factors)){
            df[df[[new]] == i & !is.na(df[[new]]),new] <- factors[names(factors) == i]
         }
         df[[new]] <- as.factor(df[[new]])
      }else{
         df[[new]] <- as.factor(df[[new]])
         levels(df[[new]]) <- factors
      }
   }else{
      if(!is.na(na)){
         df[[new]][df[[new]] %in% na] <- NA
      }
      df[[new]] <- as.numeric(df[[new]])
   }
   if(remove){
      df[[old]] <- NULL
   }
   return(df)
}

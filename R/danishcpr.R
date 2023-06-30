# ==== DOCUMENTATION ====

#' Danish CPR number to birthday and sex (danishcpr)
#'
#' `danishcpr()` converts a list of CPR-numbers to a corresponding list birthday and sex.
#'
#' @name danishcpr
#'
#' @usage danishcpr(code)
#'
#' @param code list of CPR numbers (`list`)
#'
#' @return Returns a list with $birthday which is a list of dates, and $sex which is a list of Male/Female.
#'
#' @examples
#' \dontrun{
#'    cpr <- danishcpr(code)
#'    birthdays <- cpr$birthday
#'    sex <- cpr$sex
#' }
#'
#' @export
#
# ==== FUNCTION ====

danishcpr <- function(code){
   code <- gsub("-","",code)
   output <- NULL

   A <- c(rep("19",4),rep("20",6))
   B <- c(rep("19",5),rep("20",4),"19")
   C <- c(rep("19",5),rep("18",4),"19")
   day <- substr(code,1,2)
   month <- substr(code,3,4)
   year <- substr(code,5,6)
   snum <- 1+as.numeric(substr(code,7,7))
   prefix <- ifelse(as.numeric(year) <= 36,A[snum],ifelse(as.numeric(year)<=57,B[snum],C[snum]))
   year <- paste0(prefix,year)
   output$birthday <- as.Date(paste(year,month,day,sep = "-"))

   sex <- as.numeric(substr(code,10,10)) %% 2
   sex[sex == 1] <- "Male"
   sex[sex == 0] <- "Female"
   output$sex <- sex
   return(output)
}

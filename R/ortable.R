# ==== DOCUMENTATION ====

#' Logistic regression table with Odds ratio (ortable)
#'
#' `ortable()` is a small function which utilises the output from the glm-function to print a dataframe with odds ratio, confidence limits, and p-values.
#'
#' @name ortable
#'
#' @usage ortable(x, d, d_p, intercept, simple)
#'
#' @param x Utilises the output from a glm-function. (`glm-output`)
#'
#' @param d Refers to the number of digits for odds ratio and confidence intervals. Default is `2`. (`numeric`)
#'
#' @param d_p Refers to the number of digits for odds ratio and confidence intervals. Default is `3`. (`numeric`)
#'
#' @param intercept The intercept is presented in the table if `TRUE`. Default is `FALSE`. (`boolian`)
#'
#' @param simple Odds ratio and confidence intervals are merged into one column if `TRUE`. Default is `TRUE`. (`boolian`)
#'
#' @return Returns a dataframe with with odds ratio,
#' confidence limits, and p-values.
#'
#' @examples
#' df <- data.frame(outcome=sample(0:1, 100,replace=TRUE),
#'         var=sample(0:100,100,replace=TRUE))
#' ortable(glm(outcome ~ ., data=df))
#'
#' @importFrom stats coef
#' @importFrom stats confint
#' @export
#
# ==== FUNCTION ====

ortable <- function(x, d = 2, d_p = 3, intercept = F, simple=T){

   temp1 <- as.data.frame(exp(coef(x)))
   temp2 <- as.data.frame(exp(suppressMessages(confint(x))))
   temp3 <- as.data.frame(summary(x)$coef[,4])
   rownames(temp2) <- rownames(temp1)
   results <- as.data.frame(cbind(temp1,cbind(temp2,temp3)))
   colnames(results) <- c("OR","LCL","UCL","p")
   results[,c(1:3)] <- round(results[,c(1:3)],digits=d)
   results[,4] <- round(results[,4],digits=d_p)
   results[results$p == 0,4] <- paste0("<0.",paste0(rep(0,d_p-1),collapse=''),"1")
   if(simple == TRUE){
      results[,1] <- paste0(results[,1], " (", results[,2], "-", results[,3], ")" )
      results <- results[,c(1,4)]
      colnames(results)[1] <- "OR (95%CI)"
   }
   if(intercept == FALSE){
      results <- results[c(2:nrow(results)),]
   }
   return(results)
}

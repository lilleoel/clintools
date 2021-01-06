OR_table <- function(x, d = 3, intercept = F){

   temp1 <- as.data.frame(exp(coef(x)))
   temp2 <- as.data.frame(exp(confint(x)))
   temp3 <- as.data.frame(summary(x)$coef[,4])
   rownames(temp2) <- rownames(temp1)
   results <- as.data.frame(cbind(temp1,cbind(temp2,temp3)))
   colnames(results) <- c("or","lcl","ucl","p")
   results <- round(results,digits=3)
   if(intercept == FALSE){
      results <- results[c(2:nrow(results)),]
   }
   return(results)
}

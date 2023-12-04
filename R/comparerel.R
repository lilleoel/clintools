# ==== DOCUMENTATION ====

#' Compare measures of reliability (comparerel)
#'
#' `comparerel()` is a function which compares measures from calcrel using bootstrapping.
#'
#' @name comparerel
#'
#' @usage comparerel(d1,d2,d3,d4,n_boot,seedno)
#'
#' @param d1 list of numbers from measurement one of sample 1
#' @param d2 list of numbers from measurement two of sample 1 (same order as for d1)
#' @param d3 list of numbers from measurement one of sample 2
#' @param d4 list of numbers from measurement two of sample 2 (same order as for d2)
#' @param n_boot numbers of iterations (default is 1000)
#' @param seedno the seed number used for bootstrapping (default is)
#'
#' @return Returns a nested list of difference between reliability measures using bootstrapping.
#'
#' @examples
#' \dontrun{
#'    d1 <- rnorm(15,10,1)
#'    d2 <- rnorm(15,10,1)
#'    d3 <- rnorm(15,10,1)
#'    d4 <- rnorm(15,10,1)
#'    comparerel(d1,d2,d3,d4)
#' }
#' @export
#
# ==== FUNCTION ====

# d1 <- tmp_df1$first
# d2 <- tmp_df1$last
# d3 <- tmp_df$first
# d4 <- tmp_df$last

comparerel <- function(d1,d2,d3,d4,n_boot=1000,seedno=730){

   res <- NULL
   res1 <- tryCatch(calcrel(d1,d2),error=function(e) e, warning=function(w) w)
   res2 <- tryCatch(calcrel(d3,d4),error=function(e) e, warning=function(w) w)

   if(any(class(res1) %in% c("error","try-error","warning")) |
      any(class(res2) %in% c("error","try-error","warning"))){
         stop()
   }

   diffs <- NULL
   diffs$SRD <- unname(res1$SRD[1]-res2$SRD[1])
   diffs$CV <- unname(res1$CV[1]-res2$CV[1])
   diffs$ICC <- unname(res1$ICC[1]-res2$ICC[1])
   diffs$BA_estimates <- unname(res1$BA_estimates[1]-res2$BA_estimates[1])

   set.seed(seedno)
   for(i in 1:n_boot){
      done <- cbind(d1,d2)
      done <- done[sample(1:nrow(done), replace = TRUE),]
      res1s <- tryCatch(calcrel(done[,1],done[,2]),error=function(e) e, warning=function(w) w)

      dtwo <- cbind(d3,d4)
      dtwo <- dtwo[sample(1:nrow(dtwo), replace = TRUE),]
      res2s <- tryCatch(calcrel(dtwo[,1],dtwo[,2]),error=function(e) e, warning=function(w) w)

      if(any(class(res1) %in% c("error","try-error","warning")) |
         any(class(res2) %in% c("error","try-error","warning"))){
         next()
      }

      diffs$SRD_boot <- c(diffs$SRD_boot,unname(res1s$SRD[1]-res2s$SRD[1]))
      diffs$CV_boot <- c(diffs$CV_boot,unname(res1s$CV[1]-res2s$CV[1]))
      diffs$ICC_boot <- c(diffs$ICC_boot,unname(res1s$ICC[1]-res2s$ICC[1]))
      diffs$BA_estimates_boot <- c(diffs$BA_estimates_boot,
                                   unname(res1s$BA_estimates[1]-res2s$BA_estimates[1]))
   }

   for(i in c("SRD","CV","ICC","BA_estimates")){
      res[[i]] <- c(`diff` = diffs[[i]],
         `diff_sim` = mean(diffs[[paste0(i,"_boot")]],na.rm=T),
         lcl = unname(quantile(diffs[[paste0(i,"_boot")]], 0.025)),
         ucl = unname(quantile(diffs[[paste0(i,"_boot")]], 0.975)),
         pval = min(mean(diffs[[paste0(i,"_boot")]] >= 0,na.rm=T),
                     mean(diffs[[paste0(i,"_boot")]] <= 0,na.rm=T)))

   }

   class(res) <- "comparerel"
   return(res)
}

# FUNCTION | print comparerel ----
#' @method print comparerel
#' @export
print.comparerel <- function(x,...){
   cat("Calculation of difference between two reliability measures\n",
       "All below presented as difference (95%CI;p-value):")
   cat(paste0("- SRD: ",round(x$SRD[1],2),
              " (",round(x$SRD[3],2),";",round(x$SRD[4],2),"; p-value: ",round(x$SRD[5],4),")\n"))
   cat(paste0("- CV: ",round(x$CV[1],2),
              " (",round(x$CV[3],2),";",round(x$CV[4],2),"; p-value: ",round(x$CV[5],4),")\n"))
   cat(paste0("- ICC: ",round(x$ICC[1],2),
              " (",round(x$ICC[3],2),";",round(x$ICC[4],2),"; p-value: ",round(x$ICC[5],4),")\n"))
   cat(paste0("- Bland-Altman bias: ",round(x$BA_estimates[1],2),
              " (",round(x$BA_estimates[3],2),";",round(x$BA_estimates[4],2),"; p-value: ",round(x$BA_estimates[5],4),")\n"))
}

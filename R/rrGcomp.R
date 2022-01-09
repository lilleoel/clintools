# ==== DOCUMENTATION ====

#' Relative risk derived by G-computation (rrGcomp)
#'
#' `rrGcomp()` is a small function which generates population-level (marginal) relative risks derived by G-computation. For models with random effects mixed-effects generalized linear model with a logit link with adjustment for stratification variables will be used, while those without random effects a logistic regression will be used. The code is based on the method used in the paper by Dankiewicz et al. (2021) N Engl J Med. Jun 17;384(24):2283-2294. (\href{https://pubmed.ncbi.nlm.nih.gov/34133859/}{PubMed}
#'
#' @name rrGcomp
#'
#' @usage rrGcomp(df, outcome_col = NULL,
#' group_col = NULL,fixed_strata = NULL,
#' random_strata = NULL, nbrIter = 5000,
#' conf_level = 0.95)
#'
#' @param df the individual participant dataframe
#' @param outcome_col column name for the outcome column
#' @param group_col column name for the group column
#' @param fixed_strata list of column names for the fixed effect stratification columns
#' @param random_strata list of column names for the random effect stratification columns
#' @param nbrIter number of iterations to be used in the G-computation. The original paper used 5000, which is also the default.
#' @param conf_level the confidence level to be reported.
#'
#' @return Returns a list with relative risk (rr), simulated rr (simRR), lower- and upper confidence level (simLCL/simUCL), and the p-value (p_val)
#'
#' @examples
#' df <- sRCT(n_sites=3,n_pop=50)
#' rrGcomp(df,"outcome","Var1","age","site",10)
#'
#' @aliases print.rrGcomp
#'
#' @importFrom lme4 glmer
#' @importFrom stats formula
#' @importFrom stats glm
#' @importFrom stats predict
#' @importFrom stats quantile
#' @importFrom stats binomial
#' @export
#
# ==== FUNCTION ====

#

rrGcomp <- function(df, outcome_col = NULL, group_col = NULL,
                    fixed_strata = NULL, random_strata = NULL,
                    nbrIter = 5000, conf_level = 0.95){
   ETA <- Sys.time()
   results <- NULL

   df_y <- df
   df_y[df_y[[group_col]]==1,group_col]=0
   df_z <- df
   df_z[df_z[[group_col]]==0,group_col]=1

   if(length(fixed_strata > 0)){
      f_strata <- paste("+",paste(fixed_strata, collapse=" + "))
      formel <- formula(paste(outcome_col,"~",group_col,f_strata))
   }
   if(length(random_strata > 0)){
      r_strata <- paste("+ (1 |",paste(random_strata, collapse=") + (1 | "),")")
      formel <- formula(paste(outcome_col,"~",group_col,f_strata,r_strata))
   }

   #FOR RR
   if(length(random_strata) > 0){
      f1 <- suppressMessages(lme4::glmer(formel, family = binomial() , data = df))
   }else{
     f1 <- glm(formel, family=binomial(), data=df)
   }

   pre_y <- predict(f1,df_y,type="response")
   pre_z <- predict(f1,df_z,type="response")
   results$rr <- mean(pre_z/pre_y)

   testF <- function(df, formel, random_strata){
      if(length(random_strata) > 0){
         f1 <- suppressMessages(lme4::glmer(formel, family = binomial() , data = df))
      }else{
         f1 <- glm(formel, family=binomial(), data=df)
      }
      df_y <- df
      df_y[df_y[[group_col]]==1,group_col]=0
      df_z <- df
      df_z[df_z[[group_col]]==0,group_col]=1
      pre_y <- predict(f1,df_y,type="response")
      pre_z <- predict(f1,df_z,type="response")

      return(mean(pre_z)/mean(pre_y))
   }

   simRRs <- replicate(nbrIter, testF(df[sample(1:nrow(df), replace = T),],formel,random_strata))

   results$simRR <- quantile(simRRs,probs=c(0.5))
   results$simLCL <- quantile(simRRs,probs=c((1-conf_level)/2))
   results$simUCL <- quantile(simRRs,probs=c(1-(1-conf_level)/2))


   p_df <- data.frame(summary(f1)$coefficients)
   results$p_val <- p_df$Pr...z..[row.names(p_df) == group_col]
   results$formel <- formel
   results$secs_it_took <- as.numeric(difftime(Sys.time(),ETA,units="secs"))

   class(results) <- "rrGcomp"
   return(results)
}

#' @method print rrGcomp
#' @export
print.rrGcomp <- function(x,...){
   cat("Relative risk derived by G-computation")
   cat("\n RR: ")
   cat(as.character(round(x$rr,digits=2)))

   cat(" (sRR: ")
   cat(as.character(round(x$simRR,digits=2)))
   cat(")")

   cat("\n Confidence interval: ")
   cat(as.character(round(x$simLCL,digits=2)))
   cat(" - ")
   cat(as.character(round(x$simUCL,digits=2)))

   cat("\n p-value: ")
   if(round(x$p_val,digits=4) == 0){
      cat("< 0.0001")
   }else{
      cat(as.character(round(x$p_val,digits=4)))
   }

   cat("\n It took ")
   cat(floor(x$secs_it_took))
   cat(" seconds.")

}

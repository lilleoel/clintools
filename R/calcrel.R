# ==== DOCUMENTATION ====

#' Calculation of reliability (calcrel)
#'
#' `calcrel()` is a function used to calculate different reliability measures, including Coefficient of variance, smallest real difference, intraclass correlation coefficient, and Bland-Altman plot derived bias with 95% limits of agreement.
#'
#' @name calcrel
#'
#' @usage calcrel(d1,d2)
#'
#' @param d1 list of numbers from measurement one
#' @param d2 list of numbers from measurement two (same order as for measurement one)
#'
#' @return Returns a nested list of reliability measures.
#'
#'
#' @examples
#'    d1 <- rnorm(15,10,1)
#'    d2 <- rnorm(15,10,1)
#'    calcrel(d1,d2)
#'
#' @importFrom stats Gamma aov confint.default poisson qt rchisq
#' @importFrom irr icc
#' @importFrom nlme lme
#' @export
#
# ==== FUNCTION ====

calcrel <- function(d1,d2){
  res <- NULL
  tmp_long <- data.frame(rbind(cbind(1:length(d1),1,d1),cbind(1:length(d2),2,d2)))
  colnames(tmp_long) <- c("ID","group","value")
  tmp1 <- data.frame(cbind(1:length(d1),1,d1))
  colnames(tmp1) <- c("ID","group","value")
  tmp2 <- data.frame(cbind(1:length(d2),1,d2))
  colnames(tmp2) <- c("ID","group","value")
  diff <- data.frame((tmp1-tmp2)^2)
  diff$value[diff$value == 0] <- 0.000001

  # 1) SRD
  anova<-aov(value~factor(ID),data=tmp_long)
  anova <- summary(anova)
  StddevW<-sqrt(anova[[1]]["Residuals","Mean Sq"])
  degfree <- anova[[1]]["Residuals","Df"]

  fit <- try(
    glm(value~1 ,family=Gamma(link="log"),data=diff), silent=T
   )

  if("try-error" %in% class(fit)){
    fit <- glm(value~1 ,family=poisson(link="log"),data=diff)
    res$SRD <- c(`est`=unname(qt(0.975, degfree-1)*sqrt(exp(coef(fit)))),
                 `lcl`=qt(0.975, degfree-1)*sqrt(exp(suppressMessages(confint(fit))))[1],
                 `ucl`=qt(0.975, degfree-1)*sqrt(exp(suppressMessages(confint(fit))))[2])
    res$SRD_family <- "poisson"

  }else{
    res$SRD <- c(`est`=unname(qt(0.975, degfree-1)*sqrt(exp(coef(fit)))),
                `lcl`=qt(0.975, degfree-1)*sqrt(exp(suppressMessages(confint(fit))))[1],
                `ucl`=qt(0.975, degfree-1)*sqrt(exp(suppressMessages(confint(fit))))[2])
    res$SRD_family <- "gamma"
  }

  # 2) Coefficient of variance (CV)
  model <- nlme::lme(value~1, data=tmp_long, random=~1 | factor(ID))
  m2 <- summary(model)
  antal= m2$fixDF$X  #Insert correct df
  sigma= model$sigma #residual stdev
  omega= m2$tTable[,"Std.Error"] #intercept stdev
  my= m2$tTable[,"Value"] #fixed effect intercept value
  sigma2=sigma^2
  omega2=omega^2
  varians=omega2/antal+sigma2/(2*antal)
  spred=sqrt(varians)
  s2=sigma2*rchisq(1:100000, df=antal)/antal #insert df
  ystreg=rnorm(1:100000,mean=my,sd=spred)
  cv=sqrt(s2)/ystreg

  res$CV <- c(`est`=StddevW / mean(tmp_long$value) *100,
               `lcl`=unname(round(quantile(cv,probs=c(0.025))*100,1)),
               `ucl`=unname(round(quantile(cv,probs=c(0.975))*100,1)))

  # 3) Intra class correlation coefficient (ICC)
  iccres <- irr::icc(cbind(tmp1$value,tmp2$value),
                     model = "twoway", type = "agreement", unit = "single")

  res$ICC <- c(`est`=iccres$value,
              `lcl`=iccres$lbound,
              `ucl`=iccres$ubound)

  # 4) Bias and Limits of agreegment
  biasLOA <- function(diff, simple = T, digs = 1){
    mean <- mean(diff)
    lower <- round(mean - 1.96*sd(diff),digits=digs)
    upper <- round(mean + 1.96*sd(diff), digits=digs)
    mean <- round(mean, digits=digs)
    output <- c(mean, lower, upper)
    if(simple) output <- paste0(mean, " (",lower,";",upper,")")
    return(output)
  }

  bias <- biasLOA(d2-d1, simple=F)
  res$BA_estimates <- c(`bias`=bias[1],
                        `lloa`=bias[2],
                        `uloa`=bias[3])

  # 5) Paired T-test
  ttest <- t.test(d1,d2, paired=T)
  res$t.test <- c(`est`=ttest$estimate[[1]],
                  `lcl`=ttest$conf.int[1],
                  `ucl`=ttest$conf.int[2],
                  `pval`=ttest$p.value)

  class(res) <- "calcrel"
  return(res)
}

# FUNCTION | print calcrel ----
#' @method print calcrel
#' @export
print.calcrel <- function(x,...){
   cat("Calculation of reliability between two measures\n")
   cat(paste0("- Smallest real difference (SRD; 95%CI): ",round(x$SRD[1],2)," (",round(x$SRD[2],2),";",round(x$SRD[3],2),")\n"))
   cat(paste0("- Coefficient of variance (CV; 95%CI): ",round(x$CV[1],2)," (",round(x$CV[2],2),";",round(x$CV[3],2),")\n"))
   cat(paste0("- Intraclass correlation coefficient (ICC; 95%CI): ",round(x$ICC[1],2)," (",round(x$ICC[2],2),";",round(x$ICC[3],2),")\n"))
   cat(paste0("- Bland-Altman plot estimates - bias (95%LOA): ",round(x$BA_estimates[1],2)," (",round(x$BA_estimates[2],2),";",round(x$BA_estimates[3],2),")"))
}

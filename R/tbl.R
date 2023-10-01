# ==== DOCUMENTATION ====

#' Create a table (tbl)
#'
#' `tbl()` is a function which create a dataframe, which can be copied directly into
#' word or presented in as a summary table.
#'
#' @name tbl
#'
#' @usage tbl(df,strata,vars,render.numeric,
#' render.factor, tests, paired,
#'    digs_n,digs_f, digs_p, digs_s,
#'    only_stats, strata.fixed, strata.random,
#'    present.missing)
#'
#' @param df dataframe. (`df`)
#' @param strata Column name of stratification (`string`)
#' @param vars Column names of variables of interest (`list`)
#' @param render.numeric list of presentation of numeric variables (`list`)
#' @param render.factor presentation of factors, with `simple` removing one factor when only two exists
#' @param tests list of tests carried out, currently the following works: `t.test`, `wilcox.test`, `fisher.test`, `auc`, `lm`, and `glm`. (`list`)
#' @param paired if tests should be paired (`boolean`)
#'
#' @param digs_n digits for numeric (`numeric`)
#' @param digs_f digits for factors (`numeric`)
#' @param digs_p digits for p-values (`numeric`)
#' @param digs_s digits for statistics (`numeric`)
#' @param only_stats if only stats should be presented (`booolean`)
#'
#' @param strata.fixed list of columns which should be used as fixed stratification (`list`)
#' @param strata.random list of columns which should be used as random stratification (`list`)
#' @param present.missing default is dynamic where non-missing variables are not presented.
#'
#' @return Returns summarised information in dataframe.
#'
#' @examples
#' \dontrun{
#'    hmm <- tbl(df,strata="group",
#'    vars = c("Gestational Age at birth","Maternal preeclampsia"),
#'    tests=c("wilcox.test","glm"),only_stats=F,strata.random = "site")
#'    library(pander)
#'    pander(hej, keep.line.breaks = TRUE,split.tables=Inf, row.names = F)
#' }
#'
#' @importFrom stats as.formula na.omit reshape t.test lm fisher.test quasipoisson
#' @importFrom parameters p_value
#' @importFrom pROC auc ci.auc
#' @importFrom lme4 lmer glmer fixef glmerControl
#' @export
#
# ==== FUNCTION ====

# df=pt
# strata="LOS_group"
# vars = c("Age","ASA","Previous craniotomies",
#          "Tumor location","Preoperative Prednisolone",
#          "Preoperative opioid","Type of tumor")
# tests=c("wilcox.test","fisher.test")
# only_stats=F
# render.factor = F
# render.numeric = c("median [IQR]","mean (95%CI)")
# paired = F
# digs_n = 2; digs_f = 1; digs_p = 3; digs_s = 2
# only_stats = F; strata.fixed = NA; strata.random = NA
# present.missing = "dynamic"
tbl <- function(df,strata,vars,
                render.numeric = c("median [IQR]","mean (95%CI)"),
                render.factor = "simple", tests = NA, paired = F,
                digs_n = 2, digs_f = 1, digs_p = 3, digs_s = 2,
                only_stats = T, strata.fixed = NA, strata.random = NA,
                present.missing = "dynamic"){
   d <- df
   d[[strata]] <- as.factor(d[[strata]])
   strata_list <- levels(d[[strata]])
   strata_list <- strata_list[strata_list != ""]
   render.numeric <- c(render.numeric,"missing")

   tbl <- NULL
   # SUMMARY ----

   #   HELPER
   numsum <- function(x){
      if(class(x) %in% c("numeric","integer")){
         out <- NULL
         out$number = length(na.omit(x))
         out$missing = paste0(sum(is.na(x))," (",round(sum(is.na(x))/length(x)*100,1),"%)")
         out$mean = mean(x,na.rm=T)
         out$sd = sd(x,na.rm=T)
         out$lcl = if(all(is.na(x)) | out$sd == 0){ NA
         }else{ t.test(na.omit(x))$conf.int[[1]] }
         out$ucl = if(all(is.na(x)) | out$sd == 0){ NA
         }else{ t.test(na.omit(x))$conf.int[[2]] }
         out$median = median(x,na.rm=T)
         out$min = quantile(x,na.rm=T)[["0%"]]
         out$max = quantile(x,na.rm=T)[["100%"]]
         out$q1 = quantile(x,na.rm=T)[["25%"]]
         out$q3 = quantile(x,na.rm=T)[["75%"]]
      }else{
         warning("Not numeric or integer as input")
      }
      return(out)
   }

   #   ACTUAL
   for(i in vars){

      #NUMERIC
      if(class(d[[i]]) %in% c("numeric","integer")){
         for(j in render.numeric){
            tbl$var <- c(tbl$var,i)
            tbl$var2 <- c(tbl$var2,j)

            for(k in strata_list){
               nmb <- j

               if(!is.null(strata)){
                  dt <- d[d[[strata]] == k,]
               }else{ dt <- d }

               vrs <- numsum(dt[[i]])
               nmb <- gsub("iqr","q1;q3",tolower(nmb))
               nmb <- gsub("range","min to max",nmb)
               nmb <- gsub("95%ci","lcl;ucl",nmb)
               for(l in names(vrs)){
                  if(class(vrs[[l]]) %in% "numeric"){
                     nmb <- gsub(l,format(round(vrs[[l]],digs_n),nsmall=digs_n),nmb)
                  }else{
                     nmb <- gsub(l,vrs[[l]],nmb)
                  }
               }
               tbl[[k]] <- c(tbl[[k]],nmb)

            }
         }

         # FACTOR
      }else if(class(d[[i]]) %in% c("character","factor")){
         d[[i]] <- as.factor(d[[i]])
         if(length(levels(d[[i]])) == 2 & render.factor %in% "simple"){
            lvls <- levels(d[[i]])[[2]]
         }else{ lvls <- levels(d[[i]]) }
         for(j in c(lvls,"missing")){
            tbl$var <- c(tbl$var,i)
            tbl$var2 <- c(tbl$var2,j)

            for(k in strata_list){
               if(!is.null(strata)){
                  dt <- d[d[[strata]] == k,]
               }else{ dt <- d }
               if(j != "missing"){ nmb <- sum(dt[[i]] == j & !is.na(dt[[i]]))
               }else{ nmb <- sum(is.na(dt[[i]])) }
               nmb <- paste0(nmb, " (",
                             format(round(nmb/nrow(dt)*100,digs_f),nsmall=digs_f),
                             "%)")
               tbl[[k]] <- c(tbl[[k]],nmb)
            }
         }
      }
   }
   tbl <- data.frame(tbl,check.names = F)

   if(present.missing == "dynamic"){
      tmp <- tbl[tbl$var2 == "missing",]
      for(m in strata_list){
         tmp[[m]] <- as.numeric(gsub("[^0-9]", "",tmp[[m]]))
      }
      tmp$allmiss <- rowSums(tmp[,strata_list])
      tbl <- tbl[!row.names(tbl) %in% row.names(tmp[tmp$allmiss == 0,]),]
   }

   #Add subheaders
   if(length(vars) > length(unique(tbl$var))){
      tmp <- data.frame(matrix(ncol=ncol(tbl),nrow=0))
      for(i in vars){
         if(!(i %in% tbl$var)){
            i <- paste0("**",i,"**")
            i <- c(i,rep(" ",ncol(tmp)-1))
            tmp <- rbind(tmp,i)
         }else{
            tmp <- rbind(tmp,tbl[tbl$var == i,])
         }
         colnames(tmp) <- colnames(tbl)
      }
      tbl <- tmp
   }

   tbl$var[duplicated(tbl$var)] <- ""

   # STATISTICS ----
   #   HELPER
   numeric2groups <- function(d,var,strata,strata_list,test,digs_s,digs_p,
                              paired,strata.fixed,strata.random){
      out <- NULL
      if(test %in% c("t.test","wilcox.test")){
         y <- d[[var]][d[[strata]] == strata_list[1]]
         x <- d[[var]][d[[strata]] == strata_list[2]]

         if(test=="t.test") tst <- suppressWarnings(t.test(x,y,paired=paired))
         if(test=="wilcox.test") tst <- wilcox.test(x,y,paired=paired,conf.int = T)

         if(length(tst$estimate) == 2) tst$estimate <- tst$estimate[1]-tst$estimate[2]
         est <- format(round(tst$estimate[[1]],digs_s),nsmall=digs_s)
         lcl <- format(round(tst$conf.int[[1]],digs_s),nsmall=digs_s)
         ucl <- format(round(tst$conf.int[[2]],digs_s),nsmall=digs_s)

         if(test=="t.test" & paired==T) out$txt <- "paired.t.test"
         if(test=="t.test" & paired==F) out$txt <- "unpaired.t.test"
         if(test=="wilcox.test" & paired==T) out$txt <- "paired.wilcox.test"
         if(test=="wilcox.test" & paired==F) out$txt <- "unpaired.wilcox.test"

         out$estci <- paste0(est, " (",lcl,";",ucl,")")
         out$pval <- format(round(tst$p.value,digs_p),nsmall=digs_p)
      }else if(test=="auc"){
         y <- d[[var]][d[[strata]] == strata_list[1]]
         x <- d[[var]][d[[strata]] == strata_list[2]]

         dfauc <- data.frame(rbind(cbind("x",x),cbind("y",y)))
         colnames(dfauc) <- c("group","val")
         dfauc$val <- as.numeric(dfauc$val)
         auctest1 <- suppressMessages(pROC::auc(dfauc$group,dfauc$val,direction="<"))
         auctest2 <- suppressMessages(pROC::auc(dfauc$group,dfauc$val,direction=">"))
         if(auctest1 < auctest2) auctest1 <- auctest2
         tst <- pROC::ci.auc(auctest1)

         est <- format(round(tst[[2]],digs_s),nsmall=digs_s)
         lcl <- format(round(tst[[1]],digs_s),nsmall=digs_s)
         ucl <- format(round(tst[[3]],digs_s),nsmall=digs_s)

         out$txt <- "auroc"
         out$estci <- paste0(est, " (",lcl,";",ucl,")")
         out$pval <- NA
      }else if(test %in% c("lm")){

         formel <- paste0("`",strata,"`")
         if(any(!is.na(strata.fixed))){
            formel <- paste(formel,"+",paste0("`",strata.fixed,"`",collapse=" + "))
         }
         if(!is.na(strata.random)){
            formel <- paste(formel,"+",paste0("(1|`",strata.random,"`)",collapse=" + "))
         }
         formel <- formula(paste0("`",var,"`~",formel))

         if(!is.na(strata.random)){
            m1 <- lme4::lmer(formel,data=d)
            est <- format(round(fixef(m1)[grepl(strata,names(fixef(m1)))],digs_s),nsmall=digs_s)

            ci <- confint(m1)
            lcl <- format(round(ci[grepl(strata,rownames(ci)),1],digs_s),nsmall=digs_s)
            ucl <- format(round(ci[grepl(strata,rownames(ci)),2],digs_s),nsmall=digs_s)

            pval <- parameters::p_value(m1)
            pval <- pval[grepl(strata,pval$Parameter),"p"]

            out$txt <- "lmer"

         }else{
            m1 <- lm(formel,data=d)
            est <- format(round(m1$coefficients[grepl(strata,names(m1$coefficients))],digs_s),nsmall=digs_s)
            ci <- confint(m1)
            lcl <- format(round(ci[grepl(strata,rownames(ci)),1],digs_s),nsmall=digs_s)
            ucl <- format(round(ci[grepl(strata,rownames(ci)),2],digs_s),nsmall=digs_s)

            pval <- parameters::p_value(m1)
            pval <- pval[grepl(strata,pval$Parameter),"p"]

            out$txt <- "lm"
         }

         out$estci <- paste0(est, " (",lcl,";",ucl,")")
         out$pval <- format(round(pval,digs_p),nsmall=digs_p)
      }
      return(out)
   }

   factorXgroups <- function(d,var,strata,strata_list,test,digs_s,digs_p,
                             paired,strata.fixed,strata.random){
      out <- NULL
      if(test %in% c("fisher.test")){
         x <- d[[strata]]
         y <- d[[j]]

         tst <- tryCatch(fisher.test(table(y,x))
            ,error=function(e) e, warning=function(w) w)

         if(any(class(tst) %in% c("error","try-error","warning"))){
            tst <- fisher.test(table(y,x),simulate.p.value=TRUE,B=10^(digs_p+1))
         }
         if(!is.null(tst$estimate)){
            est <- format(round(tst$estimate[[1]],digs_s),nsmall=digs_s)
            lcl <- format(round(tst$conf.int[[1]],digs_s),nsmall=digs_s)
            ucl <- format(round(tst$conf.int[[2]],digs_s),nsmall=digs_s)
         }else{
            est <- "-"; lcl <- "-"; ucl <- "-";
         }


         out$txt <- "fisher.test"
         out$estci <- paste0(est, " (",lcl,";",ucl,")")
         out$pval <- format(round(tst$p.value,digs_p),nsmall=digs_p)
      }else if(test %in% c("glm") & length(strata_list) == 2){
         formel <- paste0("`",strata,"`")
         if(!is.na(strata.fixed)){
            formel <- paste(formel,"+",paste0("`",strata.fixed,"`",collapse=" + "))
         }
         if(!is.na(strata.random)){
            formel <- paste(formel,"+",paste0("(1|`",strata.random,"`)",collapse=" + "))
         }
         formel <- formula(paste0("`",var,"`~",formel))

         m <- d[complete.cases(d[,colnames(d) %in% c(var,strata,strata.fixed,strata.random)]),]

         if(!is.na(strata.random)){
            m1 <- tryCatch(lme4::glmer(formel, data = m,
                                 family=binomial(log)),error=function(e) e, warning=function(w) w)

            if(any(class(m1) %in% c("error","try-error","warning"))){
               m1 <- tryCatch(lme4::glmer(formel, data = m,
                                     family=binomial(log), nAGQ = 0),error=function(e) e, warning=function(w) w)
            }
            if(any(class(m1) %in% c("error","try-error","warning"))){
               m1 <- tryCatch(lme4::glmer(formel, data = m,
                                     family=binomial(log), control=glmerControl(optimizer="bobyqa")),error=function(e) e, warning=function(w) w)
            }
            if(!(any(class(m1) %in% c("error","try-error","warning")))){ out$txt <- "glmer" }
            if(any(class(m1) %in% c("error","try-error","warning"))){
               formel <- paste0(deparse(formel),collapse="")
               formel <- gsub("\\(1 \\|","",formel)
               formel <- gsub("\\)","",formel)
               tmp_d <- d
               tmp_d[[j]] <- as.numeric(as.factor(tmp_d[[j]]))-1
               m1 <- glm(as.formula(formel), data = tmp_d, family=quasipoisson)
               out$txt <- "glm"
            }
         }else{
            tmp_d <- d
            tmp_d[[j]] <- as.numeric(as.factor(tmp_d[[j]]))-1

            m1 <- glm(formel, data = tmp_d, family=quasipoisson)
            out$txt <- "glm"
         }

         if(out$txt == "glm"){
            est <- format(round(exp(coef(m1))[grepl(strata,names(coef(m1)))],digs_s),nsmall=digs_s)

            ci <- try(exp(confint(m1)),silent=T)
            if("try-error" %in% class(ci)) ci <- exp(confint.default(m1))
            lcl <- format(round(ci[grepl(strata,rownames(ci)),1],digs_s),nsmall=digs_s)
            ucl <- format(round(ci[grepl(strata,rownames(ci)),2],digs_s),nsmall=digs_s)

            pval <- parameters::p_value(m1)
            pval <- pval[grepl(strata,pval$Parameter),"p"]

         }else if(out$txt == "glmer"){
            res <- exp(cbind(fixef(m1), confint(m1, method = 'Wald')[-1,]))
            est <- format(round(res[grepl(strata,rownames(res)),1],digs_s),nsmall=digs_s)
            lcl <- format(round(res[grepl(strata,rownames(res)),2],digs_s),nsmall=digs_s)
            ucl <- format(round(res[grepl(strata,rownames(res)),3],digs_s),nsmall=digs_s)
            pval <- summary(m1)$coefficients[grepl(strata,rownames(summary(m1)$coefficients)),4]
         }
         out$estci <- paste0(est, " (",lcl,";",ucl,")")
         out$pval <- format(round(pval,digs_p),nsmall=digs_p)
      }

      return(out)
   }




   # ACTUAL ----
   if(any(!is.na(tests))){
      tsts <- NULL
      tsts$var <- vars
      tsts <- data.frame(tsts)
      for(i in tests){
         tmp <- NULL
         for(j in vars){
            # if(is.null(d[[vars[j]]])) next
            res <- NULL
            if(class(d[[j]]) %in% c("numeric","integer") &
               length(strata_list) == 2){
               res <- numeric2groups(d,var=j,strata,strata_list,test=i,digs_s,digs_p,
                                     paired,strata.fixed,strata.random)
            }else if(class(d[[j]]) %in% c("factor","character")){
               res <- factorXgroups(d,var=j,strata,strata_list,test=i,digs_s,digs_p,
                                    paired,strata.fixed,strata.random)
            }
            if(!is.null(res)){
               tmp$var <- c(tmp$var,j)
               tmp[[res$txt]] <- c(tmp[[res$txt]],res$estci)
               tmp[[paste0("pval.",res$txt)]] <- c(tmp[[paste0("pval.",res$txt)]],res$pval)
            }
         }
         tmp <- data.frame(tmp)
         tsts <- merge(tsts,tmp,by="var",all=T)
      }
      tsts[tsts == "- (-;-)"] <- NA
      tsts <- tsts[,colSums(is.na(tsts))<nrow(tsts)]
      tsts <- tsts[rowSums(is.na(tsts))<ncol(tsts)-1,]

      # MERGE
      tbl$id <- 1:nrow(tbl)
      tbl <- merge(tbl,tsts,by="var",all=T,)
      tbl <- tbl[order(tbl$id),]
      tbl$id <- NULL
      tbl[is.na(tbl)] <- " "
   }

   # ADD N to groups
   if(only_stats & all(!is.na(tests))){
      tbl <- tbl[,!(colnames(tbl) %in% strata_list)]
      tbl$var2 <- NULL
      tbl <- tbl[rowSums(tbl == " ")<nrow(tbl),]
   }else{
      for(i in strata_list){
         colnames(tbl)[colnames(tbl) == i] <-
            paste0("**",i,"**\\\n*n = ",sum(d[[strata]] == i),"*")
      }
   }


   # BEAUTIFY
   nmz <- c(`var` = " ", `var2` = " ",
            `paired.t.test` = "**Paired t-test**\\\n*mean diff. (95%CI)*",
            `unpaired.t.test` = "**Unpaired t-test**\\\n*mean diff. (95%CI)*",
            `paired.wilcox.test` = "**Wilcoxon signed rank test**\\\n*median diff. (95%HLCI)*",
            `unpaired.wilcox.test` = "**Wilcoxon rank sum test**\\\n*median diff. (95%HLCI)*",
            `fisher.test` = "**Fisher's exact test**\\\n*OR (95%CI)*",
            `lm` = "**Linear regression**\\\n*estimate (95%CI)*",
            `lmer` = "**Mixed effects linear regression**\\\n*estimate (95%CI)*",
            `glm` = "**Logistic regression**\\\n*RR (95%CI)*",
            `glmer` = "**Mixed effects logistic regression**\\\n*RR (95%CI)*",
            `auroc` = "**AUROC**\\\n*AUC (95%CI)*")

   txt_pval <- "*p*"
   colnames(tbl)[grepl("pval\\.",colnames(tbl))] <- txt_pval

   for(i in names(nmz)){
      colnames(tbl)[colnames(tbl) == i] <- nmz[names(nmz) == i]
   }

   rownames(tbl) <- 1:nrow(tbl)
   if(sum(colnames(tbl) == "*p*") > 1){
      p_colz <- which(colnames(tbl) == "*p*")
      last_p <- max(p_colz)
      p_colz <- p_colz[p_colz != last_p]
      tmp <- unname(which(rowSums(tbl[,colnames(tbl) == "*p*"] != " ") == 1))
      if(length(tmp) != 0){
         for(i in tmp){
            tbl[i,last_p] <- gsub(" ","",paste0(tbl[i,c(p_colz,last_p)],collapse=""))
            tbl[i,p_colz] <- " "
         }
      }
      tbl <- tbl[,colSums(tbl == " ")<nrow(tbl)]
      colnames(tbl) <- gsub("\\.[[:digit:]]","",colnames(tbl))
   }


   return(tbl)
}

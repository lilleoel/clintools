# ==== DOCUMENTATION ====

#' Create a table (tbl)
#'
#' `tbl()` is a function which create a dataframe, which can be copied directly into
#' word or presented in as a summary table.
#'
#' @name tbl
#'
#' @usage tbl(df,strata,vars,render.numeric,
#' render.factor, tests, test.vars, paired,
#'    digs_n,digs_f, digs_p, digs_s,
#'    only_stats, strata.fixed, strata.random,
#'    time.to, present.missing, conf.level,
#'    zeroonetoyn,
#'    markdown, caption, rd)
#'
#' @param df dataframe. (`df`)
#' @param strata Column name of stratification (`string`)
#' @param vars Column names of variables of interest (`list`)
#' @param render.numeric list of presentation of numeric variables (`list`)
#' @param render.factor presentation of factors, with `simple` removing one factor when only two exists
#' @param tests list of tests carried out, currently the following works: `t.test`, `wilcox.test`, `fisher.test`, `auc`, `lm`, and `glm`. (`list`)
#' @param test.vars a list of variable names from vars, where analyses should be carried out. If left NA all variable will be analysed (`list`)
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
#' @param time.to Column name of the time column for cox regression (`list`)
#' @param present.missing default is dynamic where non-missing variables are not presented. `FALSE` removes missingness from presentation.
#' @param conf.level confidence intervals which should be presented (`numeric`).
#' @param zeroonetoyn for factor variables which are 0 and 1, convert them to No and Yes (`boolean`)
#' @param markdown default is true and output is pander, while false output is a dataframe (`boolean`)
#' @param caption Table caption only in use when markdown is true (`string`)
#' @param rd if risk difference should be added as an output for logistic regressions (`boolean`)
#'
#' @return Returns summarised information in dataframe.
#'
#' @examples
#' \dontrun{
#'    hmm <- tbl(df,strata="group",
#'    vars = c("Gestational Age at birth","Maternal preeclampsia"),
#'    tests=c("wilcox.test","glm"),only_stats=F,strata.random = "site",
#'    markdown=F)
#'    pander::pander(hmm, keep.line.breaks = TRUE,split.tables=Inf, row.names = F)
#' }
#'
#' @importFrom stats as.formula na.omit reshape t.test lm fisher.test quasipoisson dbinom pbinom
#' @importFrom parameters p_value
#' @importFrom pROC auc ci.auc
#' @importFrom lme4 lmer glmer fixef glmerControl
#' @importFrom pander pander
#' @export
#
# ==== FUNCTION ====
#
# strata = NULL;
# render.numeric = c("median [IQR]","mean (%CI)");
# render.factor = "simple"; tests = NA; test.vars = NA; paired = F;
# digs_n = 2; digs_f = 1; digs_p = 3; digs_s = 2;
# only_stats = F; strata.fixed = NA; strata.random = NA;
# time.to = NA; present.missing = "dynamic"; conf.level = 0.95;
# zeroonetoyn =T
# markdown=T; caption=""
#
# strata="group"
# vars = c("Primary outcome")
# tests=c("glm")
# strata.random = "site"
# rd = F



tbl <- function(df,strata = NULL,vars,
                render.numeric = c("median [IQR]","mean (%CI)"),
                render.factor = "simple", tests = NA, test.vars = NA, paired = F,
                digs_n = 2, digs_f = 1, digs_p = 3, digs_s = 2,
                only_stats = F, strata.fixed = NA, strata.random = NA,
                time.to = NA, present.missing = "dynamic", conf.level = 0.95,
                zeroonetoyn = T,
                markdown=T, caption="",
                rd=T){
   d <- df
   if(!is.null(strata)){
      d[[strata]] <- as.factor(d[[strata]])
      strata_list <- levels(d[[strata]])
      strata_list <- strata_list[strata_list != ""]
   }else{
      strata_list <- "Overall"
   }
   if(!isFALSE(present.missing)) render.numeric <- c(render.numeric,"missing")

   tbl <- NULL
   # SUMMARY ----

   #   HELPER
   numsum <- function(x,ci){
      if(class(x) %in% c("numeric","integer")){
         out <- NULL
         out$number = length(na.omit(x))
         out$missing = paste0(sum(is.na(x))," (",round(sum(is.na(x))/length(x)*100,1),"%)")
         out$mean = mean(x,na.rm=T)
         out$sd = sd(x,na.rm=T)
         out$lcl = if(all(is.na(x)) | out$sd == 0 | is.na(out$sd)){ NA
         }else{ t.test(na.omit(x), ,conf.level=conf.level)$conf.int[[1]] }
         out$ucl = if(all(is.na(x)) | out$sd == 0 | is.na(out$sd)){ NA
         }else{ t.test(na.omit(x), ,conf.level=conf.level)$conf.int[[2]] }
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

   # NAMED VARS
   if(!is.null(names(vars))){
      for(i in 1:length(vars)){
         if(nchar(names(vars)[i]) > 0){
            d[[names(vars)[i]]] <- d[[vars[i]]]
            vars[i] <- names(vars)[i]
         }
      }
   }

   # Convert 0 and 1 to No and Yes
   if(zeroonetoyn){
      for(i in 1:length(vars)){
         if(!is.null(d[[vars[i]]]) &
            length(na.omit(unique(d[[vars[i]]]))) == 2 &
            levels(as.factor(d[[vars[i]]]))[1] == "0" &
            levels(as.factor(d[[vars[i]]]))[2] == "1" &
            class(d[[vars[i]]]) %in% c("character","factor")){
            d[[vars[i]]] <- as.factor(d[[vars[i]]])
            levels(d[[vars[i]]]) <- c("No","Yes")
         }
      }
   }

   # ACTUAL
   for(i in vars){

      #NUMERIC
      if(class(d[[i]]) %in% c("numeric","integer")){
         for(j in render.numeric){
            tbl$var <- c(tbl$var,i)

            if(grepl("%ci",tolower(j))){
               j_full <- gsub("%ci",paste0(round(conf.level*100,digs_n),"%CI"),tolower(j))
            }else{ j_full <- j }
            tbl$var2 <- c(tbl$var2,j_full)

            for(k in strata_list){
               nmb <- j

               if(!is.null(strata)){
                  dt <- d[d[[strata]] == k,]
               }else{ dt <- d }

               nmb <- gsub("iqr","q1;q3",tolower(nmb))
               nmb <- gsub("range","min to max",nmb)

               nmb <- gsub("%ci","lcl;ucl",nmb)
               vrs <- numsum(dt[[i]], ci=conf.level)

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
         if(!isFALSE(present.missing)) lvls <- c(lvls,"missing")
         for(j in lvls){
            tbl$var <- c(tbl$var,i)
            tbl$var2 <- c(tbl$var2,j)
            for(k in strata_list){
               if(is.null(strata)){
                  dt <- d
               }else{
                  dt <- d[d[[strata]] == k,]
               }
               if(j != "missing"){
                  nmb <- sum(dt[[i]] == j & !is.na(dt[[i]]))
               }else{
                  nmb <- sum(is.na(dt[[i]]))
               }
               if(!isFALSE(present.missing)){
                  demonit <- nrow(dt)
               }else{
                  demonit <- length(na.omit(dt[[i]]))
               }
               nmb <- paste0(nmb, " (",
                             format(round(nmb/demonit*100,digs_f),nsmall=digs_f),
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
      if(length(strata_list) < 2){
         tmp$allmiss <- tmp[,strata_list]
      }else{
         tmp$allmiss <- rowSums(tmp[,strata_list])
      }
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

   #Longify
   tmp <- aggregate(tbl$var,by=list(tbl$var),length)
   for(i in tmp$Group.1){
      if(all(tbl$var2[tbl$var == i] != " ")){
         if(min(which(tbl$var %in% i)) == 1){
            tbl <- rbind(c(i,rep(" ",ncol(tbl)-1)),
                         tbl[min(which(tbl$var %in% i)):nrow(tbl),])
         }else{
            tbl <- rbind(tbl[1:(min(which(tbl$var %in% i))-1),],
                         c(i,rep(" ",ncol(tbl)-1)),
                         tbl[min(which(tbl$var %in% i)):nrow(tbl),])
         }
      }
   }
   tbl$var[duplicated(tbl$var)] <- ""

   tbl$var[tbl$var == ""] <- paste0("   ", tbl$var2[tbl$var == ""])
   tbl$var2 <- NULL

   # STATISTICS ----
   #   HELPER
   numeric2groups <- function(d,var,strata,strata_list,test,digs_s,digs_p,
                              paired,strata.fixed,strata.random,conf.level){
      out <- NULL
      if(test %in% c("t.test","wilcox.test")){
         y <- d[[var]][d[[strata]] == strata_list[1]]
         x <- d[[var]][d[[strata]] == strata_list[2]]

         if(test=="t.test") tst <- suppressWarnings(t.test(x,y,paired=paired,conf.level=conf.level))
         if(test=="wilcox.test") tst <- wilcox.test(x,y,paired=paired,conf.int = T,conf.level=conf.level)

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
      }else if(test %in% c("van.elteren") & length(strata_list) == 2){
         # outcome og gruppevariabler
         y <- d[[var]]
         g <- d[[strata]]

         # kræver en stratifikationsvariabel
         if(is.na(strata.fixed) && is.na(strata.random)){
            stop("Van Elteren kræver en stratifikationsvariabel (fx strata.fixed eller strata.random)")
         }

         block <- if(!is.na(strata.fixed)) d[[strata.fixed]] else d[[strata.random]]
         block <- as.factor(block)
         # lav data frame til coin
         dfve <- data.frame(y = y, g = g, block = block)

         # Van Elteren test (asymptotisk fordeling)
         tst <- coin::wilcox_test(y ~ g | block, data = dfve, distribution = "asymptotic")

         # Hodges–Lehmann estimat og CI
         hl <-  wilcox.test(y ~ g, data=dfve, conf.int = T,conf.level=conf.level)

         est <- format(round(hl$estimate[[1]],digs_s),nsmall=digs_s)
         lcl <- format(round(hl$conf.int[[1]],digs_s),nsmall=digs_s)
         ucl <- format(round(hl$conf.int[[2]],digs_s),nsmall=digs_s)

         out$txt <- "van.elteren"
         out$estci <- paste0(est, " (", lcl, ";", ucl, ")")
         out$pval <- sprintf(paste0("%.", digs_p, "f"), round(coin::pvalue(tst), digs_p))
      }else if(test=="auc"){
         y <- d[[var]][d[[strata]] == strata_list[1]]
         x <- d[[var]][d[[strata]] == strata_list[2]]

         dfauc <- data.frame(rbind(cbind("x",x),cbind("y",y)))
         colnames(dfauc) <- c("group","val")
         dfauc$val <- as.numeric(dfauc$val)
         auctest1 <- suppressMessages(pROC::auc(dfauc$group,dfauc$val,direction="<"))
         auctest2 <- suppressMessages(pROC::auc(dfauc$group,dfauc$val,direction=">"))
         if(auctest1 < auctest2) auctest1 <- auctest2
         tst <- pROC::ci.auc(auctest1,conf.level=conf.level)

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

            ci <- tryCatch(confint(m1,level=conf.level)
                     ,error=function(e) e, warning=function(w) w)
            if(any(class(ci) %in% c("error","try-error","warning"))){
               ci <- confint(m1, method="Wald",level=conf.level)
            }

            lcl <- format(round(ci[grepl(strata,rownames(ci)),1],digs_s),nsmall=digs_s)
            ucl <- format(round(ci[grepl(strata,rownames(ci)),2],digs_s),nsmall=digs_s)

            pval <- parameters::p_value(m1)
            pval <- pval[grepl(strata,pval$Parameter),"p"]

            out$txt <- "lmer"

         }else{
            m1 <- lm(formel,data=d)
            est <- format(round(m1$coefficients[grepl(strata,names(m1$coefficients))],digs_s),nsmall=digs_s)
            ci <- confint(m1,level=conf.level)
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
                             paired,strata.fixed,strata.random,time.to,conf.level){
      out <- NULL
      if(test %in% c("fisher.test")){
         x <- d[[strata]]
         y <- d[[var]]

         tst <- tryCatch(fisher.test(table(y,x),conf.level=conf.level)
                         ,error=function(e) e, warning=function(w) w)

         if(any(class(tst) %in% c("error","try-error","warning"))){
            tst <- fisher.test(table(y,x),simulate.p.value=TRUE,B=10^(digs_p+1),conf.level=conf.level)
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
         if(any(!is.na(strata.fixed))){
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
               m1 <- glm(as.formula(formel), data = tmp_d, family=poisson(link = "log"))
               out$txt <- "glm"
            }
         }else{
            tmp_d <- d
            tmp_d[[j]] <- as.numeric(as.factor(tmp_d[[j]]))-1

            m1 <- tryCatch(glm(formel, data = tmp_d, family=binomial(log)),error=function(e) e, warning=function(w) w)
            if(any(class(m1) %in% c("error","try-error","warning"))){
               m1 <- glm(formel, data = tmp_d, family = poisson(link = "log"))
            }


            out$txt <- "glm"
         }

         if(out$txt == "glm"){
            est <- format(round(exp(coef(m1))[grepl(strata,names(coef(m1)))],digs_s),nsmall=digs_s)

            ci <- try(exp(confint(m1,level=conf.level)),silent=T)
            if("try-error" %in% class(ci)) ci <- exp(confint.default(m1,conf.level=conf.level))
            lcl <- format(round(ci[grepl(strata,rownames(ci)),1],digs_s),nsmall=digs_s)
            ucl <- format(round(ci[grepl(strata,rownames(ci)),2],digs_s),nsmall=digs_s)

            pval <- parameters::p_value(m1)
            pval <- pval[grepl(strata,pval$Parameter),"p"]

         }else if(out$txt == "glmer"){
            res <- exp(cbind(fixef(m1), confint(m1, method = 'Wald',level=conf.level)[-1,]))
            est <- format(round(res[grepl(strata,rownames(res)),1],digs_s),nsmall=digs_s)
            lcl <- format(round(res[grepl(strata,rownames(res)),2],digs_s),nsmall=digs_s)
            ucl <- format(round(res[grepl(strata,rownames(res)),3],digs_s),nsmall=digs_s)
            pval <- summary(m1)$coefficients[grepl(strata,rownames(summary(m1)$coefficients)),4]
         }
         out$estci <- paste0(est, " (",lcl,";",ucl,")")
         out$pval <- format(round(pval,digs_p),nsmall=digs_p)

         # --- RD start ---
         if(length(unique(m[[var]])) == 1){
            out$rdci <- NA
         }else{
            # --- RD start ---
            # Byg newdata (nd) ud fra model.frame så alle predictor-navne + factor-levels er præcis som i modellen
            mf <- tryCatch(model.frame(m1), error = function(e) model.frame(formula(m1), data = m))

            # response- og predictor-navne i model-frame
            mf_names <- names(mf)
            resp_name <- mf_names[1]
            preds <- mf_names[-1]

            # Find hvad strata hedder i model-frame (håndterer både "strata" og syntaktiske navne)
            possible <- c(strata, make.names(strata))
            strata_in_mf <- intersect(preds, possible)
            if(length(strata_in_mf) == 0){
               # forsøg match uden ikke-alfanumeriske tegn
               clean <- function(x) gsub("[^[:alnum:]]", "", x)
               matches <- which(clean(preds) == clean(strata))
               if(length(matches)) strata_in_mf <- preds[matches[1]]
            }
            if(length(strata_in_mf) == 0){
               stop("Kunne ikke finde 'strata' blandt modellens prediktorer. Tjek navne.")
            }

            # Start nd som to kopier af første observation i mf (bevarer factor-struktur)
            nd <- mf[rep(1, 2), , drop = FALSE]

            # Sæt default værdier for alle prediktorer (undtagen strata som sættes separat)
            for(v in preds){
               if(v == strata_in_mf) next
               colv <- mf[[v]]
               if(is.factor(colv)){
                  # vælg reference-level (første) som default
                  nd[[v]] <- factor(rep(levels(colv)[1], 2), levels = levels(colv))
               } else if(is.numeric(colv)){
                  nd[[v]] <- rep(mean(colv, na.rm = TRUE), 2)
               } else {
                  # fallback: kopier eksisterende værdi
                  nd[[v]] <- rep(colv[1], 2)
               }
            }

            # Sæt strata-rækkerne: hvis faktor -> brug de to første levels; hvis numerisk -> brug to unikke værdier hvis muligt
            col_strata <- mf[[strata_in_mf]]
            if(is.factor(col_strata)){
               levs <- levels(col_strata)
               if(length(levs) < 2) stop("Strata har mindre end 2 niveauer i data.")
               nd[[strata_in_mf]] <- factor(c(levs[1], levs[2]), levels = levs)
            } else {
               uniqv <- sort(unique(col_strata))
               if(length(uniqv) >= 2){
                  nd[[strata_in_mf]] <- uniqv[1:2]
               } else {
                  # default fallback (hvis kun én numerisk værdi findes)
                  nd[[strata_in_mf]] <- c(0,1)
               }
            }

            # Check: hvis outcome er 0 i alle obs (eller 1 i alle), så returner NA for estimering (predict vil fejle/degenerere)
            resp_vec <- model.response(mf)
            if(all(resp_vec == 0) || all(resp_vec == 1)){
               out$rdci <- NA
               # (du kan også sætte out$estci/out$pval <- NA hvis ønsket)
            } else {
               if(out$txt == "glm"){
                  # glm: predict med se.fit (newdata har nu alle de rigtige columns)
                  p <- predict(m1, newdata = nd, type = "response", se.fit = TRUE)
                  p0 <- as.numeric(p$fit[1]); p1 <- as.numeric(p$fit[2])
                  rd <- p1 - p0
                  # konservativ antagelse: var(rd) = var(p0)+var(p1) (vi antager ingen kovarians)
                  se_rd <- sqrt((p$se.fit[1])^2 + (p$se.fit[2])^2)
               } else if(out$txt == "glmer"){
                  # glmer: delta-metode på fixed effects (marginal på populationens niveau)
                  X <- model.matrix(delete.response(terms(m1)), nd)
                  V <- as.matrix(vcov(m1))
                  fit <- X %*% fixef(m1)              # linear predictor (2 x 1)
                  pfit <- 1/(1 + exp(-fit))           # probabilities (2 x 1)
                  rd <- as.numeric(pfit[2] - pfit[1])
                  # gradient for hver række: each row i X multipliceres med p*(1-p)
                  grad <- sweep(X, 1, as.numeric(pfit * (1 - pfit)), FUN = "*")  # 2 x k
                  # varians for hver pred. sandsynlighed
                  var_each <- diag(grad %*% V %*% t(grad))
                  # varians for forskel = var(p1)+var(p0) - 2*cov(p1,p0)
                  # vi kan estimere cov via off-diagonal i grad %*% V %*% t(grad)
                  cov_mat <- grad %*% V %*% t(grad)
                  var_rd <- var_each[1] + var_each[2] - 2 * cov_mat[1,2]
                  se_rd <- sqrt(max(var_rd, 0))  # undgå negative pga. numerik
               } else {
                  stop("Ukendt modeltype i out$txt: skal være 'glm' eller 'glmer'.")
               }

               alpha <- 1 - conf.level
               lcl_rd <- rd - qnorm(1 - alpha/2) * se_rd
               ucl_rd <- rd + qnorm(1 - alpha/2) * se_rd

               out$rdci <- paste0(
                  format(round(rd, digs_s), nsmall = digs_s), " (",
                  format(round(lcl_rd, digs_s), nsmall = digs_s), ";",
                  format(round(ucl_rd, digs_s), nsmall = digs_s), ")"
               )
            }
         }
         # --- RD slut ---

      }else if(test %in% c("midp") & paired == T & length(strata_list) == 2){
         midP <- function(n) {

            if (n[1, 2] == n[2, 1]) {
               midP <- 1 - 0.5 * dbinom(n[1, 2], n[1, 2] + n[2, 1], 0.5)
            } else {
               P <- 2 * pbinom(min(n[1, 2], n[2, 1]), n[1, 2] + n[2, 1], 0.5)
               P <- min(P, 1)
               midP <- P - dbinom(n[1, 2], n[1, 2] + n[2, 1], 0.5)
            }

            return( midP )
         }

         x <- d[[strata]]
         y <- d[[var]]
         res_midp <- midP(table(d[d[[strata]] == strata_list[1],var],
                                d[d[[strata]] == strata_list[2],var]))

         out$txt <- "midp"
         out$estci <- format(round(res_midp,digs_p),nsmall=digs_p)
         out$pval <- ""


      }else if(test %in% c("cox")){
         #COX
         d[[var]] <- as.numeric(d[[var]])-1
         SurvVar <- survival::Surv(d[[time.to]],d[[var]])

         formel <- paste0("`",c(strata,strata.fixed),"`",collapse=" + ")
         formel <- formula(paste0("`SurvVar`~",formel))
         m1 <- survival::coxph(formel, data = d)
         m1est <- exp(m1$coefficients)
         m1ci <- exp(confint(m1,level=conf.level))

         est <- format(round(m1est[[1]],digs_s),nsmall=digs_s)
         lcl <- format(round(m1ci[1,1],digs_s),nsmall=digs_s)
         ucl <- format(round(m1ci[1,2],digs_s),nsmall=digs_s)

         out$txt <- "cox"
         out$estci <- paste0(est, " (",lcl,";",ucl,")")
         out$pval <- format(round(parameters::p_value(m1)[1,2],digs_p),nsmall=digs_p)

      }

      return(out)
   }


   # ACTUAL ----
   if(any(!is.na(tests))){
      tsts <- NULL
      if(any(is.na(test.vars))){
         tsts$var <- vars[vars %in% colnames(d)]
      }else{
         tsts$var <- test.vars
      }
      tsts <- data.frame(tsts)
      for(i in tests){
         tmp <- NULL
         for(j in tsts$var){
            # if(is.null(d[[vars[j]]])) next
            res <- NULL
            if(class(d[[j]]) %in% c("numeric","integer") &
               length(strata_list) == 2){
               res <- numeric2groups(d,var=j,strata,strata_list,test=i,digs_s,digs_p,
                                     paired,strata.fixed,strata.random,conf.level)
            }else if(class(d[[j]]) %in% c("factor","character")){
               res <- factorXgroups(d,var=j,strata,strata_list,test=i,digs_s,digs_p,
                                    paired,strata.fixed,strata.random,time.to,conf.level)
            }

            tmp$var <- c(tmp$var,j)
            if(!is.null(res)){
               tmp[[res$txt]] <- c(tmp[[res$txt]],res$estci)
               if(rd) tmp[[paste0("rd.",res$txt)]] <- c(tmp[[paste0("rd.",res$txt)]], res$rdci)
               tmp[[paste0("pval.",res$txt)]] <- c(tmp[[paste0("pval.",res$txt)]],res$pval)
            }else{
               tmp[[names(tmp)[2]]] <- c(tmp[[names(tmp)[2]]],NA)
               tmp[[names(tmp)[3]]] <- c(tmp[[names(tmp)[3]]],NA)
               if(rd) tmp[[names(tmp)[4]]] <- c(tmp[[names(tmp)[4]]],NA)
            }
         }
         if(length(unique(lengths(tmp))) > 1) stop("Some statistical analysis did not converge, try to run analyses one at a time!")
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
      tbl[is.na(tbl)] <- ""
   }

   #Shorten for those with only one line apart from missing
   for(i in 1:nrow(tbl)){
      if(substr(tbl[i,1],1,2) == "  " & !grepl("missing",tbl[(i),1])){
         if(substr(tbl[(i-1),1],1,2) != "  " &
            (i == nrow(tbl) | substr(tbl[(i+1),1],1,2) != "  " | grepl("missing",tbl[(i+1),1]))){
            tbl[(i-1),1] <- paste(tbl[(i-1),1],"-",gsub("^.{0,3}", "",tbl[(i),1]))
            tbl[(i-1),1] <- gsub(" - Yes","",tbl[(i-1),1])
            tbl[(i),1] <- ""
            for(j in strata_list){
               tbl[(i-1),j] <- tbl[(i),j]
               tbl[(i),j] <- ""
            }
         }
      }
   }
   tbl <- tbl[rowSums(tbl == "" | is.na(tbl)) != ncol(tbl), ]
   tbl <- tbl[ ,colSums(tbl == "" | is.na(tbl)) != nrow(tbl)]


   # ADD N to groups
   if(only_stats & all(!is.na(tests))){
      tbl <- tbl[,!(colnames(tbl) %in% strata_list)]
      tbl$var2 <- NULL
      tbl <- tbl[rowSums(tbl == " ")<nrow(tbl),]
   }else if(!is.null(strata)){
      for(i in strata_list){
         colnames(tbl)[colnames(tbl) == i] <-
            paste0("**",i,"**\\\n*n = ",sum(d[[strata]] == i),"*")
      }
   }else{
      colnames(tbl)[colnames(tbl) == strata_list] <-
         paste0("**",strata_list,"**\\\n*n = ",nrow(d),"*")
   }


   # BEAUTIFY
   nmzci <- round(conf.level*100,digs_s)
   nmz <- c(`var` = " ",
      `paired.t.test` = paste0("**Paired t-test**\\\n*mean diff. (",nmzci,"%CI)*"),
      `unpaired.t.test` = paste0("**Unpaired t-test**\\\n*mean diff. (",nmzci,"%CI)*"),
      `paired.wilcox.test` = paste0("**Wilcoxon signed rank test**\\\n*median diff. (",nmzci,"%HLCI)*"),
      `unpaired.wilcox.test` = paste0("**Wilcoxon rank sum test**\\\n*median diff. (",nmzci,"%HLCI)*"),
      `van.elteren` = paste0("**Van Elteren**\\\n*median diff. (",nmzci,"%HLCI)*"),
      `fisher.test` = paste0("**Fisher's exact test**\\\n*OR (",nmzci,"%CI)*"),
      `midp` = paste0("**McNemar's test**\\\n*p*-value"),
      `lm` = paste0("**Linear regression**\\\n*estimate (",nmzci,"%CI)*"),
      `lmer` = paste0("**Mixed effects linear regression**\\\n*estimate (",nmzci,"%CI)*"),
      `glm` = paste0("**Logistic regression**\\\n*RR (",nmzci,"%CI)*"),
      `glmer` = paste0("**Mixed effects logistic regression**\\\n*RR (",nmzci,"%CI)*"),
      `rd.glm` = paste0("**Logistic regression**\\\n*RD (",nmzci,"%CI)*"),
      `rd.glmer` = paste0("**Mixed effects logistic regression**\\\n*RD (",nmzci,"%CI)*"),
      `auroc` = paste0("**AUROC**\\\n*AUC (",nmzci,"%CI)*"),
      `cox` = paste0("**Cox Proportional-Hazards Model**\\\n*HR (",nmzci,"%CI)*"))

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



   if(markdown){
      tbl[,1] <- gsub(" ","&nbsp;",tbl[,1])
      pander::pander(tbl, keep.line.breaks = TRUE,split.tables=Inf, row.names = F,
                     caption=caption)
   }else{
      return(tbl)
   }
}

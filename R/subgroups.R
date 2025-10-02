# ==== DOCUMENTATION ====

#' Create subgroup analysis table (subgroups)
#'
#' `subgroups()` is a function which create a dataframe, which can be copied directly into
#' word or presented in as a subgroup analysis.
#'
#' @name subgroups
#'
#' @usage subgroups(df,group,subgroups,outcome,test,time.to,
#'                   strata.fixed,strata.random,digs_p,digs_s,
#'                   conf.level, paired,markdown, caption, fig)
#'
#' @param df dataframe. (`df`)
#' @param group Column name of stratification (`string`)
#' @param subgroups column names of subgroups (`list`)
#' @param outcome column name of outcome of interest (`string`)
#' @param test list of tests carried out, currently the following works: `t.test`, `wilcox.test`, `fisher.test`, `auc`, `lm`, and `glm`. (`list`)
#' @param paired if tests should be paired (`boolean`)
#' @param digs_p digits for p-values (`numeric`)
#' @param digs_s digits for statistics (`numeric`)
#' @param strata.fixed list of columns which should be used as fixed stratification (`list`)
#' @param strata.random list of columns which should be used as random stratification (`list`)
#' @param time.to Column name of the time column for cox regression (`list`)
#' @param conf.level confidence intervals which should be presented (`numeric`).
#' @param markdown default is true and output is pander, while false output is a dataframe (`boolean`)
#' @param caption Table caption only in use when markdown is true (`string`)
#' @param fig If figure should be presented (`boolean`)
#'
#' @return Returns summarised information in dataframe.
#'
#' @examples
#' \dontrun{
#'    zubgroups <- c(`Estimated fetal weight`="est_fetal_weight_grp",
#'    `Gestational age`="Gestational Age at rand (cat)",
#'    `End diastolic flow`="end_dia_flow_grp",
#'    `Uterine artery Doppler / Notch`="dop_notch_grp",
#'    `Maternal S-Placental-GF`="maternalSplacentalGF_grp",
#'    `Hypertension or preeclampsia`="ht_preeclamps_grp",
#'    `Abdominal circumference`="abd_circum_cent_grp")
#'
#'    subgroups(df,group="group",zubgroups,
#'       outcome="Infant survival without SAE",
#'       test="glm",strata.fixed = "site")
#' }
#'
#' @importFrom stats as.formula anova setNames
#' @importFrom patchwork wrap_plots
#' @importFrom ggplot2 geom_point expand_scale geom_errorbar
#' @importFrom utils stack
#' @export
#
# ==== FUNCTION ====
#
# group = NULL;
# render.numeric = c("median [IQR]","mean (%CI)");
# render.factor = "simple"; tests = NA; test.vars = NA; paired = F;
# digs_n = 2; digs_f = 1; digs_p = 3; digs_s = 2;
# only_stats = F; strata.fixed = NA; strata.random = NA;
# time.to = NA; present.missing = "dynamic"; conf.level = 0.95;
# zeroonetoyn =T
# markdown=T; caption=""
# fig=T
#
# group="group"
# subgroups=c("Trial","Sex"," Age","GAF/SAS-SR")
# outcome="Primary outcome (short)"
# test="glm"

subgroups <- function(df,group,subgroups,outcome,
                      test = NA, time.to = NA,
                      strata.fixed = NA, strata.random = NA,
                      digs_p = 3, digs_s = 2,
                      conf.level = 0.95, paired=F,
                      markdown=T, caption="",
                      fig = F){

####### HELPER FUNCTIONS #######
   format_p_value <- function(p_value, digs_p) {
      # Check if the p-value is less than the threshold for rounding
      if (p_value < 10^(-digs_p)) {
         return(paste0("<", format(10^(-digs_p), nsmall = digs_p)))
      } else {
         return(format(round(p_value, digs_p), nsmall = digs_p))
      }
   }

   quickStat <- function(d,outcome,group,test,digs_s,digs_p,
                         paired,strata.fixed,strata.random,interactions,time.to,conf.level){

      # Helper functions
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

      format_ci <- function(est, lcl, ucl, digs_s) {
         paste0(format(round(est, digs_s), nsmall = digs_s), " (",
                format(round(lcl, digs_s), nsmall = digs_s), ";",
                format(round(ucl, digs_s), nsmall = digs_s), ")")
      }

      generate_formula <- function(outcome, group, strata.fixed, strata.random,
                                   interactions) {
         formel <- paste0("`",group,"`")
         if(any(!is.na(strata.fixed))){
            formel <- paste0(formel," + ",paste0("`",strata.fixed,"`",collapse = " + "))
         }
         if(!is.na(strata.random)){
            formel <- paste0(formel, " + (1|`", strata.random, "`)", collapse = " + ")
         }
         if(all(!is.na(interactions))){
            formel <- paste0(formel," + ",paste0("`",interactions[1],"`*`",
                                              interactions[2],"`"))
         }
         formula(paste0("`", outcome, "` ~ ", formel))
      }

      # Starting
      out <- NULL

      d[[group]] <- as.factor(d[[group]])
      group_list <- levels(d[[group]])
      group_list <- group_list[group_list != ""]

      formel <- generate_formula(outcome, group, strata.fixed, strata.random,
                                 interactions)
      formelf <- generate_formula(outcome, group, strata.fixed=c(strata.fixed, strata.random),
                                 strata.random = NA,
                                 interactions)

      # Continuous outcome
      if(class(d[[outcome]]) %in% c("numeric","integer") & length(group_list) == 2){
         y <- d[[outcome]][d[[group]] == group_list[1]]
         x <- d[[outcome]][d[[group]] == group_list[2]]

         if(test %in% c("t.test","wilcox.test")){
            tst <- if(test == "t.test"){
               suppressWarnings(t.test(x, y, paired = paired, conf.level = conf.level))
            }else{
               wilcox.test(x, y, paired = paired, conf.int = TRUE,
                           conf.level = conf.level)
            }
            if(length(tst$estimate) == 2) tst$estimate <-
                  tst$estimate[1]-tst$estimate[2]

            out$txt <- if (test == "t.test") {
               if (paired) "paired.t.test" else "unpaired.t.test"
            } else {
               if (paired) "paired.wilcox.test" else "unpaired.wilcox.test"
            }

            out$estci <- format_ci(tst$estimate[[1]],
                                   tst$conf.int[[1]], tst$conf.int[[2]], digs_s)
            out$est <- tst$estimate[[1]]
            out$lcl <- tst$conf.int[[1]]
            out$ucl <- tst$conf.int[[2]]
            out$pval <- format_p_value(tst$p.value, digs_p)

         }else if(test=="auc"){
            auc_values <- sapply(c("<", ">"), function(dir) {
               suppressMessages(pROC::auc(d[[group]], d[[outcome]], direction = dir))
            })
            best_auc <- max(auc_values)
            tst <- pROC::ci.auc(best_auc, conf.level = conf.level)

            out$txt <- "auroc"
            out$estci <- format_ci(tst[2], tst[1], tst[3], digs_s)
            out$est <- tst[2]
            out$lcl <- tst[1]
            out$ucl <- tst[3]
            out$pval <- NA

         }else if(test %in% c("lm")){
            if(!is.na(strata.random)){
               m1 <- lme4::lmer(formel,data=d)
               est <- fixef(m1)[grepl(group,names(fixef(m1)))]
               ci <- tryCatch(confint.default(m1,level=conf.level),
                              error=function(e) e, warning=function(w) w)
               if(any(class(ci) %in% c("error","try-error","warning"))){
                  ci <- confint(m1, method="Wald",level=conf.level)
               }
               out$txt <- "lmer"
            }else{
               m1 <- lm(formel,data=d)
               est <- m1$coefficients[grepl(group,names(m1$coefficients))]
               ci <- confint.default(m1,level=conf.level)
               out$txt <- "lm"
            }
            out$estci <- format_ci(est, ci[grepl(group,rownames(ci)),1],
                                   ci[grepl(group,rownames(ci)),2], digs_s)
            out$est <- est
            out$lcl <- ci[grepl(group,rownames(ci)),1]
            out$ucl <- ci[grepl(group,rownames(ci)),2]
            pval <- parameters::p_value(m1)
            pval <- pval[grepl(group,pval$Parameter),"p"]
            out$pval <- format_p_value(pval, digs_p)
         }

         # Categorical outcome
      }else if(class(d[[outcome]]) %in% c("factor","character")){
         if(test %in% c("fisher.test")){
            x <- d[[group]]
            y <- d[[outcome]]

            tst <- tryCatch(fisher.test(table(y,x),conf.level=conf.level)
                            ,error=function(e) e, warning=function(w) w)
            if(any(class(tst) %in% c("error","try-error","warning"))){
               tst <- fisher.test(table(y,x),simulate.p.value=TRUE,B=10^(digs_p+1),
                                  conf.level=conf.level)
            }
            if(!is.null(tst$estimate)){
               out$estci <- format_ci(tst$estimate[[1]], tst$conf.int[[1]],
                                      tst$conf.int[[2]], digs_s)
               out$est <- tst$estimate[[1]]
               out$lcl <- tst$conf.int[[1]]
               out$ucl <- tst$conf.int[[2]]
            }else{
               out$estci <- "- (-;-)"
               out$est <- NA
               out$lcl <- NA
               out$ucl <- NA
            }
            out$txt <- "fisher.test"
            format_p_value(tst$p.value, digs_p)

         }else if(test %in% c("glm") & length(group_list) == 2){
            m <- d[complete.cases(d[,colnames(d) %in%
                                       c(outcome,group,strata.fixed,strata.random)]),]

            if(!is.na(strata.random)){
               fit_glmer_safe <- function(formel, data) {
                  # Liste af forsøg med forskellige kontrolparametre
                  attempts <- list(
                     list(args = list(family = binomial(log))),                   # standard
                     list(args = list(family = binomial(log), nAGQ = 0)),         # fallback 1
                     list(args = list(family = binomial(log),
                                      control = glmerControl(optimizer = "bobyqa")))  # fallback 2
                  )

                  for(attempt in attempts){
                     result <- try(lme4::glmer(formel, data = data, !!!attempt$args), silent = TRUE)
                     if(!inherits(result, "try-error")){
                        return(result)  # returnér straks hvis succes
                     }
                  }

                  stop("Alle forsøg på at fit glmer mislykkedes.")
               }

               # Brug funktionen:
               m1 <- tryCatch(fit_glmer_safe(formel, m),
                              error=function(e) e, warning=function(w) w)
               if(!(any(class(m1) %in% c("error","try-error","warning")))){
                  out$txt <- "glmer" }

               if(any(class(m1) %in% c("error","try-error","warning"))){
                  formel_fixed <- as.formula(
                     gsub("\\(1 \\| ([^)]+)\\)", "\\1", paste(deparse(formel), collapse = ""))
                  )
                  tmp_d <- d
                  tmp_d[[outcome]] <- as.numeric(as.factor(tmp_d[[outcome]]))-1
                  m1 <- glm(formel_fixed, data = tmp_d, family=poisson(link = "log"))
                  out$txt <- "glm"
               }
            }else{
               tmp_d <- d
               tmp_d[[outcome]] <- as.numeric(as.factor(tmp_d[[outcome]]))-1

               m1 <- glm(formel, data = tmp_d, family=poisson(link = "log"))
               out$txt <- "glm"
            }

            if(out$txt == "glm"){
               est <- exp(coef(m1))[grepl(group,names(coef(m1))) &
                                       !grepl(":",names(coef(m1)))]

               ci <- try(exp(confint.default(m1,level=conf.level)),silent=T)
               if("try-error" %in% class(ci)) ci <-
                  exp(confint(m1,conf.level=conf.level))
               lcl <- ci[grepl(group,rownames(ci)) &
                            !grepl(":",names(coef(m1))),1]
               ucl <- ci[grepl(group,rownames(ci)) &
                            !grepl(":",names(coef(m1))),2]

               pval <- parameters::p_value(m1)
               pval <- pval[grepl(group,pval$Parameter) &
                               !grepl(":",pval$Parameter),"p"]

            }else if(out$txt == "glmer"){
               res <- exp(cbind(fixef(m1), confint.default(m1,
                                                   level=conf.level)[-1,]))
               est <- res[grepl(group,rownames(res)) &
                             !grepl(":",rownames(res)),1]
               lcl <- res[grepl(group,rownames(res)) &
                             !grepl(":",rownames(res)),2]
               ucl <- res[grepl(group,rownames(res)) &
                             !grepl(":",rownames(res)),3]
               pval <- summary(m1)$coefficients[grepl(
                  group,rownames(summary(m1)$coefficients)) &
                     !grepl(":",rownames(summary(m1)$coefficients)),4]
            }
            out$estci <- format_ci(est, lcl, ucl, digs_s)
            out$est <- est
            out$lcl <- lcl
            out$ucl <- ucl
            out$pval <- format_p_value(pval, digs_p)

         }else if(test %in% c("midp") & paired == T & length(group_list) == 2){
            res_midp <- midP(table(d[d[[group]] == group_list[1],outcome],
                                   d[d[[group]] == group_list[2],outcome]))

            out$txt <- "midp"
            out$estci <- ""
            out$est <- NA
            out$lcl <- NA
            out$ucl <- NA
            out$pval <- format_p_value(res_midp, digs_p)

         }else if(test %in% c("cox")){
            d[[outcome]] <- as.numeric(d[[outcome]])-1
            SurvVar <- survival::Surv(d[[time.to]],d[[outcome]])

            formel <- paste0("`",c(group,strata.fixed),"`",collapse=" + ")
            if(all(!is.na(interactions))){
               formel <- paste(formel,"+",paste0("`",interactions[1],"`*`",interactions[2],"`"))
            }
            formel <- formula(paste0("`SurvVar`~",formel))
            m1 <- survival::coxph(formel, data = d)
            m1est <- exp(m1$coefficients)
            m1ci <- exp(confint.default(m1,level=conf.level))

            out$txt <- "cox"
            out$estci <- format_ci( m1est[[1]], m1ci[1,1], m1ci[1,2], digs_s)
            out$est <- m1est[[1]]
            out$lcl <- m1ci[1,1]
            out$ucl <- m1ci[1,2]
            out$pval <- format_p_value(parameters::p_value(m1)[1,2], digs_p)
         }
      }

      test_names <- list(
         "paired.t.test" = "Paired t-test",
         "unpaired.t.test" = "Unpaired t-test",
         "paired.wilcox.test" = "Wilcoxon signed rank test",
         "unpaired.wilcox.test" = "Wilcoxon rank sum test",
         "fisher.test" = "Fisher's exact test",
         "midp" = "McNemar's test",
         "lm" = "Linear regression",
         "lmer" = "Mixed effects linear regression",
         "glm" = "Logistic regression",
         "glmer" = "Mixed effects logistic regression",
         "auroc" = "AUROC",
         "cox" = "Cox Proportional-Hazards Model"
      )
      out$name <- test_names[[out$txt]]
      nmzci <- round(conf.level * 100, digs_s)
      out$estci_txt <- paste0(
         switch(out$txt,
                "paired.t.test" = "MD",
                "unpaired.t.test" = "MD",
                "paired.wilcox.test" = "median diff.",
                "unpaired.wilcox.test" = "median diff.",
                "fisher.test" = "OR",
                "midp" = NA,
                "lm" = "est.",
                "lmer" = "est.",
                "glm" = "RR",
                "glmer" = "RR",
                "auroc" = "AUC",
                "cox" = "HR"),
         " (", nmzci, "%CI)")

      out <- c(out, list(model = m1))

      return(out)
   }

####### HELPER FUNCTIONS #######

   d <- df

   # NAMED VARS
   if(!is.null(names(subgroups))){
      for(i in 1:length(subgroups)){
         if(nchar(names(subgroups)[i]) > 0){
            d[[names(subgroups)[i]]] <- d[[subgroups[i]]]
            subgroups[i] <- names(subgroups)[i]
         }
      }
   }

   tbl <- NULL
   # SUMMARY ----
   for(i in subgroups){

      tmp_reduced <- quickStat(d[!is.na(d[[i]]),],outcome,group,test,digs_s,digs_p,
                               paired,strata.fixed=i,
                               strata.random,interactions=NA,time.to,conf.level)
      tmp_full <- quickStat(d[!is.na(d[[i]]),],outcome,
                            group,test,digs_s,digs_p,
                            paired,strata.fixed=i,strata.random,interactions=c(i,group),
                            time.to,conf.level)

      lrt <- anova(tmp_reduced$model, tmp_full$model, test = "LRT")

      tbl <- rbind(tbl,cbind(i,levels(factor(d[[i]])),
                             format_p_value(na.omit(lrt$`Pr(>Chi)`),digs_p)))

   }

   tbl <- data.frame(tbl)
   colnames(tbl) <- c("subgroup","group","p_interaction")

   # Analyses for each subgroup
   res <- NULL
   for(i in 1:nrow(tbl)){
      tmpd <- d[d[,tbl[i,1]] == tbl[i,2],]
      tmp <- quickStat(tmpd,outcome,group,test,digs_s,
                       digs_p,paired,strata.fixed,strata.random,
                       interactions=NA,time.to,conf.level)
      tmpd <- tmpd[complete.cases(tmpd[,na.omit(c(outcome,group,strata.fixed,
                                                  strata.random))]),]

      tmp$C <- paste0(sum(tmpd[[group]] == levels(tmpd[[group]])[1] &
                             tmpd[[outcome]] == levels(tmpd[[outcome]])[2]), "/",
                      sum(tmpd[[group]] == levels(tmpd[[group]])[1]))
      tmp$E <- paste0(sum(tmpd[[group]] == levels(tmpd[[group]])[2] &
                             tmpd[[outcome]] == levels(tmpd[[outcome]])[2]), "/",
                      sum(tmpd[[group]] == levels(tmpd[[group]])[2]))

      tmp$model <- NULL

      res <- rbind(res,data.frame(tmp))
   }
   tbl <- cbind(tbl,res)

   tbl2 <- tbl[,c("subgroup","group","C","E","estci","est","lcl","ucl","pval","p_interaction")]

   tbl2$yaxis <- as.factor(c(1:nrow(tbl2)))
   tbl2$subgroup[duplicated(tbl2$subgroup)] <- ""
   tbl2$p_interaction[tbl2$subgroup == ""] <- ""

   if(!fig){
      tbl2[,c("yaxis","est","lcl","ucl")] <- NULL
      colnames(tbl2) <- c("",
                          "**Subgroup**",
                          paste0("**",levels(d[[group]])[1],"**\\\n*n/N*"),
                          paste0("**",levels(d[[group]])[2],"**\\\n*n/N*"),
                          paste0("**",unique(tbl$name),"**\\\n*",
                                 unique(tbl$estci_txt),"*"),
                          "**p-value**",
                          "**p-value for interaction**")

      if(markdown){
         pander::pander(tbl2, keep.line.breaks = TRUE,split.tables=Inf, row.names = F,
                        caption=caption)
      }else{
         return(tbl2)
      }
   }else{
      ######## FIGUR
      # Left
      tmp <- cbind(tbl2$yaxis,stack(tbl2[,colnames(tbl2) %in% c("group","C","E","estci")]))
      colnames(tmp)[1] <- "yaxis"
      tmp$hjust <- (tmp$ind != "group")*0.5

      left <- ggplot(tmp, aes(y=yaxis, x=ind, label=values)) +
         geom_text(size = 3, hjust=tmp$hjust) +
         scale_x_discrete(position = "top",expand=expand_scale(mult = c(0, 0.25)),
                          labels=c("Subgroups",paste0(levels(d[[group]])[1],"\nn/N"),
                                   paste0(levels(d[[group]])[2],"\nn/N"),
                                   unique(tbl$estci_txt))
         ) +
         scale_y_discrete(limits=rev, labels=rev(tbl2$subgroup)) +
         labs(y = NULL, x = NULL) +
         theme_classic() +
         theme(
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_text(face = "bold"),
            plot.margin = margin()
         )

      # Middle
      middle <- ggplot(tbl2,aes(x = yaxis, y = est, ymin = lcl,
                                ymax = ucl)) +
         geom_hline(yintercept = 1, colour = "red") +
         scale_x_discrete(limits=rev) +
         ylab(unique(tbl$estci_txt)) +
         geom_errorbar(width = 0.1) +
         geom_point() +
         theme_classic() +
         theme(
            strip.text.y = element_text(face = "bold", size = 12),
            legend.position = "none",
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank(),
            axis.line.y = element_blank(),
            plot.margin = margin()
         ) +
         coord_flip()

      # Right
      tmp <- cbind(tbl2$yaxis,stack(tbl2[,colnames(tbl2) %in% c("pval","p_interaction")]))
      colnames(tmp)[1] <- "yaxis"

      right <- ggplot(tmp, aes(y=yaxis, x=ind, label=values)) +
         geom_text(size = 3) +
         scale_x_discrete(position = "top",expand=expand_scale(mult = c(0.2, 0.5)),
                          labels=c("p-value",
                                   "p-value\nfor interaction")) +
         scale_y_discrete(limits=rev) +
         labs(y = NULL, x = NULL) +
         theme_classic() +
         theme(
            panel.grid.major = element_blank(),
            panel.border = element_blank(),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            axis.text.x = element_text(face="bold"),
            plot.margin = margin()
         )

      print(patchwork::wrap_plots(left,middle,right,widths = c(4,3,2)))

   }


}






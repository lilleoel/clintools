# ==== DOCUMENTATION ====

#' Create a table (tbl)
#'
#' `tbl()` is a function which create a dataframe, which can be copied directly into
#' word or presented in as a summary table.
#'
#' @name tbl
#'
#' @usage tbl(df,strata,vars,cols,tests = NA,digs = 2,digs_p = 3)
#'
#' @param df dataframe. (`df`)
#'
#' @param strata Column name of stratification (`string`)
#'
#' @param vars Column names of variables of interest (`list`)
#'
#' @param cols the methods on how to aggregate data (`list`)
#'
#' @param tests Tests - functional are 'wilcox', 'ttest', and 'auc' (`list`)
#'
#' @param digs digits in output (`numeric`)
#'
#' @param digs_p digits in output for p-values (`numeric`)
#'
#' @return Returns summarised information in dataframe.
#'
#' @examples
#' \dontrun{
#'    hmm <- tbl1(df,strata="group",
#'       vars=c("WBC_renset","Ratio_WBC"),
#'       cols=c("No","Missing","median [IQR]"),
#'       tests=c("Wilcox.test","AUC"))
#'    knitr::kable(hmm)
#' }
#'
#' @importFrom stats as.formula na.omit reshape t.test
#' @export
#
# ==== FUNCTION ====

tbl <- function(df,strata,vars,cols,tests = NA,digs = 2,digs_p = 3){
   dfi <- df
   dfi[[strata]] <- as.factor(dfi[[strata]])
   strata_list <- levels(dfi[[strata]])

   # SUMMARY PARAMETERS ----
   res <- NULL
   for(i in vars){
      for(j in strata_list){
         if(!is.null(strata)) tmpj <- dfi[dfi[[strata]] == j,]
         if(is.null(strata)) tmpj <- dfi

         if(class(dfi[[i]]) %in% c("numeric","integer")){
            tmp_list <- c(
               `type` = "num",
               `number` = length(na.omit(tmpj[[i]])),
               `missing` = sum(is.na(tmpj[[i]])),
               `mean` = mean(tmpj[[i]],na.rm=T),
               `sd` = sd(tmpj[[i]],na.rm=T),
               `lcl` = if(all(is.na(tmpj[[i]]))){ NA
               }else{ t.test(na.omit(tmpj[[i]]))$conf.int[[1]] },
               `ucl` = if(all(is.na(tmpj[[i]]))){ NA
               }else{ t.test(na.omit(tmpj[[i]]))$conf.int[[2]] },
               `median` = median(tmpj[[i]],na.rm=T),
               `min` = quantile(tmpj[[i]],na.rm=T)[["0%"]],
               `max` = quantile(tmpj[[i]],na.rm=T)[["100%"]],
               `q1` = quantile(tmpj[[i]],na.rm=T)[["25%"]],
               `q3` = quantile(tmpj[[i]],na.rm=T)[["75%"]]
            )
            res[[i]][[j]] <- tmp_list
         }
      }
   }

   # CREATE SUMMARY TABLE ----
   tbl <- expand.grid(strata_list,vars,cols)
   colnames(tbl) <- c("strata","var","col")
   tbl$val <- as.character(tbl[,3])

   tbl$val <- gsub("no","number",tolower(tbl$val))
   tbl$val <- gsub("missing","missing",tolower(tbl$val))
   tbl$val <- gsub("iqr","q1 to q3",tolower(tbl$val))
   tbl$val <- gsub("range","min to max",tolower(tbl$val))
   tbl$val <- gsub("95%ci","lcl to ucl",tolower(tbl$val))

   for(i in 1:nrow(tbl)){
      tmp <- res[[tbl[i,2]]][[tbl[i,1]]]
      for(j in 2:length(tmp)){
         tbl$val[[i]] <- gsub(names(tmp)[j],
                              round(as.numeric(tmp[[j]]),digits=2),tbl$val[i])
      }
   }

   tbl <- reshape(data = tbl, idvar = c("var","strata"),
                  timevar = "col", direction = "wide")
   tbl <- reshape(data = tbl, idvar = c("var"),
                  timevar = "strata", direction = "wide")
   colnames(tbl) <- gsub("val.","",colnames(tbl))
   tbl$var <- as.character(tbl$var)
   tbl <- rbind(stringr::str_split_fixed(colnames(tbl), "\\.", 2)[,1],tbl)
   colnames(tbl) <- stringr::str_split_fixed(colnames(tbl), "\\.", 2)[,2]

   # ADD STATISTICS ----
   if(any(!is.na(tests))){
      tsts <- NULL
      for(i in vars){
         tmp <- NULL
         # NUMERIC + 2 GROUPS
         if(class(dfi[[i]]) %in% c("numeric","integer") &
            length(strata_list) == 2){

            tmp$var <- i

            #T-TEST
            if(any(grepl("t.test.unpaired",tolower(tests)))){
               ttest <- suppressWarnings(
                  t.test(dfi[[i]][dfi[[strata]] == strata_list[1]],
                         dfi[[i]][dfi[[strata]] == strata_list[2]])
               )
               tmp$`Mean difference (95%CI)` <- paste0(
                  round(ttest$estimate[[1]]-ttest$estimate[[2]],digs),
                  " (", round(ttest$conf.int[1],digs), " to ",
                  round(ttest$conf.int[2],digs),")"
               )
               tmp$`p-value` <- round(ttest$p.value,digs_p)
            }
            #WILCOX TEST
            if(any(grepl("wilcox.test.unpaired",tolower(tests)))){
               wilcoxtest <- suppressWarnings(
                  wilcox.test(
                     dfi[[i]][dfi[[strata]] == strata_list[1]],
                     dfi[[i]][dfi[[strata]] == strata_list[2]],conf.int = T)
               )
               tmp$`Median difference (HL95%CI)` <- paste0(
                  round(wilcoxtest$estimate[[1]],digs),
                  " (", round(wilcoxtest$conf.int[1],digs), " to ",
                  round(wilcoxtest$conf.int[2],digs),")"
               )
               tmp$`p-value ` <- round(wilcoxtest$p.value,digs_p)
            }
            #PAIRED WILCOX TEST
            if(any(grepl("wilcox.test.paired",tolower(tests)))){
               wilcoxtest <- suppressWarnings(
                  wilcox.test(
                     dfi[[i]][dfi[[strata]] == strata_list[1]],
                     dfi[[i]][dfi[[strata]] == strata_list[2]],paired=T,conf.int = T)
               )
               tmp$`Median difference (HL95%CI)` <- paste0(
                  round(wilcoxtest$estimate[[1]],digs),
                  " (", round(wilcoxtest$conf.int[1],digs), " to ",
                  round(wilcoxtest$conf.int[2],digs),")"
               )
               tmp$`p-value ` <- round(wilcoxtest$p.value,digs_p)
            }
            #AUC
            if(any(grepl("auc",tolower(tests)))){
               auctest1 <- suppressMessages(
                  pROC::auc(as.formula(paste0(strata," ~ ", i)),
                            data=dfi,direction="<")
               )
               auctest2 <- suppressMessages(
                  pROC::auc(as.formula(paste0(strata," ~ ", i)),
                            data=dfi,direction=">")
               )
               if(auctest1 < auctest2) auctest1 <- auctest2
               tmp$`AUC (95%CI)` <- paste0(
                  round(pROC::ci.auc(auctest1)[2],digs),
                  " (", round(pROC::ci.auc(auctest1)[1],digs), " to ",
                  round(pROC::ci.auc(auctest1)[3],digs),")"
               )
            }
            tsts <- rbind(tsts,tmp)
         }
      }
      tsts <- data.frame(tsts,check.names = F)
      tsts <- rbind(colnames(tsts),tsts)
      colnames(tsts)[2:ncol(tsts)] <- "Statistics"
      tsts$var[[1]] <- "var"

      #CREATE FINAL TABLE
      colnames(tbl)[1] <- "var"

      tbl <- suppressWarnings(merge(tbl,tsts,by="var",all.x=T))
   }
   tbl[tbl[,1] == "var",1] <- " "
   tbl <- tbl[order(tbl[,1]),]
   colnames(tbl) <- stringr::str_split_fixed(colnames(tbl), "\\.", 2)[,1]
   colnames(tbl)[duplicated(colnames(tbl))] <- " "
   colnames(tbl)[1] <- " "

   return(tbl)

}

# ==== DOCUMENTATION ====

#' Assess dilations from `PLR3000`-output.
#'
#' `dilations()` is a function which converts longformat recording of pupillary size and uses markers to generate a dataframe, plot, and markdown output to inspect the results.
#'
#' @name dilations
#'
#' @usage dilations(pupils, markers,
#'   remove_markers = NULL, add_markers = NULL,
#'   not_assess = NULL, artefacts = c(0.55,9.95),
#'   time_assess = c(`1` = 10, `3` = 5),
#'   sig.level = 0.05, min_change = NULL)
#'
#' @param pupils recording of pupillary function. long format dataframe with at least three columns: record_id, time, and size.
#' @param markers time of markers. long format dataframe with at least two columns: record_id and time.
#' @param remove_markers markers which should be removed. long format dataframe with at least two columns: record_id and time. The time column need one decimal.
#' @param add_markers markers which should be added. long format dataframe with at least two columns: record_id and time. The time column need one decimal.
#' @param not_assess a list of record ids which should no be assessed.
#' @param artefacts a list of the limits of the artefacts. The first number in the list is the minimum size to be assessed and the second is the maximum size to be assessed
#' @param time_assess This named list define the number of seconds which should be used in the assessment of dilatios. The name is the number of periods-of-interest and the value is the seconds.
#' @param sig.level This is the significance level to be used when comparing the size of the period-of-interest. The significance level corresponds to the Wilcox.test used.
#' @param min_change This is the minimum size of mm which needs to change before a dilation can be identified. Default is no minimum requirement.
#'
#' @return Returns a nested list one dataframe of the results ($dilations), plot ($plot$id\[record id\]), and a markdown output ($plot$markdown$id\[record_id\]). The dilations dataframe include the following columns: Record ID (record_id); Patient ID (pt_id); Date (date); pupil side (side); start of the period (min); end of the period (max); length of period (rec_length); number of measurements (n); median size (median); P value when comparing with the previous period (p_before); P value when comparing with the following period (p_after); and if dilation is identified (dilation, 1 is successful dilation and 0 is no dilation).
#'
#' @examples
#'  \dontrun{
#'    recordings <- PLR3000("C:/PLR3000/R_20200105_205901.xls")
#'    dilations <- dilations(recordings$pupils,recordings$markers)
#'
#'    # The dataframe of the results
#'    dilations$dilations
#'
#'    # The plot of one of the recordings
#'    dilations$plot$id833
#'
#'    # The markdown output of one of the recordings
#'    dilations$markdown$id833
#'
#'  }
#'
#' @importFrom stats median
#' @importFrom stats wilcox.test
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_vline
#' @importFrom ggplot2 aes_string
#' @importFrom ggplot2 annotate
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 theme_classic
#' @importFrom ggplot2 ylab
#' @importFrom ggplot2 xlab
#' @export
#
# ==== FUNCTION ====

dilations <- function(pupils, markers,
                      remove_markers = NULL, add_markers = NULL,
                      not_assess = NULL, artefacts = c(0.55,9.95),
                      time_assess = c(`1` = 10, `3` = 5), sig.level = 0.05,
                      min_change = NULL){
   results <- NULL

   #Control colnames
   '%!in%' <- function(x,y)!('%in%'(x,y))
   if(any(c("record_id","time","size") %!in% colnames(pupils))){
      stop("Cannot find required columns for 'pupils': 'record_id', 'time', and 'size'")
   }
   if(any(c("record_id","time") %!in% colnames(markers))){
      stop("Cannot find required columns for 'markers': 'record_id', 'time'")
   }
   if(!is.null(add_markers)){
      if(!is.data.frame(add_markers)) stop("'add_markers' is not a dataframe")
      if(ncol(add_markers) != 2) stop("'add_markers' need two columns: 'record_id' and 'time'")
   }
   if(!is.null(remove_markers)){
      if(!is.data.frame(remove_markers)) stop("'remove_markers' is not a dataframe")
      if(ncol(remove_markers) != 2) stop("'remove_markers' need two columns: 'record_id' and 'time'")
   }

   for(i in unique(pupils$record_id)){
      #Do not continue
      if(i %in% not_assess) next;

      tmp <- pupils[pupils$record_id == i,c("time","size")]
      tmp[] <- lapply(tmp,as.numeric)
      tmp <- tmp[!duplicated(tmp),]
      tmp_m <- markers[markers$record_id == i,c("time"),]
      tmp_m <- unique(tmp_m)
      tmp_m <- round(as.numeric(tmp_m),1)

      if(!is.null(remove_markers)){
         tmp_remove <- as.numeric(remove_markers[[2]][remove_markers[[1]] == i])
         tmp_m[which(tmp_m %in% tmp_remove)] <- NA
      }
      if(!is.null(add_markers)){
         tmp_add <- as.numeric(add_markers[[2]][add_markers[[1]] == i])
         tmp_m <- c(tmp_m,tmp_add)
      }
      tmp_m <- tmp_m[!is.na(tmp_m)]
      tmp_m2 <- NULL
      for(j in 1:length(tmp_m)){
         tmp_m2 <- c(tmp_m2,tmp$time[which(round(as.numeric(tmp$time),1) %in% tmp_m[j])][1])
      }
      tmp_m <- tmp_m2
      tmp_m <- tmp_m[order(tmp_m)]
      tmp_m <- data.frame(tmp_m)
      colnames(tmp_m) <- "time"
      tmp_m$period <- 1:nrow(tmp_m)

      tmp <- merge(tmp,tmp_m,by="time",all.x=T)
      tmp <- tmp[order(as.numeric(tmp$time)),]
      tmp[1,"period"] <- 0
      tmp$period[is.na(tmp$period)] <- 0
      tmp$period_2 <- cumsum(c(0,as.numeric(diff(tmp$period))!=0))
      tmp$period <- round((tmp$period_2)/2)
      tmp$period_2 <- NULL

      #DELETE ARTEFACTS
      tmp$size[tmp$size < artefacts[[1]]] <- NA
      tmp$size[tmp$size > artefacts[[2]]] <- NA

      even <- nrow(tmp_m) %% 2 == 0
      t <- NULL
      tmp$assessing <- NA
      for(m in unique(tmp$period)){
         t_min <- min(tmp$time[tmp$period == m])
         t_max <- max(tmp$time[tmp$period == m])
         t_rec_length <- t_max-t_min
         t_n <- length(tmp$time[tmp$period == m])
         if(m %% 2 == 1){
            t_assess <- unname(time_assess[names(time_assess) == (length(unique(tmp$period))-1)/2])
            if(length(t_assess) == 1){
               before <- tmp$size[tmp$period == m-1 &
                                     tmp$time > max(tmp$time[tmp$period == m-1])-t_assess]
               current <- tmp$size[tmp$period == m &
                                      tmp$time < min(tmp$time[tmp$period == m])+t_assess]
               after <- tmp$size[tmp$period == m+1 &
                                    tmp$time < min(tmp$time[tmp$period == m+1])+t_assess]
               tmp$assessing[tmp$period == m-1 &
                                tmp$time > max(tmp$time[tmp$period == m-1])-t_assess] <- before
               tmp$assessing[tmp$period == m &
                                tmp$time < min(tmp$time[tmp$period == m])+t_assess] <- current
               tmp$assessing[tmp$period == m+1 &
                                tmp$time < min(tmp$time[tmp$period == m+1])+t_assess] <- after
            }else{
               before <- tmp$size[tmp$period == m-1]
               current <-  tmp$size[tmp$period == m]
               after <- tmp$size[tmp$period == m+1]
               tmp$assessing <- tmp$size
            }
            before_median <- median(before,na.rm=T)
            current_median <- median(current,na.rm=T)
            after_median <- median(after,na.rm=T)


            if(even & length(before) > 2 & length(current) > 2){
               p_before <- wilcox.test(before,current)$p.value}else{p_before <- NA}
            if(even & length(after) > 2 & length(current) > 2){
               p_after <- wilcox.test(after,current)$p.value}else{p_after <- NA}

            if(is.null(min_change)) min_change <- 0

            dilation <- (current_median > before_median+min_change &
                            current_median > after_median+min_change &
                            p_before < sig.level & p_after < sig.level)*1
         }else{
            current <-  tmp$size[tmp$period == m]
            current_median <- median(current,na.rm=T)
            p_before <- NA
            p_after <- NA
            dilation <- NA
         }

         if(any(colnames(pupils) == "pt_id")){
            pt_id <- unique(pupils[pupils$record_id == i,c("pt_id")])[[1]]
         }else{ pt_id <- NA }
         if(any(colnames(pupils) == "date")){
            date <- unique(pupils[pupils$record_id == i,c("date")])[[1]]
         }else{ date <- NA }
         if(any(colnames(pupils) == "side")){
            side <- unique(pupils[pupils$record_id == i,c("side")])[[1]]
         }else{ side <- NA }

         to_t <- c(i, pt_id, date, side,t_min,t_max,t_rec_length,t_n,
                   current_median,p_before,p_after,dilation)
         t <- rbind(t,to_t)
      }
      t <- data.frame(t)
      colnames(t) <- c("record_id","pt_id","date","side","min","max",
                       "rec_length","n","median","p_before","p_after","dilation")
      results$dilations <- rbind(results$dilations,t)

      anner <- round(tmp_m$time,1)
      n_occur <- data.frame(table(anner))
      n_occur$label <- n_occur$anner <- as.character(n_occur$anner)
      n_occur$label[n_occur$Freq > 1] <- paste0(n_occur$anner[n_occur$Freq > 1],"\n(n=",n_occur$Freq[n_occur$Freq > 1],")")
      n_occur$color <- "red"
      n_occur$color[n_occur$Freq > 1] <- "#8b0000"

      t$min <- as.numeric(t$min)
      t$max <- as.numeric(t$max)

      results$plot[[paste0("id",i)]] <- ggplot() + geom_vline(aes_string(xintercept = tmp_m$time), color="red") +
         annotate("label",x = as.numeric(n_occur$anner), y=Inf,
                  label=n_occur$label, vjust=1.1, color="red") +
         annotate("text",x = rowMeans(t[,c("min","max")]), y=Inf,
                  label=t$dilation, vjust=3, color="blue") +
         annotate("text",x = -Inf, y=Inf,
                  label="Dilation", vjust=3, hjust=0, color="blue") +
         geom_line(aes_string(x=tmp$time,y=tmp$assessing)) +
         geom_line(aes_string(x=tmp$time,y=tmp$size), alpha=0.1) + theme_classic() +
         ylab("mm") + xlab("time")

      results$markdown[[paste0("id",i)]] <-
         unlist(c("\n\n **Record ID:**", i, "| **Patient ID:**", pt_id,
                  "\n\n **Date:**", substr(date,1,16), "| **Pupil:**", side,
                  "\n\n **Markers:**", paste(round(tmp_m$time,1),collapse=" | "),
                  "\n\n **Medians:**", paste0(t$median, collapse= " | "),
                  "\n\n **P-values before:**", paste0(round(as.numeric(t$p_before),3), collapse= " | "),
                  "\n\n **P-values after:**", paste0(round(as.numeric(t$p_after),3), collapse= " | "),
                  "\n\n **Period length:**", paste0(round(as.numeric(t$rec_length)), collapse= " | ")))
   }
   rownames(results$dilations) <- c(1:nrow(results$dilations))

   return(results)

}

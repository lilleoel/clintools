# ==== DOCUMENTATION ====

#' Quiet any output (cdm.miss)
#'
#' `cdm.miss()` is a small function which suppresses any output
#'
#' @name cdm.miss
#'
#' @usage cdm.miss(df, id, cols, date, lostFU, filter)
#'
#' @param df      dateframe to be assessed for missing data
#' @param id      column-name for unique id's
#' @param cols    columns to be assessed for missing data
#' @param date    column with the date of follow-up, i.e. when data is missing
#' @param lostFU  column for patients lost to follow up, TRUE/FALSE in the column
#' @param filter  how many should be shown in figures - 'all' for all, 'waiting'
#' for those with missing or waiting for data, and 'missing' for only those
#' with missing data
#'
#' @return Returns a full markdown output.
#'
#' @examples
#' \dontrun{
#'    cdm.miss(data,id=idcols[[1]],cols=missing.cols,lostFU="lostFU",
#'    date = "follow_up_date", filter="missing")
#' }
#'
#' @importFrom ggplot2 aes geom_tile geom_text scale_fill_manual scale_color_manual
#' @importFrom ggplot2 scale_x_discrete scale_y_discrete theme element_blank
#' @importFrom ggplot2 element_text margin
#'
#' @export
#
# ==== FUNCTION ====

cdm.miss <- function(df, id, cols, date = NULL, lostFU = NULL, filter = "all"){
   df <- data.frame(df,check.names = F)
   if(length(cols) > 25) stop("No more than 25 columns can be monitored")

   #Create new dataframe
   if(is.null(date)){ date <- "date"; df$date <- Sys.Date()-1 }
   if(is.null(lostFU)){ lostFU <- "lostFU"; df$lostFU <- F}
   tmp <- df[,c(id,cols,date,lostFU)]
   for(i in cols) tmp[,i] <- is.na(tmp[,i])*1

   #Summarise missing
   if(length(cols) == 0){  stop("No columns defined to assess missing data")
   }else if(length(cols) == 1){ tmp$Missing <- tmp[,cols]
   }else{ tmp$Missing <- rowSums(tmp[,cols]) }

   if(!is.null(names(cols))){
      cols <- c(`Missing`="Missing",cols)
   }else{
      cols <- c("Missing",cols)
   }

   prntperc <- function(x) paste0("(",round(sum(x*100)),"%)")

   #Text output
   cat("**In total:**", nrow(tmp), "participants are included, of them \n\n",
       "*", sum(tmp$Missing == 0), prntperc(sum(tmp$Missing == 0)/nrow(tmp)), "have complete data  *(green fields)*\n",
       "*", sum(tmp$Missing[tmp[[date]] >= Sys.Date() & tmp[[lostFU]] == F] > 0), prntperc(sum(tmp$Missing[tmp[[date]] >= Sys.Date() & tmp[[lostFU]] == F] > 0)/nrow(tmp)), "are waiting for data input *(yellow fields)*\n",
       "*", sum(tmp$Missing[tmp[[lostFU]] == T] > 0), prntperc(sum(tmp$Missing[tmp[[lostFU]] == T] > 0)/nrow(tmp)), "are lost to follow-up *(blue fields)*\n",
       "*", sum(tmp$Missing[tmp[[date]] < Sys.Date() & tmp[[lostFU]] == F] > 0),  prntperc(sum(tmp$Missing[tmp[[date]] < Sys.Date() & tmp[[lostFU]] == F] > 0)/nrow(tmp)),"have missing data *(red fields)*\n\n")

   #Filter
   if(filter == "missing"){
      tmp <- tmp[tmp$Missing > 0 & tmp[[date]] < Sys.Date() &
                    tmp[[lostFU]] == F,]
   }else if(filter == "waiting"){
      tmp <- tmp[tmp$Missing > 0,]
   }

   #Ensure rounded to 50
   add.n <- ceiling(nrow(tmp)/50)*50-nrow(tmp)
   for(i in 1:add.n) tmp <-rbind(tmp,c(paste(rep(" ",i),collapse=""),rep(NA,ncol(tmp)-1)))
   tmp[,cols] <- lapply(tmp[,cols],as.numeric)

   #Create dataframe for figure
   tmp <- reshape(tmp,direction="long",varying=cols,idvar=id,
                  v.names="variable",sep="")
   if(!is.null(names(cols))){
      tmp$time <- as.factor(names(cols)[tmp$time])
      tmp$time <- factor(tmp$time,levels=names(cols))
   }else{
      tmp$time <- as.factor(cols[tmp$time])
      tmp$time <- factor(tmp$time,levels=cols)
   }


   tmp$fillz[tmp$variable > 0] <- "red"
   tmp$fillz[tmp$variable == 0] <- "green"
   tmp$fillz[tmp[[date]] >= Sys.Date() & tmp$fillz == "red"] <- "yellow"
   tmp$fillz[tmp$variable > 0 & tmp[[lostFU]] == T] <- "blue"
   tmp$fillz[is.na(tmp[[date]])] <- "white"

   tmp$colz[!is.na(tmp[[date]])] <- "black"
   tmp$colz[is.na(tmp[[date]])] <- "white"

   tmp$labelz[tmp$time == "Missing"] <- tmp$variable[tmp$time == "Missing"]
   tmp$labelz[tmp$labelz == 0 & !is.na(tmp$labelz)] <- NA
   tmp$labelz[tmp$fillz == "yellow" | tmp$fillz == "blue"] <- NA

   #Order
   tmp[[id]] <- as.factor(tmp[[id]])
   newlvls <- unique(tmp[!is.na(tmp$variable) & order(tmp[[id]]),id])
   if(add.n > 0){
      tmplvls2 <- levels(tmp[[id]])[c(1:add.n)]
      newlvls <- c(as.character(newlvls),tmplvls2)
   }
   tmp[[id]] <- factor(tmp[[id]], levels=newlvls)

   pts <- levels(tmp[[id]])

   for(i in 1:(length(pts)/50)){
      tmp2 <- tmp[which(tmp[[id]] %in% pts[c(((i-1)*50+1):(i*50))]),]
      suppressWarnings(
         print(
            ggplot(tmp2,
               aes(x=tmp2[["time"]],y=get(id),fill=tmp2[["fillz"]],
               label=tmp2[["labelz"]], color=tmp2[["colz"]])) +
            geom_tile() +
            geom_text(size=2.5,color="black") +
            scale_fill_manual(
               values=c(`red`="#FF5733",`green`="#50C878",`yellow`="#FFEA00",
               `blue`="#6495ED",`white`="#FFFFFF")) +
            scale_color_manual(
               values=c(`black`="black",`white`="#FFFFFF",`none`="")) +
            scale_x_discrete(position = "top") +
            scale_y_discrete(labels=function(x) gsub(" ", "", x, fixed=TRUE),
               limits=rev) +
            theme_classic() +
            theme(legend.position = "none", axis.title = element_blank(),
               axis.line = element_blank(), axis.ticks.y = element_blank(),
               axis.text.x = element_text(angle=60,hjust=0),
               plot.margin = margin(r=25))
            )
      )
      cat("\n\n")
   }
}

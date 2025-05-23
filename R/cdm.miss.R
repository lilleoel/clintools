# ==== DOCUMENTATION ====

#' Quiet any output (cdm.miss)
#'
#' `cdm.miss()` is a small function which suppresses any output
#'
#' @name cdm.miss
#'
#' @usage cdm.miss(df, id, cols, fudate, lostFU, filter, blind, caption)
#'
#' @param df      dateframe to be assessed for missing data
#' @param id      column-name for unique id's
#' @param cols    columns to be assessed for missing data
#' @param fudate    column with the date of follow-up, i.e. when data is missing
#' @param lostFU  column for patients lost to follow up, TRUE/FALSE in the column
#' @param filter  how many should be shown in figures - 'all' for all, 'waiting'
#' for those with missing or waiting for data, and 'missing' for only those
#' with missing data
#' @param blind boolean if TRUE, participant IDs will be blinded.
#' @param caption boolean to add or remove a small description of the figure.
#'
#' @return Returns a full markdown output.
#'
#' @examples
#' \dontrun{
#'    cdm.miss(data,id=idcols[[1]],cols=missing.cols,lostFU="lostFU",
#'    fudate = "follow_up_date", filter="missing")
#' }
#'
#' @importFrom ggplot2 aes geom_tile geom_text scale_fill_manual scale_color_manual
#' @importFrom ggplot2 scale_x_discrete scale_y_discrete theme element_blank
#' @importFrom ggplot2 element_text margin
#'
#' @export
#
# ==== FUNCTION ====

# df <- tmp_f
# id="pt_id"
# cols=form_longnames
# fudate = names(form_longnames)
# lostFU = "lostfu"
# filter="missing"
#  filter = "all"; blind = F

cdm.miss <- function(df, id, cols, fudate = NULL, lostFU = NULL, filter = "all", blind = F, caption=T){
   df <- data.frame(df,check.names = F)
   if(length(cols) > 25) stop("No more than 25 columns can be monitored")

   #Create new dataframe
   if(is.null(fudate)){
      fudate <- paste0("fudate",1:length(cols))
      df[,paste0("fudate",1:length(cols))] <- as.character(Sys.Date()-1)
      df[,paste0("fudate",1:length(cols))] <- lapply(df[,paste0("fudate",1:length(cols))], as.Date)
   }else if(length(fudate) == 1){
      df[,paste0("fudate",1:length(cols))] <- as.character(df[[fudate]])
      df[,paste0("fudate",1:length(cols))] <- lapply(df[,paste0("fudate",1:length(cols))], as.Date)
      fudate <- paste0("fudate",1:length(cols))
   }else{
      df[,paste0("fudate",1:length(cols))] <- df[,fudate]
      fudate <- paste0("fudate",1:length(cols))
   }
   if(is.null(lostFU)){ lostFU <- "lostFU"; df$lostFU <- F}
   tmp <- df[,c(id,cols,fudate,lostFU)]

   if(blind){
      tmp[[id]] <- as.factor(tmp[[id]])
      blinders <- NULL
      for(i in 1:10000){
         blinders <- c(blinders,paste0(sample(c(letters,LETTERS),5,replace=T),collapse=""))
      }
      levels(tmp[[id]]) <- unique(blinders)[1:length(levels(tmp[[id]]))]
      tmp[[id]] <- as.character(tmp[[id]])
   }

   # 0 complete; 1 missing; 2 waiting; 3 lostFU
   for(i in 1:length(cols)){
      tmp[,cols[i]] <- is.na(tmp[,cols[i]])*1
      tmp[[fudate[i]]] <- as.numeric(round(
         difftime(Sys.Date(),as.Date(tmp[[fudate[i]]]),units="days")))
      tmp[tmp[[fudate[i]]] < 0 & tmp[,cols[i]] == 1,cols[i]] <- 2
      tmp[tmp[[lostFU]] & tmp[[cols[i]]] > 0,cols[i]] <- 3

      tmp[tmp[[cols[i]]] != 1,fudate[i]] <- NA
   }

   #Summarise missing
   if(length(cols) == 0){  stop("No columns defined to assess missing data")
   }else if(length(cols) == 1){
      tmp$`Missing for...` <- paste(tmp[[fudate]],"days")
   }else{
      tmp$`Missing for...` <- apply(tmp[,fudate],1,FUN=function(x)ifelse(all(is.na(x)),0,paste(max(x,na.rm=T),"days")))
   }

   tmp$label[rowSums(tmp[,cols]) == 0] <- "Complete"
   tmp$label[rowSums(tmp[,cols] == 2) > 0] <- "Waiting"
   tmp$label[rowSums(tmp[,cols] == 3) > 0] <- "Lost"
   tmp$label[rowSums(tmp[,cols] == 1) > 0] <- "Missing"

   prntperc <- function(x) paste0("(",round(sum(x*100)),"%)")
   #Text output
   cat("**In total:**", nrow(tmp), "participants are included, of them \n\n",
       "*", sum(tmp$label == "Complete"), prntperc(sum(tmp$label == "Complete")/nrow(tmp)), "have complete data  *(green fields)*\n",
       "*", sum(tmp$label == "Waiting"), prntperc(sum(tmp$label == "Waiting")/nrow(tmp)), "are waiting for data input *(yellow fields)*\n",
       "*", sum(tmp$label == "Lost"), prntperc(sum(tmp$label == "Lost")/nrow(tmp)), "are lost to follow-up *(blue fields)*\n",
       "*", sum(tmp$label == "Missing"),  prntperc(sum(tmp$label == "Missing")/nrow(tmp)),"have missing data *(red fields)*\n\n")

   #Filter
   if(filter == "missing"){
      tmp <- tmp[tmp$label == "Missing",]
   }else if(filter == "waiting"){
      tmp <- tmp[tmp$label != "Complete",]
   }

   tmp$label[rowSums(tmp[,cols]) == 0] <- "Complete"
   tmp$label[rowSums(tmp[,cols] == 2) > 0] <- "Waiting"
   tmp$label[rowSums(tmp[,cols] == 3) > 0] <- "Lost"
   tmp$label[rowSums(tmp[,cols] == 1) > 0] <- "Missing"

   #Input information about field
   for(i in 1:length(cols)){
      tmp[tmp[[cols[i]]] == 0,cols[i]] <- "Complete"
      tmp[tmp[[cols[i]]] == 1,cols[i]] <- "Missing"
      tmp[tmp[[cols[i]]] == 2,cols[i]] <- "Waiting"
      tmp[tmp[[cols[i]]] == 3,cols[i]] <- "Lost"
   }

   #Ensure rounded to 50
   add.n <- ceiling(nrow(tmp)/50)*50-nrow(tmp)
   for(i in 1:add.n) tmp <-rbind(tmp,c(paste(rep(" ",i),collapse=""),rep(NA,ncol(tmp)-1)))

   if(!is.null(names(cols))){
      cols <- c(`Missing for...`="Missing for...",cols)
   }else{
      cols <- c("Missing for...",cols)
   }

   #Control if no missing data
   if(!is.null(tmp[[id]])){
      #Create dataframe for figure
      tmp <- reshape(tmp[,c(id,"label",cols)],direction="long",varying=cols,idvar=id,
                     v.names="variable",sep="")
      tmp$time <- as.factor(cols[tmp$time])
      tmp$time <- factor(tmp$time,levels=cols)

      tmp$fillz[tmp$variable == "Missing"] <- "red"
      tmp$fillz[tmp$variable == "Complete"] <- "green"
      tmp$fillz[tmp$variable == "Waiting"] <- "yellow"
      tmp$fillz[tmp$variable == "Lost"] <- "blue"
      tmp$fillz[is.na(tmp$variable)] <- "white"

      tmp$fillz[tmp$time == "Missing for..." & tmp$label == "Missing"] <- "red"
      tmp$fillz[tmp$time == "Missing for..." & tmp$label == "Complete"] <- "green"
      tmp$fillz[tmp$time == "Missing for..." & tmp$label == "Waiting"] <- "yellow"
      tmp$fillz[tmp$time == "Missing for..." & tmp$label == "Lost"] <- "blue"

      tmp$colz[!is.na(tmp$variable)] <- "black"
      tmp$colz[is.na(tmp$variable)] <- "white"

      tmp$labelz[tmp$time == "Missing for..."] <- tmp$variable[tmp$time == "Missing for..."]
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
         out <- ggplot(tmp2,
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
                  plot.margin = margin(r=25),
                  plot.subtitle = element_text(face="italic", hjust=0,size = 9),
                  plot.title.position = "plot")

         if(caption & i == 1){
            out <- out + labs(subtitle="Overview of each participant and their completeness. Participants with all forms completed are not shown.")
         }

         suppressWarnings(print(out))
         cat("\n\n")
      }
   }
}

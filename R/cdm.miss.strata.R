# ==== DOCUMENTATION ====

#' Quiet any output (cdm.miss)
#'
#' `cdm.miss.strata()` is a small function which suppresses any output
#'
#' @name cdm.miss.strata
#'
#' @usage cdm.miss.strata(df, id, cols, strata, fudate, lostFU, filter, blind, n_sites, setting)
#'
#' @param df      dateframe to be assessed for missing data
#' @param id      column-name for unique id's
#' @param cols    columns to be assessed for missing data
#' @param strata  colum to aggregate missingness
#' @param fudate    column with the date of follow-up, i.e. when data is missing
#' @param lostFU  column for patients lost to follow up, TRUE/FALSE in the column
#' @param filter  how many should be shown in figures - 'all' for all, 'waiting'
#' for those with missing or waiting for data, and 'missing' for only those
#' with missing data
#' @param blind boolean if TRUE, participant IDs will be blinded.
#' @param n_sites number of sites presented per figure
#' @param setting setting if it is a full report "full" or if it a public report "short" or if it is site specific where the site name is set, e.g. "SKM".
#' @param caption boolean to add or remove a small description of the figure.
#'
#' @return Returns a full markdown output.
#'
#' @examples
#' \dontrun{
#'    cdm.miss.strata(data,id=idcols[[1]],strata="site",
#'    cols=missing.cols,lostFU="lostFU",
#'    fudate = "follow_up_date", filter="missing")
#' }
#'
#' @importFrom ggplot2 scale_fill_gradient
#'
#' @export
#
# ==== FUNCTION ====

# fudate = NULL; lostFU = NULL; filter = "all"; blind = F

cdm.miss.strata <- function(df, id, cols, strata, fudate = NULL, lostFU = NULL, filter = "all", blind = F, n_sites=15,setting="full", caption=T){

   # # TEST
   # df = dff[!is.na(dff$rand_date),]
   # id = "pt_id"
   # cols = form_longnames
   # strata = "site"
   # fudate = names(form_longnames)
   # lostFU = "lostfu"
   # filter = "missing"
   # blind=F
   # n_sites=13
   # setting=report_type
   # TEST


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
   }else{
      df[,paste0("fudate",1:length(cols))] <- df[,fudate]
      fudate <- paste0("fudate",1:length(cols))
   }
   if(is.null(lostFU)){ lostFU <- "lostFU"; df$lostFU <- F}
   tmp <- df[,c(id,strata,cols,fudate,lostFU)]

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
      tmp[tmp[[fudate[i]]] >= Sys.Date() & tmp[,cols[i]] == 1,cols[i]] <- 2
      tmp[tmp[[lostFU]] & tmp[[cols[i]]] > 0,cols[i]] <- 3
   }

   #Input information about field
   for(i in 1:length(cols)){
      tmp[tmp[[cols[i]]] == 0,cols[i]] <- "Complete"
      tmp[tmp[[cols[i]]] == 1,cols[i]] <- "Missing"
      tmp[tmp[[cols[i]]] == 2,cols[i]] <- "Waiting"
      tmp[tmp[[cols[i]]] == 3,cols[i]] <- "Lost"
   }

   # All new data for overall
   tmp_all <- tmp
   tmp_all[[strata]] <- "All "
   tmp <- rbind(tmp_all,tmp)
   # Create dftmp_eligible# Create df with percentage missingness of all

   if(setting == "short"){
      tmp <- tmp[tmp[[strata]] == "All ",]
   }else if(setting != "full"){
      tmp <- tmp[tmp[[strata]] %in% c("All ",setting),]
   }

   tmp1 <- aggregate(tmp[[cols[i]]],by=list(tmp[[strata]]),length)
   colnames(tmp1) <- c(strata,"n")
   for(i in 1:length(cols)){
      tmp2 <- aggregate(tmp[[cols[i]]] == "Missing",
                        by=list(tmp[[strata]]),sum)
      colnames(tmp2) <- c(strata,cols[i])
      tmp1 <- merge(tmp1,tmp2,all=T)
      tmp1[[cols[i]]] <- tmp1[[cols[i]]]/tmp1$n
   }
   tmp1[[strata]] <- paste0(tmp1[[strata]]," (n=",tmp1$n,")")

   tmp <- tmp1
   id <- strata

   # ADD EMPTY LINE BETWEEN ALL AND REST
   tmp <- rbind(tmp[1,],rbind(c(paste(" ",collapse=""),rep(NA,ncol(tmp)-1)),
                             tmp[2:nrow(tmp),]))

   #Ensure rounded to 50
   add.n <- ceiling(nrow(tmp)/n_sites)*n_sites-nrow(tmp)
   for(i in 1:add.n) tmp <-rbind(tmp,c(paste(rep(" ",i+2),collapse=""),rep(NA,ncol(tmp)-1)))
   siteorder <- tmp[[id]]

   #Control if no missing data
   if(!is.null(tmp[[id]])){
      #Create dataframe for figure
      tmp <- reshape(tmp[,c(id,cols)],direction="long",varying=cols,idvar=id,
                     v.names="variable",sep="")
      tmp$time <- as.factor(cols[tmp$time])
      tmp$time <- factor(tmp$time,levels=cols)

      tmp$colz[!is.na(tmp$variable)] <- "black"
      tmp$colz[is.na(tmp$variable)] <- "white"

      tmp$label[!is.na(tmp$variable)] <- paste0(round(as.numeric(tmp$variable[!is.na(tmp$variable)])*100),"%")

      #Order
      tmp[[id]] <- as.factor(tmp[[id]])
      tmp[[id]] <- factor(tmp[[id]],levels=siteorder)
      newlvls <- unique(tmp[!is.na(tmp$variable) & order(tmp[[id]]),id])
      # if(add.n > 0){
      #    tmplvls2 <- levels(tmp[[id]])[c(1:add.n)]
      #    newlvls <- c(as.character(newlvls),tmplvls2)
      # }
      # tmp[[id]] <- factor(tmp[[id]], levels=newlvls)

      pts <- levels(tmp[[id]])

      tmp$variable[is.na(tmp$variable)] <- 0
      tmp$variable <- as.numeric(tmp$variable)

      for(i in 1:(length(pts)/n_sites)){
         tmp2 <- tmp[which(tmp[[id]] %in% pts[c(((i-1)*n_sites+1):(i*n_sites))]),]
         out <- ggplot(tmp2,
                aes(x=tmp2[["time"]],y=get(id), color=tmp2[["colz"]],
                    label=tmp2[["label"]], fill=tmp2[["variable"]])) +
            geom_tile() +
            geom_text(size=2.5,color="black") +
            scale_color_manual(
               values=c(`black`="black",`white`="#FFFFFF",`none`="")) +
            scale_fill_gradient(low = "white", high = "red") +
            scale_x_discrete(position = "top") +
            scale_y_discrete(labels=function(x) gsub(" ", "", x, fixed=TRUE),
                             limits=rev) +
            theme_classic() +
            theme(legend.position = "none", axis.title = element_blank(),
                  axis.line = element_blank(), axis.ticks.y = element_blank(),
                  axis.text.x = element_text(angle=60,hjust=0),
                  plot.margin = margin(r=25,t=1),
                  plot.subtitle = element_text(face="italic", hjust=0,size = 9),
                  plot.title.position = "plot")

         if(caption & i == 1){
            out <- out + labs(subtitle="Percentage of participants who have incomplete data after a specified deadline overall and at each site.")
         }


         suppressWarnings(print(out))
         cat("\n\n")
      }
   }
}

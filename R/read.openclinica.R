# ==== DOCUMENTATION ====

#' Create dataframe from Open Clinica (read,openclinica)
#'
#' `read.openclinica()` is a small function which can extract data from Open Clinica
#'
#' @name read.openclinica
#'
#' @usage read.openclinica(trial, link, prefix, ids)
#'
#' @param trial The trialname used in openclinica for extraction
#' @param link The link where to access data
#' @param prefix for each instrument how many characters should be used in the initial part of the colnames
#' @param ids list with ID's which reccur from each instrument
#'
#' @return Returns a dataframe with data from Open Clinica
#'
#' @examples
#' \dontrun{
#' df <- read.openclinica(trial="LightCARE",link = "https://lightcom.ctu.dk/extract/api/data/",
#' ids = c("ssid","site_id"))
#' }
#' @export
#
# ==== FUNCTION ====

read.openclinica <- function(trial, link, prefix = 4, ids){
   linklist <- paste0("list?trialName=",trial)
   linkextract <- paste0("get?trialName=",trial,"&referenceName=")

   json_data <- data.frame(suppressWarnings(jsonlite::fromJSON(paste(readLines(
      paste0(link,"list?trialName=",trial)), collapse=""))))
   json_data <- c(json_data[grepl("reference",colnames(json_data))][1,])

   d <- NULL
   for(i in 1:length(json_data)){
      tmp <- json_data[i]
      tmp_data <- tryCatch(read.csv2(
         paste0(link,"get?trialName=",trial,"&referenceName=",json_data)[i],
         sep="\t",na.strings = ".",fileEncoding="latin1")
         ,error=function(e) e, warning=function(w) w)
      if(any(class(tmp_data) %in% c("error","try-error","warning"))) next

      colnames(tmp_data)[!colnames(tmp_data) %in% ids] <- paste0(substr(tmp,1,prefix),".",colnames(tmp_data)[!colnames(tmp_data) %in% ids])

      if(is.null(d)) d <- tmp_data
      if(!is.null(d)) d <- merge(d,tmp_data,by=ids,all=T)
   }

   #Search for duplicate columns
   dup_cols <- gsub("\\.x$|\\.y$|\\.1","",colnames(d))
   dup_cols <- unique(dup_cols[duplicated(dup_cols)])
   for(i in dup_cols){
      tmp <- d[,grepl(paste0(i),colnames(d))]
      if(ncol(tmp) == 2){
         tmp[is.na(tmp)]<-""
         tmp[,duplicated(as.list(tmp))] <- ""
         x1 <- tmp[[1]]
         x2 <- tmp[[2]]
         d[,c(colnames(tmp)[1])] <- paste0(x1,x2)
         d <- d[,!colnames(d) %in% colnames(tmp)[2:ncol(tmp)]]
      }else if(ncol(tmp) == 3){
         tmp[is.na(tmp)]<-""
         tmp[,duplicated(as.list(tmp))] <- ""
         for(j in 1:nrow(tmp)) tmp[j,c(t(duplicated(t(tmp[j,]))))] <- ""
         x1 <- tmp[[1]]
         x2 <- tmp[[2]]
         x3 <- tmp[[3]]
         d[,c(colnames(tmp)[1])] <- paste0(x1,x2,x3)
         # data[,c(colnames(tmp)[1])] <- unite(tmp, newcol, c(1:ncol(tmp)), sep="")
         d <- d[,!colnames(d) %in% colnames(tmp)[2:ncol(tmp)]]
      }
   }
   colnames(d) <- gsub("\\.x$|\\.y$|\\.1","",colnames(d))


   return(d)
}

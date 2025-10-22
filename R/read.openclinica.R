# ==== DOCUMENTATION ====

#' Create dataframe from Open Clinica (read,openclinica)
#'
#' `read.openclinica()` is a small function which can extract data from Open Clinica
#'
#' @name read.openclinica
#'
#' @usage read.openclinica(trial, link, prefix, ids, metadata)
#'
#' @param trial The trialname used in openclinica for extraction
#' @param link The link where to access data
#' @param prefix for each instrument how many characters should be used in the initial part of the colnames
#' @param ids list with ID's which reccur from each instrument
#' @param metadata is it metadata that should be extracted? T/F
#'
#' @return Returns a dataframe with data from Open Clinica
#'
#' @examples
#' \dontrun{
#' df <- read.openclinica(trial="LightCARE",link = "https://lightcom.ctu.dk/extract/api/data/",
#' ids = c("ssid","site_id"))
#' }
#' @export
#'
#' @importFrom rjson fromJSON
#' @importFrom utils download.file unzip
#' @importFrom plyr rbind.fill
#
# ==== FUNCTION ====
read.openclinica <- function(trial, link, prefix = 4, ids, metadata=F){
   if(!metadata){
      linklist <- paste0("list?trialName=",trial)
      linkextract <- paste0("get?trialName=",trial,"&referenceName=")

      json_data <- data.frame(suppressWarnings(fromJSON(paste(readLines(paste0(link,"list?trialName=",trial)), collapse=""))))
      json_data <- c(json_data[grepl("reference",colnames(json_data))][1,])

      d <- NULL
      for(i in 1:length(json_data)){
         tmp <- json_data[i]
         if(nchar(tmp[[1]]) == 0) next
         tmp_data <- suppressWarnings(tryCatch(readr::read_tsv(
            paste0(link,"get?trialName=",trial,"&referenceName=",json_data)[i],na = ".", show_col_types = F, col_types = readr::cols(.default = readr::col_character()), locale = readr::locale(encoding = "ISO-8859-1"))
            ,error=function(e) e))
         if("error" %in% class(tmp_data)){
            tmp_data <-
               suppressWarnings(tryCatch(readr::read_tsv(
                  paste0(link,"get?trialName=",trial,"&referenceName=",json_data,"&includeStatus=true")[i],na = ".", show_col_types = F, col_types = readr::cols(.default = readr::col_character()), locale = readr::locale(encoding = "ISO-8859-1"))
                  ,error=function(e) e))
         }

         if(is.null(nrow(tmp_data)) || nrow(tmp_data) == 0){
            warning(paste("Error in",json_data[i],"-",i,"-",
                          tmp_data$message,"\n"))
            next
         }

         colnames(tmp_data)[!colnames(tmp_data) %in% ids] <- paste0(substr(tmp,1,prefix),".",colnames(tmp_data)[!colnames(tmp_data) %in% ids])

         if(is.null(d)) d <- tmp_data
         if(!is.null(d)){
            colnames(tmp_data)[colnames(tmp_data) %in% setdiff(colnames(df),ids)] <-
               paste0(colnames(tmp_data)[colnames(tmp_data) %in% setdiff(colnames(df),ids)],".",
                      letters[i])

            d <- merge(d,tmp_data,by=ids,all=T)
         }
      }

      #Search for duplicate columns
      dup_cols <- gsub("\\.[a-z]$|\\.NA$","",colnames(d))
      dup_cols <- unique(dup_cols[duplicated(dup_cols)])
      for(i in dup_cols){
         # Find alle kolonner relateret til det dublerede navn
         tmp <- d[, grepl(paste0("^", i, "(\\.|$)"), colnames(d)), drop = FALSE]

         # Erstat NA med tom string
         tmp[is.na(tmp)] <- ""

         # Fjern dubletter per række (f.eks. "a", "a", "b" => "a", "", "b")
         for (j in 1:nrow(tmp)) {
            row_vals <- tmp[j, ]
            dup_in_row <- duplicated(as.character(row_vals))
            tmp[j, which(dup_in_row)] <- ""
         }

         # Sammenkæd alle kolonneværdier i tmp til én kolonne
         d[, i] <- do.call(paste0, tmp)

         # Fjern de overskydende kolonner
         colnames_to_remove <- setdiff(colnames(tmp), i)
         d <- d[, !colnames(d) %in% colnames_to_remove]
      }
      colnames(d) <- gsub("\\.[a-z]$|\\.NA$","",colnames(d))


      return(d)
   }else{
      zip_url <- paste0(link,"metadata/all?trialName=",trial)
      zip_file <- "metadata.zip"
      download.file(zip_url, zip_file, mode = "wb")
      unzip(zip_file, exdir = "metadata")
      file.remove(zip_file)

      md <- NULL
      for (i in list.files("metadata/")) {
         # Read with tab separator and Latin-1 encoding
         mdtmp <- read.csv2(paste0("metadata/", i), sep = "\t", fileEncoding = "latin1")

         # Check required columns and process
         if (all(c("name", "description", "reponse_set") %in% colnames(mdtmp))) {
            md <- rbind.fill(md, mdtmp)
         }
         md[,c("ordinal","repeating_item")] <- NULL
      }
      return(md)
   }
}

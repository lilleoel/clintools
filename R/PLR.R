# ==== DOCUMENTATION ====

#' NeurOpticsTM PLR-3000 or PLR-4000 pupillometer file to dataframe (PLR)
#'
#' `PLR()` is a function which converts the XLS file imported from the NeurOpticsTM PLR-3000 or PLR-4000 pupillometer to a nested list with two dataframes.
#'
#' @name PLR
#'
#' @usage PLR(filename = NULL, df = NULL, v = "3000")
#'
#' @param filename path to the XLS-file with the measurements
#' @param df the dataframe can also be used for the function if data is already imported.
#' @param v needed to define if it is either "3000" or "4000"
#'
#' @return Returns a list with two dataframe, one with the measurements (pupils) and one with the markers (markers).
#'
#' @examples
#'  \dontrun{
#'    PLR0("C:/PLR3000/R_20200105_205901.xls")
#'  }
#'
#' @importFrom utils read.csv2
#' @export
#
# ==== FUNCTION ====

# filename = "C:/Oel/Artikler/NIA/DK_Poul_pupillometry/PLR3000 eksempel.xls"
# version = "4000"
# tmp <- PLR(filename,version="4000")
PLR <- function(filename=NULL, df=NULL, v = "3000"){
   if(is.null(df) & is.null(filename)){
      stop("Please provide either a dataframe or the filename")
   }
   if(is.null(df) & !is.null(filename)){
      df <- read.csv2(filename, sep="\t", header=FALSE, skip=1, na.strings=c("NA"))
   }
   if(ncol(df) == 1){
      df <- read.csv2(filename, header=FALSE, skip=1, na.strings=c("","NA"))
   }

   df[] <- lapply(df, as.character)

   #Reverse newlines
   linenumber <- suppressWarnings(which(!is.na(as.numeric(df[,2])) | is.na(as.numeric(df[,1]))))
   linenumber <- c(linenumber,which(df[,2] %in% c("UP","DOWN","LEFT","RIGHT","SELECT")))
   linenumber <- c(linenumber,which(is.na(df[,2])))
   linenumber <- linenumber[order(linenumber)]

   if(length(linenumber) > 0){
      df2 <- df[linenumber,]
      df <- df[-linenumber,]
      df2[] <- lapply(df2, as.character)
      temp_ncol <-  ncol(df2)
      temp_df <- as.data.frame(matrix(ncol=temp_ncol, nrow=nrow(df)))
      for(i in c(1:length(linenumber))){
         temp_ln <- linenumber[[i]]-i
         temp_df[temp_ln,] <- df2[i,]
      }
      df <- cbind(df,temp_df)
   }

   #Create the final dataframe
   pupils_data <- NULL
   markers_data <- NULL

   #defining the loop
   for(i in c(1:nrow(df))){
      #Select the data
      temp_raw <- df[i,]

      #Transpose form columns to rows
      temp_raw <- as.data.frame(t(temp_raw))

      #Select information
      if(v == 3000){
         temp_record_id <- as.character(temp_raw[1,])
         temp_patient_id <- as.character(temp_raw[3,])
         temp_date <- as.character(temp_raw[4,])
         temp_pupil <- as.character(temp_raw[6,])
      }else{
         temp_record_id <- as.character(temp_raw[1,])
         temp_patient_id <- as.character(temp_raw[3,])
         temp_date <- as.character(temp_raw[2,])
         temp_pupil <- as.character(temp_raw[5,])
      }

      #Identify number of measurements
      temp_no <- as.numeric(as.character(temp_raw[24,]))

      if(v == 3000){
         #Select time stamps
         temp_x <- temp_raw[c(25:(temp_no+24)),]

         #Select pupil size
         temp_y <- temp_raw[c((temp_no+25):(temp_no+temp_no+24)),]
      }else{
         #Select time stamps
         temp_y <- temp_raw[c(25:(temp_no+24)),]

         #Select pupil size
         temp_x <- temp_raw[c((temp_no+25):(temp_no+temp_no+24)),]
      }

      #Identify number of markers
      temp_no_markers <- as.numeric(as.character(temp_raw[(temp_no+temp_no+25),]))

      if(!is.na(temp_no_markers)){
         if(v == 3000){
            #Select time stamps for markers
            temp_x_markers <- temp_raw[c((temp_no+temp_no+26):(temp_no+temp_no+25+temp_no_markers)),]
            #Select button pushed
            temp_y_markers <- temp_raw[c((temp_no+temp_no+26+temp_no_markers):(temp_no+temp_no+25+temp_no_markers+temp_no_markers)),]
         }else{
            temp_markers <- temp_raw[c((temp_no+temp_no+26):(temp_no+temp_no+25+temp_no_markers+temp_no_markers)),]
            temp_x_markers <- temp_markers[c(1:length(temp_markers)) %% 2 == 1]
            #Select button pushed
            temp_y_markers <- temp_markers[c(1:length(temp_markers)) %% 2 == 0]
         }
      }
      #Convert data to numeric when relevant
      temp_x <- as.numeric(as.character(temp_x))
      temp_y <- as.numeric(as.character(temp_y))
      if(!is.na(temp_no_markers)){
         temp_x_markers <- as.numeric(as.character(temp_x_markers))
      }

      #Create dataframe for dilation data
      temp_pupils <- cbind(temp_x,temp_y)
      temp_pupils <- cbind(temp_record_id,temp_patient_id,temp_date,temp_pupil,temp_pupils)
      temp_pupils <- as.data.frame(temp_pupils)
      colnames(temp_pupils) <- c("record_id","pt_id","date","side","time","size")
      pupils_data <- rbind(pupils_data,temp_pupils)

      #Create marker dataframe
      if(!is.na(temp_no_markers)){
         temp_markers <- cbind(temp_x_markers,temp_y_markers)
         temp_markers <- cbind(temp_record_id,temp_patient_id,temp_date,temp_markers)
         temp_markers <- as.data.frame(temp_markers)

         colnames(temp_markers) <- c("record_id","pt_id","date","time","size")
         markers_data <- rbind(markers_data,temp_markers)
      }


   }

   results <- NULL
   results$pupils <- pupils_data
   results$markers <- markers_data

   if(any(is.na(as.numeric(markers_data[,1])))){
      if(v == 3000){
         message("It might be the wrong version - consider setting v = 4000")
      }else if(v == 4000){
         message("It might be the wrong version - consider setting v = 3000")
      }else{
         message("It might be the wrong version - consider setting v = 3000 or v = 4000")
      }
   }

   return(results)
}

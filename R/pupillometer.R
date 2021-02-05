pupil_import <- function(df){

   dilation_raw[] <- lapply(df, as.character)

   #Reverse newlines
   linenumber <- NULL
   for(i in c(2:7)){
      linenumber <- c(linenumber,which(is.na(dilation_raw[,i])))
   }
   linenumber <- c(linenumber,which(!is.na(dilation_raw[,8])))
   linenumber <- unique(sort(linenumber))

   if(length(linenumber) > 0){
      dilation_raw2 <- dilation_raw[linenumber,]
      dilation_raw <- dilation_raw[-linenumber,]
      dilation_raw2[] <- lapply(dilation_raw2, as.character)
      temp_ncol <-  ncol(dilation_raw2)
      temp_df <- as.data.frame(matrix(ncol=temp_ncol, nrow=nrow(dilation_raw)))
      for(i in c(1:length(linenumber))){
         temp_ln <- linenumber[[i]]-i
         temp_df[temp_ln,] <- dilation_raw2[i,]
      }
      dilation_raw <- cbind(dilation_raw,temp_df)
   }

   #remove empty columns
   dilation_raw[,c(2,4:5,7,8:23)] <- NULL

   #Create the final dataframe
   pupils_data <- NULL

   #defining the loop
   for(i in c(1:nrow(dilation_raw))){
      #Select the data
      temp_raw <- dilation_raw[i,]

      #Transpose form columns to rows
      temp_raw <- as.data.frame(t(temp_raw))

      #Select information
      temp_record_id <- as.character(temp_raw[1,])
      temp_patient_id <- as.character(temp_raw[2,])
      temp_pupil <- as.character(temp_raw[3,])

      #Identify number of measurements
      temp_no <- as.numeric(as.character(temp_raw[4,]))

      #Select time stamps
      temp_x <- temp_raw[c(5:(temp_no+4)),]

      #Select pupil size
      temp_y <- temp_raw[c((temp_no+5):(temp_no+temp_no+4)),]

      #Identify number of markers
      temp_no_markers <- as.numeric(as.character(temp_raw[(temp_no+temp_no+5),]))

      #Select time stamps for markers
      temp_x_markers <- temp_raw[c((temp_no+temp_no+6):(temp_no+temp_no+5+temp_no_markers)),]

      #Select button pushed
      temp_y_markers <- temp_raw[c((temp_no+temp_no+6+temp_no_markers):(temp_no+temp_no+5+temp_no_markers+temp_no_markers)),]

      #Convert data to numeric when relevant
      temp_x <- as.numeric(as.character(temp_x))
      temp_y <- as.numeric(as.character(temp_y))
      temp_x_markers <- as.numeric(as.character(temp_x_markers))

      #Create dataframe for dilation data
      temp_pupils <- cbind(temp_x,temp_y)
      temp_pupils <- cbind(temp_pupil,temp_pupils)
      temp_pupils <- cbind(temp_record_id,temp_pupils)
      temp_pupils <- cbind(temp_patient_id,temp_pupils)

      #Convert from vector to data frame
      temp_pupils <- as.data.frame(temp_pupils)

      #For each marker insert button direction into
      for(j in c(1:temp_no_markers)){
         temp_pupils$marker[temp_pupils$temp_x == temp_x_markers[j]] <- as.character(temp_y_markers[j])
      }

      #Insert into final data frame
      if(is.null(pupils_data)){
         pupils_data <- temp_pupils
      }else{
         pupils_data <- rbind(pupils_data,temp_pupils)
      }
   }

   colnames(pupils_data) <- c("patient_id","record_id","side","time","size","marker")
   return(pupils_data)
}

mx <- function(
   #Dataframes
   df, del_pres = NULL, del_mcav = NULL, trigger = NULL,
   #Calculation settings
   blocksize = 3, epochsize = 20, freq = 1000,
   #Data Quality
   blockmin = 0.5, epochmin = 0.5,
   #Overlapping
   overlapping = FALSE,
   #Output
   output = "period"
){

   #FUNCTIONS ----

   #'runtime' counts how long the script was to execute.
   runtime <- function(x){
      round(as.numeric(difftime(Sys.time(),x, units="secs")),digits=2)
   }

   #Validation of dataframes
   func_validation <- function(df, trigger, del_pres, del_mcav){
      cat("..."); time <- Sys.time()

      #Validation of number of columns in 'df'
      if(ncol(df) != 3){ stop("Recording does not have the required columns, data must be 3 numeric columns with 'time', 'BP', and 'MCAv'") }
      if(sum((sapply(df,class) != "numeric")*1) > 0){ stop("At least one of the columns in the recording dataframe is not numeric") }
      #Validation of number of columns in 'trigger'
      if(!is.null(trigger)) if(ncol(trigger) != 2){ stop("Trigger does not have the required columns, data must be two columns with start and end time for periodes which will be analysed") }
      if(!is.null(trigger)) if(sum((sapply(trigger,class) != "numeric")*1) > 0){ stop("At least one of the columns in the trigger dataframe is not numeric") }
      #Validation of number of columns in 'deleter_p'
      if(!is.null(del_pres)) if(ncol(del_pres) != 2){ stop("Pressure deleter does not have the required columns, data must be two columns with start and end time for periodes which should be deleted") }
      if(!is.null(trigger)) if(sum((sapply(trigger,class) != "numeric")*1) > 0){ stop("At least one of the columns in the pressure deleter dataframe is not numeric") }
      #Validation of number of columns in 'deleter_p'
      if(!is.null(del_mcav)) if(ncol(del_mcav) != 2){ stop("Velocity deleter does not have the required columns, data must be two columns with start and end time for periodes which should be deleted") }
      if(!is.null(trigger)) if(sum((sapply(trigger,class) != "numeric")*1) > 0){ stop(" At least one of the columns in the velocity deleter dataframe is not numeric") }

      #Define data
      colnames(df) <- c("time","pres","mcav")
      df$n <- c(1:nrow(df))
      cat(paste0("Data is validated (", runtime(time)," s)...\n"))
      return(df)
   }

   #Remove measurements outside of the trigger periods.
   func_trigger <- function(df, trigger){
      cat("..."); time <- Sys.time()
      if(!is.null(trigger)) {
         #Name periods
         for(i in c(1:nrow(trigger))){
            start <- trigger[i,1]
            end <- trigger[i,2]
            df$period[df[[1]] >= start & df[[1]] < end] <- i
         }
         #Remove data outside for trigger periods
         df <- df[!is.na(df$period),]
         cat(paste0("Trigger periods (n=", nrow(trigger),") are extracted (", runtime(time)," s)...\n"))
      }else{
         cat("No trigger periods defined, full measurement selected...\n")
         df$period <- 1
      }

      return(df)
   }

   #Remove deleted measurements
   func_deleter <- function(df,del_pres,del_mcav){
      cat("..."); time <- Sys.time()

      #NA inserted in deletion periods
      if(!is.null(del_pres)) for(i in c(1:nrow(del_pres))){
         df[df[[1]] >= del_pres[i,1] & df[[1]] <= del_pres[i,2],2] <- NA
      }
      if(!is.null(del_mcav)) for(i in c(1:nrow(del_mcav))){
         df[df[[1]] >= del_mcav[i,1] & df[[1]] <= del_mcav[i,2],3] <- NA
      }
      if(!is.null(del_pres) & !is.null(del_mcav)) {
         df <- df[!is.na(df[,2]) & !is.na(df[,3]),]
         cat(paste0("Deleter periods (n=", nrow(del_mcav)+nrow(del_pres),") are removed (converted to NA) (", runtime(time)," s)...\n"))
      }else{
         cat(paste0("Deleters are empty, no data is deleted...\n"))
      }

      return(df)
   }

   #Define blocks
   func_blocks <- function(df,freq,blocksize){
      cat("..."); time <- Sys.time()

      temp <- NULL
      for(i in unique(df$period)){
         temp_df <- df[df$time >= min(df$time[df$period == i]) & df$time <= max(df$time[df$period == i]),]
         temp_df$block <- temp_df$n-min(temp_df$n)+1
         temp_df$block <- ceiling(temp_df$block/(blocksize*freq))
         temp <- rbind(temp,temp_df[,c("n","block")])
      }
      df <- merge(df,temp,by="n",all.x=T)

      cat(paste0("Blocks created (", runtime(time)," s)...\n"))
      return(df)
   }

   #Define epochs
   func_epochs <- function(df_block, epochsize, overlapping){
      cat("..."); time <- Sys.time()

      if(!overlapping){
         df$epoch <- ceiling(df$block/epochsize)
      }else{
         temp <- NULL
         for(i in unique(df$period)){
            temp_df <- as.data.frame(cbind(i,
                                           c(1:max(df$block[df$time >= min(df$time[df$period == i]) & df$time <= max(df$time[df$period == i])])),
                                           NA))
            colnames(temp_df) <- c("period","block","epoch")
            for(j in temp_df$block[temp_df$block %% overlapping == 0]){

               temp_df$epoch[temp_df$block > j-epochsize & temp_df$block <= j] <-
                  paste0(temp_df$epoch[temp_df$block > j-epochsize & temp_df$block <= j], ",-", j, "-")

            }
            temp <- rbind(temp,temp_df)
         }
         temp$epoch <- substr(temp$epoch,4,nchar(temp$epoch))
         df <- merge(df,temp,by=c("period","block"),all.x=T)
      }

      cat(paste0("Epochs created (", runtime(time)," s)...\n"))
      return(df)
   }

   #Check for measurements in blocks and blocks in epochs
   func_quality <- function(df, freq, blocksize, blockmin, epochsize, epochmin){

      #Quality control of blocks
      if(!is.null(blockmin)){
         cat("..."); time <- Sys.time()
         temp <- aggregate(df$n,by=list(df$period,df$block),length)
         temp$del <- (temp$x<(freq*blocksize*blockmin))*1
         colnames(temp) <- c("period","block","n","del")
         df <- merge(df,temp[,c("period","block","del")], all.x=T)
         df <- df[df$del != "1",-c(ncol(df))]
         cat(paste0("Quality control - ", sum(temp$del), " block(s) deleted (", runtime(time)," s)...\n"))
      }

      #Quality control of epochs
      if(!is.null(epochmin)){
         cat("..."); time <- Sys.time()
         if(!overlapping){
            temp <- aggregate(df$block,by=list(df$period,df$epoch),function(x) length(unique(x)))
            temp$del <- (temp$x<(epochsize*epochmin))*1
            colnames(temp) <- c("period","epoch","n","del")
            df <- merge(df,temp[,c("period","epoch","del")], all.x=T)
            df <- df[df$del != "1",-c(ncol(df))]
            cat(paste0("Quality control - ", sum(temp$del), " epoch(s) deleted (", runtime(time)," s)...\n"))
         }else{
            temp_del <- 0
            for(i in unique(df$period)){
               temp_epochs <- unique(unlist(strsplit(df$epoch[df$period == i],",")))
               temp_epochs <- temp_epochs[!is.na(temp_epochs)]
               for(j in temp_epochs){
                  if(length(unique(df$block[grep(j,df$epoch[df$period == i])]))<epochsize*epochmin){
                     temp_del <- temp_del+1
                     df$epoch[df$period == i] <- gsub(j,",",df$epoch[df$period == i])
                  }
               }

            }
            cat(paste0("Quality control - ", temp_del, " epoch(s) deleted (", runtime(time)," s)...\n"))
         }

      }
      return(df)
   }

   #Correlations for every epoch
   func_cor <- function(df){
      cat("..."); time <- Sys.time()
      mx <- NULL
      temp_block <- aggregate(df[,c("pres","mcav")],by=list(df$period,df$epoch,df$block),mean)
      colnames(temp_block)[c(1:3)] <- c("period","epoch","block")
      for(i in unique(temp_block$period)){
         temp_period <- temp_block[temp_block$period == i,]
         if(overlapping) j_temp <- unique(unlist(strsplit(temp_period$epoch[temp_period$period == i],",")))
         if(!overlapping) j_temp <- unique(temp_period$epoch[temp_period$period == i])
         j_temp <- j_temp[j_temp != ""]

         for(j in j_temp){
            if(overlapping) temp_epoch <- temp_period[grep(j,temp_period$epoch),]
            if(!overlapping) temp_epoch <- temp_period[temp_period$period == i & temp_period$epoch == j,]
            mx <- rbind(mx,cbind(i,j,cor(temp_epoch$pres,temp_epoch$mcav)))
         }
      }
      cat(paste0("Index calculated (", runtime(time)," s)...\n"))
      return(mx)
   }

   #Output creation
   func_output <- function(df, mx, freq, output, overlapping){
      cat("..."); time <- Sys.time()
      if(output == "period"){

         results <- NULL
         for(i in unique(df$period)){
            if(overlapping) n_epoch <- unique(unlist(strsplit(df$epoch[df$period == i],",")))
            if(!overlapping) n_epoch <- unique(df$epoch[df$period == i])
            n_epoch <- n_epoch[n_epoch != "" & !is.na(n_epoch)]

            temp <- cbind(i,
                          length(n_epoch),
                          length(unique(df$block[df$period == i])),
                          min(df$time[df$period == i]),
                          max(df$time[df$period == i]),
                          mean(df$pres[df$period == i]),
                          mean(df$mcav[df$period == i]),
                          round(100-length(unique(df$n[df$period == i]))/
                                   (freq*(max(df$time[df$period == i])-
                                             min(df$time[df$period == i])))*100, digits=1),

                          mean(as.numeric(mx[mx[,1] == i,3]))
            )
            results <- rbind(results,temp)

         }

      }else if(output == "epoch"){

         results <- NULL
         for(i in unique(df$period)){
            temp_period <- df[df$period == i,]

            if(overlapping) j_temp <- unique(unlist(strsplit(temp_period$epoch[temp_period$period == i],",")))
            if(!overlapping) j_temp <- unique(temp_period$epoch[temp_period$period == i])
            j_temp <- j_temp[j_temp != "" & !is.na(j_temp)]

            for(j in j_temp){
               if(overlapping) temp_epoch <- temp_period[grep(j,temp_period$epoch),]
               if(!overlapping) temp_epoch <- temp_period[temp_period$period == i & temp_period$epoch == j,]

               temp <- cbind(i,j,
                             length(unique(temp_epoch$block[temp_epoch$period == i])),
                             min(temp_epoch$time),
                             max(temp_epoch$time),
                             mean(temp_epoch$pres),
                             mean(temp_epoch$mcav),
                             round(100-length(unique(temp_epoch$n))/
                                      (freq*(max(temp_epoch$time)-min(temp_epoch$time)))*100, digits=1),
                             as.numeric(mx[mx[,1] == i & mx[,2] == j,3])
               )
               results <- as.data.frame(rbind(results,temp))
            }

         }

      }else{
         stop("'output' must be left blank, 'period' or 'epoch'")
      }
      cat(paste0("Output created (", runtime(time)," s)...\n"))
      results <- as.data.frame(results)
      colnames(results) <- c("period","epoch","blocks","time.min","time.max","pres","vel","missing.perc","cor")
      return(results)
   }


   #RUNNING SCRIPTS ----

   df <- func_validation(df,trigger,del_pres,del_mcav)
   df <- func_trigger(df,trigger)
   df <- func_deleter(df,del_pres,del_mcav)
   df <- func_blocks(df,freq,blocksize)
   df <- func_epochs(df,epochsize,overlapping)
   df <- func_quality(df, freq, blocksize, blockmin, epochsize, epochmin)
   mx <- func_cor(df)
   results <- func_output(df, mx, freq, output, overlapping)

   return(results)
}

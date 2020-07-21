globalVariables(c("block","epoch","n","period","overlapping"))

cm_mx <- function(
   #Dataframes
   df, del_pres = NULL, del_vel = NULL, trigger = NULL,
   #Calculation settings
   blocksize = 3, epochsize = 20, freq = 1000,
   #Data Quality
   blockmin = 0.5, epochmin = 0.50,
   #Overlapping
   overlapping = FALSE,
   #Output
   output = "period"
){

   #Global functions
   z_validation(df, "recording", 3)
   z_validation(del_pres, "pressure deleter", 2)
   z_validation(del_vel, "velocity deleter", 2)
   z_validation(trigger, "trigger", 2)

   colnames(df) <- c("time","val1","val2")
   df$n <- c(1:nrow(df))

   df <- z_trigger(df,trigger)
   df <- z_deleter(df,del_pres)
   df <- z_deleter(df,del_vel)

   df <- z_blocks(df,freq,blocksize,blockmin)

   #BLOCK FUNCTION ALTERNATIVE
   #df_agg <- z_blocks(df, freq, blocksize, blockmin, by_type=c("mean"))

   df <- z_epochs(df,epochsize,epochmin,overlapping)

   df <- z_cor(df)

   #FUNCTIONS ----

   #Output creation
   func_output <- function(df, mx, freq, output, overlapping){
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
      results <- as.data.frame(results)
      colnames(results) <- c("period","epoch","blocks","time.min","time.max","pres","vel","missing.perc","cor")
      return(results)
   }


   #RUNNING SCRIPTS ----

   results <- func_output(df, mx, freq, output, overlapping)

   return(results)
}

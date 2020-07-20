#Remove measurements outside of the trigger periods.
z_trigger <- function(df, trigger){
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
z_deleter <- function(df,del_pres,del_mcav){
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
      cat(paste0("Deleter periods (n=", nrow(del_mcav)+nrow(del_pres),") are removed (", runtime(time)," s)...\n"))
   }else{
      cat(paste0("Deleters are empty, no data is deleted...\n"))
   }

   return(df)
}

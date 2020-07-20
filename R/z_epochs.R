#Define epochs
z_epochs <- function(df, epochsize, overlapping){
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

#Define epochs
z_epochs <- function(df,epochsize,epochmin,overlapping){

   if(!overlapping){
      df$epoch <- ceiling(df$block/epochsize)
      if(!is.null(epochmin)){
         df <- within(df,del <- ave(block,period,epoch, FUN =
                                    function(x) (length(unique(x))<(epochsize*epochmin))*1))
         del <- length(unique(df$epoch[df$del == 1]))
         df <- df[df$del != "1",-c(ncol(df))]
      }
   }else{


      temp <- NULL
      for(i in unique(df$period)){
         temp_df <- as.data.frame(cbind(i,unique(df$block[df$period == i]),NA))
         colnames(temp_df) <- c("period","block","epoch")
         for(j in temp_df$block[temp_df$block %% overlapping == 0]){

            if(length(temp_df$epoch[temp_df$block > j-epochsize & temp_df$block <= j])>epochsize*epochmin){
               temp_df$epoch[temp_df$block > j-epochsize & temp_df$block <= j] <-
                  paste0(temp_df$epoch[temp_df$block > j-epochsize & temp_df$block <= j], ",-", j, "-")
            }
         }
         temp <- rbind(temp,temp_df)
      }
      temp$epoch <- substr(temp$epoch,4,nchar(temp$epoch))
      df <- merge.data.frame(df,temp,by=c("period","block"),all.x=T)
   }

   return(df)
}

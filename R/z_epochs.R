#Define epochs
z_epochs <- function(df_agg,epochsize,epochmin,overlapping){

   if(!overlapping){
      df_agg$epoch <- ceiling(df_agg$block/epochsize)
      if(!is.null(epochmin)){

         df_agg <- within(df_agg,del <- ave(block,period,epoch, FUN =
                                               function(x) (length(unique(x))<(epochsize*epochmin))*1))
         df_agg <- df_agg[df_agg$del != "1",-c(ncol(df_agg))]
      }
   }else{

      temp <- NULL
      for(i in unique(df_agg$period)){
         temp_df <- as.data.frame(cbind(i,unique(df_agg$block[df_agg$period == i]),NA))
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
      df_agg <- merge.data.frame(df_agg,temp,by=c("period","block"),all.x=T)
   }

   return(df_agg)
}

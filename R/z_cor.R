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

#Correlations for every epoch
z_cor <- function(df_agg, cor_by,overlapping){

   df_agg <- df_agg[!is.na(df_agg$epoch),]
   df_cor <- NULL
   for(i in unique(df_agg$period)){
      temp_period <- df_agg[df_agg$period == i,]
      if(overlapping) j_temp <- unique(unlist(strsplit(temp_period$epoch[temp_period$period == i],",")))
      if(!overlapping) j_temp <- unique(temp_period$epoch[temp_period$period == i])
      j_temp <- j_temp[j_temp != ""]

      for(j in j_temp){
         if(overlapping) temp_epoch <- temp_period[grep(j,temp_period$epoch),]
         if(!overlapping) temp_epoch <- temp_period[temp_period$period == i & temp_period$epoch == j,]
         df_cor <- rbind(df_cor,cbind(i,j,cor(temp_epoch[,c(cor_by[1])],temp_epoch[,c(cor_by[2])])))
      }
   }
   df_cor <- as.data.frame(df_cor)
   colnames(df_cor) <- c("period","epoch","cor")
   return(df_cor)
}

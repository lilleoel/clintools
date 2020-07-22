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

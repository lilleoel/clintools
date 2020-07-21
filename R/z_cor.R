#Correlations for every epoch
z_cor <- function(df, col_by){

   #df <- within(df,val1_mean <- ave(val1,period,block, FUN = function(x) mean(x)))
   #df <- within(df,val2_mean <- ave(val2,period,block, FUN = function(x) mean(x)))

   #temp <- within(df, mx <- by(df[df$period == 3,], df$epoch[df$period == 3], FUN=function(x) cor(x$val1_mean[!duplicated(x[,c("period","block")])],x$val2_mean[!duplicated(x[,c("period","block")])])))

   col_by2 <- list(as.list(df[,c(col_by)]))
   temp_block <- aggregate(df[,c("val1","val2")],by=col_by,mean)
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

   return(mx)
}

test[!duplicated(df$block, df$epoch),]

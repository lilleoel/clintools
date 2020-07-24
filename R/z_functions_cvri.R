#Output creation
cm_cvri_output <- function(df, df_agg, by_type, freq, blocksize, output){
   if(output == "period"){

      results <- NULL
      for(i in unique(df_agg$period)){

         temp <- cbind(i,
                       length(unique(df_agg$block[df_agg$period == i])),
                       min(df$time[df$period == i]),
                       max(df$time[df$period == i]),
                       mean(df_agg[df_agg$period == i,c(by_type[1])]),
                       mean(df_agg[df_agg$period == i,c(by_type[2])]),
                       round(100-sum(df_agg$length[df_agg$period == i])/
                                (freq*(max(df$time[df$period == i])-
                                          min(df$time[df$period == i])))*100, digits=1),
                       mean((df_agg[df_agg$period == i,c(by_type[1])]/df_agg[df_agg$period == i,c(by_type[2])]))
         )
         results <- rbind(results,temp)

      }

   }else if(output == "block"){
      results <- NULL
      for(i in c(1:nrow(df_agg))){
         temp <- cbind(df_agg[i,c("period","block")],
                       min(df$time[df$period == df_agg$period[i] & df$block == df_agg$block[i]]),
                       max(df$time[df$period == df_agg$period[i] & df$block == df_agg$block[i]]),
                        df_agg[i,c("val1_max","val1_min")],
                       round(100-df_agg$length[i]/(freq*blocksize)*100, digits=1),
                        (df_agg$val2_mean[i])/df_agg$val2_mean[i])
         results <- rbind(results,temp)
      }

   }else{
      stop("\'output\' must be left blank, 'period' or 'block'")
   }
   results <- as.data.frame(results)
   colnames(results) <- c("period","blocks","time.min","time.max","val1_mean","val2_mean","missing.perc","cvri")
   return(results)
}

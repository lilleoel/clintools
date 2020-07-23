#Output creation
cm_mx_output <- function(df, df_agg, df_cor, freq, output, cor_by, overlapping){
   if(output == "period"){

      results <- NULL
      for(i in unique(df_agg$period)){
         if(overlapping) n_epoch <- unique(unlist(strsplit(df_agg$epoch[df$period == i],",")))
         if(!overlapping) n_epoch <- unique(df_agg$epoch[df_agg$period == i])
         n_epoch <- n_epoch[n_epoch != "" & !is.na(n_epoch)]

         temp <- cbind(i,
                       length(n_epoch),
                       length(unique(df_agg$block[df_agg$period == i])),
                       min(df$time[df$period == i]),
                       max(df$time[df$period == i]),
                       mean(df_agg[df_agg$period == i,c(cor_by[1])]),
                       mean(df_agg[df_agg$period == i,c(cor_by[2])]),
                       round(100-sum(df_agg$length[df_agg$period == i])/
                                (freq*(max(df$time[df$period == i])-
                                          min(df$time[df$period == i])))*100, digits=1),
                       mean(as.numeric(df_cor$cor[df_cor$period == i]))
         )
         results <- rbind(results,temp)

      }

   }else if(output == "epoch"){

      results <- NULL
      for(i in unique(df_agg$period)){

         if(overlapping) j_temp <- unique(unlist(strsplit(df_agg$epoch[df_agg$period == i],",")))
         if(!overlapping) j_temp <- unique(df_agg$epoch[df_agg$period == i])
         j_temp <- j_temp[j_temp != "" & !is.na(j_temp)]

         for(j in j_temp){
            if(overlapping) temp_epoch <- df_agg[grep(j,df_agg$epoch[df_agg$period == i]),]
            if(!overlapping) temp_epoch <- df_agg[df_agg$period == i & df_agg$epoch == j,]

            temp <- cbind(i,j,
                          length(unique(temp_epoch$block[temp_epoch$period == i])),
                          min(df$time[df$period == i & df$block == min(temp_epoch$block)]),
                          max(df$time[df$period == i & df$block == max(temp_epoch$block)]),
                          mean(temp_epoch[[cor_by[1]]]),
                          mean(temp_epoch[[cor_by[2]]]),
                          round(100-sum(temp_epoch$length)/
                              (freq*(max(df$time[df$period == i & df$block == max(temp_epoch$block)])-
                                    min(df$time[df$period == i & df$block == min(temp_epoch$block)])))*
                                 100, digits=1),
                          mean(as.numeric(df_cor$cor[df_cor$period == i & df_cor$epoch == j]))
            )
            results <- as.data.frame(rbind(results,temp))
         }

      }

   }else{
      stop("\'output\' must be left blank, 'period' or 'epoch'")
   }
   results <- as.data.frame(results)
   colnames(results) <- c("period","epoch","blocks","time.min","time.max","val1","val2","missing.perc","cor")
   return(results)
}

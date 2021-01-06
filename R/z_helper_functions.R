# ==== OPTIMIZE ====

   # Functions which optimize the function.
   # This is done by aggregating the dataframe.
   Z.fast <- function(df, freq, fast){
      if(fast >= 10 & freq > fast){
         every <- freq/fast
         na.rm = FALSE
         temp_cn <- colnames(df)
         output <- NULL
         for(i in 1:ncol(df)){
            vec <- df[[i]]
            n <- length(vec)
            x <- .colMeans(vec, every, n %/% every, na.rm)
            r <- n %% every
            if(r){ x <- c(x, mean.default(vec[(n-r+1):n], na.rm=na.rm))}
            output <- cbind(output,x)
         }
         output <- as.data.frame(output)
         colnames(output) <- temp_cn
         return(output)
      }else{
         return(df)
      }
   }

   # This function ensures changing freq if the
   # dataframe is aggregated.
   Z.fast_ftf <- function(freq, fast){
      if(fast >= 10 & freq > fast){
         freq <- fast
      }
      return(freq)
   }

# ==== DATA MANAGEMENT ====

   Z.datamanagement <- function(df, trigger, deleter, freq, blocksize = NULL, rec_col = 3){

      # Validation of dataframes
      Z.validation <- function(df, name, cols){

         if(!is.null(df)){
            if(any(sapply(df,class) != "numeric")){
               stop(paste0("The full \'", name, "\' dataframe must be numeric"))
            }else if(ncol(df) != cols){
               stop(paste0("The \'", name, "\' dataframe must contain ", cols, " columns."))
            }
         }
      }

      #Run the validation
      Z.validation(df, "recording", rec_col)
      Z.validation(deleter, "deleter", 2)
      Z.validation(trigger, "trigger", 2)

      if(rec_col == 2){
         colnames(df) <- c("time","val1")
      }else if(rec_col == 3){
         colnames(df) <- c("time","val1","val2")
      }
      df$n <- c(1:nrow(df))

      #Remove measurements outside of the trigger periods.
      Z.trigger <- function(df, trigger){
         if(!is.null(trigger)) {
            for(i in c(1:nrow(trigger))){
               df$period[df[[1]] >= trigger[i,1] & df[[1]] < trigger[i,2]] <- i
            }
            df <- df[!is.na(df$period),]
         }else{
            df$period <- 1
         }
         return(df)
      }
      df <- Z.trigger(df,trigger)

      #Remove deleted measurements
      Z.deleter <- function(df,del){
         if(!is.null(del)){
            for(i in c(1:nrow(del))){
               df <- df[df[[1]] <= del[i,1] | df[[1]] >= del[i,2],]
            }
         }
         return(df)
      }
      df <- Z.deleter(df,deleter)

      #Define blocks
      Z.blocks <- function(df, freq, blocksize){
         if(!is.null(blocksize)){
            df <- within(df,block <- ave(n,period,FUN = function(x)
               ceiling((x-min(x)+1)/(blocksize*freq))))
         }
         return(df)
      }
      df <- Z.blocks(df,freq,blocksize)

      return(df)
   }

   #Aggregate dataframe
   Z.aggregate <- function(df,freq,blocksize,blockmin,by_type,n_vars){

      #Create df_block
      df_block <- aggregate(df[,1],by=list(df$block,df$period),length)
      colnames(df_block) <- c("block","period","length")
      for(i in by_type){
         temp_df_block <- by(df$val1, list(df$block,df$period), eval(parse(text = i)), na.rm=T)
         temp_df_block <- array(temp_df_block, dim(temp_df_block), dimnames(temp_df_block))
         temp_df_block <- as.data.frame(cbind(rownames(temp_df_block),temp_df_block))
         colnames(temp_df_block)[1] <- "block"
         insertion_df <- NULL
         for(j in c(1:(ncol(temp_df_block)-1))){
            temp <- cbind(temp_df_block$block, j, temp_df_block[,c(j+1)])
            insertion_df <- rbind(insertion_df,temp)
         }
         colnames(insertion_df) <- c("block","period",paste0("val1_",i))
         df_block <- merge(df_block, insertion_df, by=c("block","period"), all.x=T)
         df_block[,paste0("val1_",i)] <- as.numeric(df_block[,paste0("val1_",i)])

        if(n_vars > 1){
           temp_df_block <- by(df$val2, list(df$block,df$period), eval(parse(text = i)), na.rm=T)
           temp_df_block <- array(temp_df_block, dim(temp_df_block), dimnames(temp_df_block))
           temp_df_block <- as.data.frame(cbind(rownames(temp_df_block),temp_df_block))
           colnames(temp_df_block)[1] <- "block"
           insertion_df <- NULL
           for(j in c(1:(ncol(temp_df_block)-1))){
              temp <- cbind(temp_df_block$block, j, temp_df_block[,c(j+1)])
              insertion_df <- rbind(insertion_df,temp)
           }
           colnames(insertion_df) <- c("block","period",paste0("val2_",i))
           df_block <- merge(df_block, insertion_df, by=c("block","period"), all.x=T)
           df_block[,paste0("val2_",i)] <- as.numeric(df_block[,paste0("val2_",i)])
        }
      }

      #Remove blocks which does not fullfill quality control
      if(!is.null(blockmin)){

         df_block <- df_block[df_block$length > freq*blocksize*blockmin,]
      }

      return(df_block)
   }

# ==== Mx | Dx | Sx | PRx ====

   #Define epochs
   Z.epochs <- function(df_agg,epochsize,epochmin,overlapping){

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
   Z.cor <- function(df_agg, cor_by,overlapping){

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

   #Output creation
   Z.cor_output <- function(df, df_agg, df_cor, freq, output, cor_by, overlapping){
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
                          mean(df_agg[df_agg$period == i,c(cor_by[1])], na.rm=T),
                          mean(df_agg[df_agg$period == i,c(cor_by[2])], na.rm=T),
                          round(100-sum(df_agg$length[df_agg$period == i])/
                                   (freq*(max(df$time[df$period == i])-
                                             min(df$time[df$period == i])))*100, digits=1),
                          mean(as.numeric(df_cor$cor[df_cor$period == i]), na.rm=T)
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

# ==== CO ====

   #Output creation
   Z.co_output <- function(df, df_agg, by_type, freq, blocksize, output){
      if(output == "period"){

         results <- NULL
         for(i in unique(df_agg$period)){

            temp <- cbind(i,
                          length(unique(df_agg$block[df_agg$period == i])),
                          min(df$time[df$period == i]),
                          max(df$time[df$period == i]),
                          mean(df_agg[df_agg$period == i,c(by_type[1])]),
                          mean(df_agg[df_agg$period == i,c(by_type[2])]),
                          mean(df_agg[df_agg$period == i,c(by_type[3])]),
                          mean(df_agg[df_agg$period == i,c(by_type[4])]),
                          round(100-sum(df_agg$length[df_agg$period == i])/
                                   (freq*(max(df$time[df$period == i])-
                                             min(df$time[df$period == i])))*100, digits=1),
                          mean((df_agg[df_agg$period == i,c(by_type[1])]-df_agg[df_agg$period == i,c(by_type[2])])/
                                  (df_agg[df_agg$period == i,c(by_type[1])]+df_agg[df_agg$period == i,c(by_type[2])])*
                                  df_agg[df_agg$period == i,c(by_type[3])])
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
                          (df_agg$val1_max[i]-df_agg$val1_min[i])/(df_agg$val1_max[i]+df_agg$val1_min[i])*
                             df_agg$val2_mean[i])
            results <- rbind(results,temp)
         }

      }else{
         stop("\'output\' must be left blank, 'period' or 'block'")
      }
      results <- as.data.frame(results)
      colnames(results) <- c("period","blocks","time.min","time.max","val1_max","val1_min","val2_mean","missing.perc","cvr")
      return(results)
   }

# ==== CVRi ====

   #Output creation
   Z.cvri_output <- function(df, df_agg, by_type, freq, blocksize, output){
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

# ==== PI ====

   #Output creation
   Z.pi_output <- function(df, df_agg, by_type, freq, blocksize, output){
      if(output == "period"){

         results <- NULL
         for(i in unique(df_agg$period)){

            temp <- cbind(i,
                          length(unique(df_agg$block[df_agg$period == i])),
                          min(df$time[df$period == i]),
                          max(df$time[df$period == i]),
                          mean(df_agg[df_agg$period == i,c(by_type[1])]),
                          mean(df_agg[df_agg$period == i,c(by_type[2])]),
                          mean(df_agg[df_agg$period == i,c(by_type[3])]),
                          round(100-sum(df_agg$length[df_agg$period == i])/
                                   (freq*(max(df$time[df$period == i])-
                                             min(df$time[df$period == i])))*100, digits=1),
                          mean((df_agg[df_agg$period == i,c(by_type[1])]-df_agg[df_agg$period == i,c(by_type[2])])/
                                  df_agg[df_agg$period == i,c(by_type[3])])
            )
            results <- rbind(results,temp)

         }

      }else if(output == "block"){
         results <- NULL
         for(i in c(1:nrow(df_agg))){
            temp <- cbind(df_agg[i,c("period","block")],
                          min(df$time[df$period == df_agg$period[i] & df$block == df_agg$block[i]]),
                          max(df$time[df$period == df_agg$period[i] & df$block == df_agg$block[i]]),
                          df_agg[i,c("val1_max","val1_min","val1_mean")],
                          round(100-df_agg$length[i]/(freq*blocksize)*100, digits=1),
                          (df_agg$val1_max[i]-df_agg$val1_min[i])/df_agg$val1_mean[i])
            results <- rbind(results,temp)
         }

      }else{
         stop("\'output\' must be left blank, 'period' or 'block'")
      }
      results <- as.data.frame(results)
      colnames(results) <- c("period","blocks","time.min","time.max","val1_max","val1_min","val1_mean","missing.perc","pi")
      return(results)
   }

# ==== PWA ====

   #Output creation
   Z.pwa_output <- function(df, df_agg, by_type, freq, blocksize, output){
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
                          mean((df_agg[df_agg$period == i,c(by_type[1])]-df_agg[df_agg$period == i,c(by_type[2])]))
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
                          (df_agg$val1_max[i]-df_agg$val1_min[i]))
            results <- rbind(results,temp)
         }

      }else{
         stop("\'output\' must be left blank, 'period' or 'block'")
      }
      results <- as.data.frame(results)
      colnames(results) <- c("period","blocks","time.min","time.max","val1_max","val1_min","missing.perc","pwa")
      return(results)
   }

# ==== RI ====

   #Output creation
   Z.ri_output <- function(df, df_agg, by_type, freq, blocksize, output){
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
                          mean((df_agg[df_agg$period == i,c(by_type[1])]-df_agg[df_agg$period == i,c(by_type[2])])/
                                  df_agg[df_agg$period == i,c(by_type[1])])
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
                          (df_agg$val1_max[i]-df_agg$val1_min[i])/df_agg$val1_max[i])
            results <- rbind(results,temp)
         }

      }else{
         stop("\'output\' must be left blank, 'period' or 'block'")
      }
      results <- as.data.frame(results)
      colnames(results) <- c("period","blocks","time.min","time.max","val1_max","val1_min","missing.perc","ri")
      return(results)
   }



# ==== TFA ====


   #calculate cyclic mean from

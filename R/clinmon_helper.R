# ==== OPTIMIZE ====

   # Functions which optimize the function.
   # This is done by aggregating the dataframe.
   Z.fast <- function(df, freq, fast){
      if(fast >= 5 & freq > fast){
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
      if(fast >= 5 & freq > fast){
         freq <- fast
      }
      return(freq)
   }

# ==== DATA MANAGEMENT ====

   Z.datamanagement <- function(df, variables, trigger, deleter, blocksize, freq){

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
      Z.validation(df, "recording", length(variables)+1)
      Z.validation(deleter, "deleter", 2)
      Z.validation(trigger, "trigger", 2)

      colnames(df) <- c("t",variables)
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
   Z.aggregate <- function(df, variables, blocksize, epochsize, overlapping, freq, blockmin, epochmin){

      #Create df.agg
      df.agg <- aggregate(df[,1],by=list(df$block,df$period),length)
      colnames(df.agg) <- c("block","period","length")

      #time min vs. max and missing data
      for(i in c(1:nrow(df.agg))){
         df.agg$time_min[i] <- min(df$t[df.agg$period[i] == df$period & df.agg$block[i] == df$block])
         df.agg$time_max[i] <- max(df$t[df.agg$period[i] == df$period & df.agg$block[i] == df$block])
      }
      df.agg$missing_percent <- abs((1-df.agg$length/(freq*blocksize))*100)

      #Calculate mean, min and max for every variable
      Z.compress <- function(df,variables){
         compressed_df <- NULL
         for(i in variables){
            for(j in c("mean","min","max")){
               temp_df.agg <- by(df[,i], list(df$block,df$period), eval(parse(text = j)), na.rm=T)
               temp_df.agg <- array(temp_df.agg, dim(temp_df.agg), dimnames(temp_df.agg))
               temp_df.agg <- as.data.frame(cbind(rownames(temp_df.agg),temp_df.agg))
               insertion_df <- NULL
               for(k in c(1:(ncol(temp_df.agg)-1))){
                  temp <- cbind(temp_df.agg[1], k, temp_df.agg[,c(k+1)])
                  insertion_df <- rbind(insertion_df,temp)
               }
               colnames(insertion_df) <- c("block","period",paste0(i,"_",j))
               if(is.null(compressed_df)){
                  compressed_df <- insertion_df
               }else{
                  compressed_df <- merge(compressed_df, insertion_df, by=c("block","period"))
               }
            }
         }
         return(compressed_df)
      }

      df.agg <- merge(df.agg, Z.compress(df,variables), by=c("block","period"), all.x=T)

      #Remove blocks which does not fullfill quality control
      if(!is.null(blockmin)){
         df.agg <- df.agg[df.agg$length > freq*blocksize*blockmin,]
      }

      #Create EPOCHS
      if(!overlapping){
         df.agg$epoch <- ceiling(df.agg$block/epochsize)
         if(!is.null(epochmin)){
            df.agg <- within(df.agg,
                             del <- ave(block,period,epoch, FUN =
                                       function(x) (length(unique(x))<(epochsize*epochmin))*1))
            df.agg <- df.agg[df.agg$del != "1",-c(ncol(df.agg))]
         }
      }else{
         temp <- NULL
         for(i in unique(df.agg$period)){
            temp_df <- as.data.frame(cbind(i,unique(df.agg$block[df.agg$period == i]),NA))
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
         df.agg <- merge.data.frame(df.agg,temp,by=c("period","block"),all.x=T)
      }

      #Time min and time max

      #Missing data
      #NEEEEEEDDDEEEED
      #TIME MIN AND TIME MAX

      #Sort df
      df.agg[,!colnames(df.agg) == "epoch"] <- sapply(df.agg[,!colnames(df.agg) == "epoch"], as.numeric)
      df.agg <- df.agg[order(df.agg$period,df.agg$block),]
      df.agg <- df.agg[,c("period","epoch","block",colnames(df.agg)[!colnames(df.agg) %in% c("period","epoch","block")])]
      df.agg$length <- NULL
      return(df.agg)
   }

# ==== BLOCK ANALYSES ====

   #All analyses depending on blocks
   Z.block_analyses <- function(df.agg,variables){
      #  COest
      # [COest = PP / (SBP+DBP) * HR]
      if(any(variables == "abp") & any(variables == "hr")){
         df.agg$COest <- (df.agg$abp_max-df.agg$abp_min)/
            (df.agg$abp_max+df.agg$abp_min)*df.agg$hr_mean
      }

      #  CVRi
      # [CVRi = mean ABP / mean MCAv]
      if(any(variables == "abp") & any(variables == "mcav")){
         df.agg$CVRi <- df.agg$abp_mean/df.agg$mcav_mean
      }

      #  PI
      # [PI = (systolic MCAv - diastolic MCAv) / mean MCAv]
      if(any(variables == "mcav")){
         df.agg$PI <- (df.agg$mcav_max-df.agg$mcav_min)/df.agg$mcav_mean
      }

      #  PWA
      # [PWA = systolic - diastolic]
      if(any(variables == "abp")){
         df.agg$PWA_abp <- df.agg$abp_max-df.agg$abp_min
      }
      if(any(variables == "icp")){
         df.agg$PWA_icp <- df.agg$icp_max-df.agg$icp_min
      }
      if(any(variables == "cpp")){
         df.agg$PWA_cpp <- df.agg$cpp_max-df.agg$cpp_min
      }
      if(any(variables == "mcav")){
         df.agg$PWA_mcav <- df.agg$mcav_max-df.agg$mcav_min
      }

      #  RI
      # [RI = (systolic MCAv - diastolic MCAv) / systolic MCAv]
      if(any(variables == "mcav")){
         df.agg$RI <- (df.agg$mcav_max-df.agg$mcav_min)/df.agg$mcav_max
      }

      #Output
      return(df.agg)
   }

# ==== EPOCH ANALYSES ====

   Z.epoch_analyses <- function(df.block,variables,overlapping){

      #Create df.cor
      Z.create_df.cor <- function(df.block,overlapping){
         df.block <- df.block[!is.na(df.block$epoch),]
         df.cor <- NULL
         for(i in unique(df.block$period)){
            temp_period <- df.block[df.block$period == i,]
            if(overlapping) {
               j_temp <- unique(unlist(strsplit(temp_period$epoch[temp_period$period == i],",")))
            }else if(!overlapping) {
               j_temp <- unique(temp_period$epoch[temp_period$period == i])
            }
            j_temp <- j_temp[j_temp != ""]

            for(j in j_temp){
               df.cor <- rbind(df.cor,cbind(i,j))
            }
         }

         df.cor <- as.data.frame(df.cor)
         colnames(df.cor) <- c("period","epoch")
         return(df.cor)
      }
      df.cor <- Z.create_df.cor(df.block,overlapping)


      #Correlation analysis, depending on epochs
      Z.correlation_analyses <- function(df.block, cor_by, name){
         df.block <- df.block[!is.na(df.block$epoch),]
         df.cor <- NULL
         for(i in unique(df.block$period)){
            temp_period <- df.block[df.block$period == i,]
            if(overlapping) {
               j_temp <- unique(unlist(strsplit(temp_period$epoch[temp_period$period == i],",")))
            }else if(!overlapping) {
               j_temp <- unique(temp_period$epoch[temp_period$period == i])
            }
            j_temp <- j_temp[j_temp != ""]

            for(j in j_temp){
               if(overlapping){
                  temp_epoch <- temp_period[grep(j,temp_period$epoch),]
               }else if(!overlapping){
                  temp_epoch <- temp_period[temp_period$period == i & temp_period$epoch == j,]
               }
               df.cor <- rbind(df.cor,cbind(i,j,cor(temp_epoch[,c(cor_by[1])],temp_epoch[,c(cor_by[2])])))
            }
         }

         df.cor <- as.data.frame(df.cor)
         colnames(df.cor) <- c("period","epoch",name)
         return(df.cor)
      }

      if(any(variables == "abp") & any(variables == "mcav")){
         df.cor <- merge(df.cor,Z.correlation_analyses(df.block,cor_by=c("abp_mean","mcav_mean"),"Mxa"),
                         by=c("period","epoch"))
         df.cor <- merge(df.cor,Z.correlation_analyses(df.block,cor_by=c("abp_mean","mcav_max"),"Sxa"),
                         by=c("period","epoch"))
         df.cor <- merge(df.cor,Z.correlation_analyses(df.block,cor_by=c("abp_mean","mcav_min"),"Dxa"),
                         by=c("period","epoch"))
      }
      if(any(variables == "cpp") & any(variables == "mcav")){
         df.cor <- merge(df.cor,Z.correlation_analyses(df.block,cor_by=c("cpp_mean","mcav_mean"),"Mx"),
                         by=c("period","epoch"))
         df.cor <- merge(df.cor,Z.correlation_analyses(df.block,cor_by=c("cpp_mean","mcav_max"),"Sx"),
                         by=c("period","epoch"))
         df.cor <- merge(df.cor,Z.correlation_analyses(df.block,cor_by=c("cpp_mean","mcav_min"),"Dx"),
                         by=c("period","epoch"))
      }
      if(any(variables == "abp") & any(variables == "icp")){
         df.cor <- merge(df.cor,Z.correlation_analyses(df.block,cor_by=c("abp_mean","icp_mean"),"PRx"),
                         by=c("period","epoch"))
      }

      #Sort df
      df.cor$epoch <- gsub("-","",df.cor$epoch)
      df.cor[] <- sapply(df.cor, as.numeric)
      df.cor <- df.cor[order(df.cor$period,df.cor$epoch),]

      return(df.cor)

   }

# ==== OUTPUT GENERATION ====

      #Output creation
      Z.output <- function(df.block,df.epoch,output,overlapping){

         #CPPopt helper
         Z.cppopt <- function(df.block,df.epoch,output){

            df.cppopt <- df.epoch[,c("period","epoch","PRx")]
            temp2 <- NULL
            for(i in 1:nrow(df.cppopt)){
               if(overlapping){
                  epoch_no <- paste0("-",df.cppopt$epoch[i],"-")
               }else{
                  epoch_no <- df.cppopt$epoch[i]
               }
               temp <- df.block[df.block$period == df.cppopt$period[i] & grepl(epoch_no,df.block$epoch),c("period","epoch","abp_mean","icp_mean","cpp_mean")]
               temp <- suppressWarnings(aggregate(temp,by=list(temp$period),mean)[,-c(1)])
               temp$epoch <- df.cppopt$epoch[i]
               temp2 <- rbind(temp2,temp)
            }
            df.cppopt <- merge(df.cppopt,temp2,by=c("period","epoch"))
            df.cppopt$CPPopt <- paste0(floor(df.cppopt$cpp_mean/5)*5,"-",floor(df.cppopt$cpp_mean/5)*5+5)
            df.cppopt <- df.cppopt[order(df.cppopt$period,df.cppopt$epoch),]

            if(output == "period"){
               df.cppopt <- aggregate(df.cppopt$PRx,by=list(df.cppopt$period,df.cppopt$CPPopt),mean)

               results <- NA
               for(i in unique(df.epoch$period)){

                  cppopt <- df.cppopt$Group.2[df.cppopt$x == min(df.cppopt$x)]
                  if(cppopt == max(df.cppopt$Group.2[df.cppopt$Group.1 == i]) |
                     cppopt == min(df.cppopt$Group.2[df.cppopt$Group.1 == i]) ){
                     cppopt <- NA
                  }
                  if(i == 1){ results <- cppopt
                  }else{ results <- c(results,cppopt) }
               }
               df.cppopt <- results
            }
            return(df.cppopt)
         }

         if(output == "block"){
            df.temp <- df.block
         }else if(output == "epoch"){
            if(!overlapping){
               df.temp <- aggregate(df.block[,c("period","epoch","block")],
                                    by=list(df.block$period,df.block$epoch),
                                    FUN=function(x) length(unique(x)))[,-c(3:4)]
               colnames(df.temp)[c(1:2)] <- c("period","epoch")
               df.temp <- merge(df.temp,aggregate(df.block[,c("period","epoch","time_min")],
                                                  by=list(df.block$period,df.block$epoch),min)[,-c(1:2)],
                                by=c("period","epoch"))
               df.temp <- merge(df.temp,aggregate(df.block[,c("period","epoch","time_max")],
                                    by=list(df.block$period,df.block$epoch),max)[,-c(1:2)],
                                by=c("period","epoch"))
               df.temp.2 <- aggregate(df.block,by=list(df.block$period,df.block$epoch),mean)[,-c(1:2)]
               df.temp.2[,c("block","time_min","time_max")] <- NULL
               df.temp <- merge(df.temp,df.temp.2,by=c("period","epoch"))
               df.temp <- merge(df.temp,df.epoch,by=c("period","epoch"))
            }else if(overlapping){
               df.temp <- df.epoch[,c("period","epoch")]
               for(i in c(1:nrow(df.epoch))){
                  df.temp.block <- df.block[df.epoch$period[i] == df.block$period & grepl(paste0("-",df.epoch$epoch[i],"-"),df.block$epoch),]

                  df.temp$blocks[i] <- length(unique(df.temp.block$block))
                  df.temp$time_min[i] <- min(df.temp.block$time_min)
                  df.temp$time_max[i] <- max(df.temp.block$time_max)

                  insertion_list <- colMeans(df.temp.block[,!(colnames(df.temp.block) %in% c("period","epoch","block","time_min","time_max"))])
                  df.temp[i,names(insertion_list)] <- c(insertion_list)
               }
               df.temp <- merge(df.temp,df.epoch,by=c("period","epoch"))
            }
            df.temp[] <- sapply(df.temp, as.numeric)
            df.temp <- df.temp[order(df.temp$period,df.temp$epoch),]
         }else if(output == "period"){
            df.temp <- aggregate(df.epoch[,c("period","epoch")],
                                 by=list(df.epoch$period),
                                 FUN=function(x) length(unique(x)))[,-c(2)]
            df.temp.2 <- aggregate(df.block[,c("period","block")],
                                 by=list(df.block$period),
                                 FUN=function(x) length(unique(x)))[,-c(2)]
            df.temp <- merge(df.temp,df.temp.2,by="Group.1")
            colnames(df.temp)[1] <- c("period")

            df.temp <- merge(df.temp,aggregate(df.block[,c("period","time_min")],
                                               by=list(df.block$period),min)[,-c(1)],
                             by=c("period"))
            df.temp <- merge(df.temp,aggregate(df.block[,c("period","time_max")],
                                               by=list(df.block$period),max)[,-c(1)],
                             by=c("period"))
            df.temp.2 <- aggregate(df.block[,colnames(df.block) != "epoch"],by=list(df.block$period),mean)[,-c(1)]
            df.temp.2[,c("epoch","block","time_min","time_max")] <- NULL
            df.temp <- merge(df.temp,df.temp.2,by=c("period"))
            df.temp.2 <- aggregate(df.epoch,by=list(df.epoch$period),mean)[,-c(1)]
            df.temp.2[,c("epoch")] <- NULL
            df.temp <- merge(df.temp,df.temp.2,by=c("period"))

            if(any(colnames(df.temp) == "PRx")){
               df.temp$CPPopt <- Z.cppopt(df.block,df.epoch,output)
            }
         }else if(output == "cppopt"){
            if(is.null(df.epoch$PRx)){
               stop("For CPPopt-output the recording needs 'icp' and 'cpp'")
            }
            df.temp <- Z.cppopt(df.block,df.epoch,output)
         }

         return(df.temp)
      }







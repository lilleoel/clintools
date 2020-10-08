#Validation of dataframes
z_validation <- function(df, name, cols){

   if(!is.null(df)){
      if(any(sapply(df,class) != "numeric")){
         stop(paste0("The full \'", name, "\' dataframe must be numeric"))
      }else if(ncol(df) != cols){
         stop(paste0("The \'", name, "\' dataframe must contain ", cols, " columns."))
      }
   }
}

#Remove measurements outside of the trigger periods.
z_trigger <- function(df, trigger){
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

#Remove deleted measurements
z_deleter <- function(df,del){
   if(!is.null(del)){
      for(i in c(1:nrow(del))){
         df <- df[df[[1]] <= del[i,1] | df[[1]] >= del[i,2],]
      }
   }
   return(df)
}

#Define blocks
z_blocks <- function(df, freq, blocksize){
   df <- within(df,block <- ave(n,period,FUN = function(x) ceiling((x-min(x)+1)/(blocksize*freq))))
   return(df)
}

#Aggregate dataframe
z_agg <- function(df,freq,blocksize,blockmin,by_type,n_vars){

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

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
z_blocks <- function(df,freq,blocksize,blockmin,by_type){

   df <- within(df,block <- ave(n,period,FUN = function(x) ceiling((x-min(x)+1)/(blocksize*freq))))

   if(!is.null(blockmin)){
      df <- within(df,del <- ave(n,period,block, FUN = function(x)
         (length(x)<(freq*blocksize*blockmin))*1))
      del <- length(unique(df$block[df$del == 1]))
      df <- df[df$del != "1",-c(ncol(df))]
   }

   df_block <- NULL
   for(i in by_type){
      temp <- aggregate(df[,c("val1","val2")],by=list(df$period,df$block),i)
      colnames(temp) <- c("period","block",paste0("val1_",i),paste0("val2_",i))
      if(!is.null(df_block)) df_block <- merge(df_block,temp,by=c("period","block"))
      if(is.null(df_block)) df_block <- temp
   }

   #this should be faster.
   #missing <- creation of dataframe with unique blocks, i think it exists in the cor function
   temp <- by(df$val1, list(df$block,df$period), mean,simplify=TRUE)



   return(df_block)
}


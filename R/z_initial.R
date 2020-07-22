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
#Change function to act for every variable, subsequent merging - NEXT ACTION
z_blocks <- function(df,freq,blocksize,blockmin,by_type){

   #Add blocks
   df <- within(df,block <- ave(n,period,FUN = function(x) ceiling((x-min(x)+1)/(blocksize*freq))))

   #Create df_block
   df_block <- aggregate(df[,1],by=list(df$block,df$period),length)
   colnames(df_block) <- c("block","period","length")
   for(i in by_type){
      df_block <- cbind(df_block,na.omit(c(by(df$val1, list(df$block,df$period), eval(parse(text = i))))))
      df_block <- cbind(df_block,na.omit(c(by(df$val2, list(df$block,df$period), eval(parse(text = i))))))
      colnames(df_block)[c((ncol(df_block)-1):(ncol(df_block)))] <- c(paste0("val1_",i), paste0("val2_",i))
   }


   #Remove blocks which does not fullfill quality control
   if(!is.null(blockmin)){
      df <- within(df,del <- ave(n,period,block, FUN = function(x)
         (length(x)<(freq*blocksize*blockmin))*1))
      del <- length(unique(df$block[df$del == 1]))
      df <- df[df$del != "1",-c(ncol(df))]
   }



   return(df_block)
}


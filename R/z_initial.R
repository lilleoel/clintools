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
z_blocks <- function(df,freq,blocksize,blockmin){
   df <- within(df, block <- ave(n,period,FUN = function(x) x-min(x)+1))
   df <- within(df,block <- ave(block,period,FUN = function(x) ceiling(x/(blocksize*freq))))

   if(!is.null(blockmin)){

      df <- within(df,del <- ave(n,period,block, FUN = function(x) (length(x)<(freq*blocksize*blockmin))*1))
      del <- length(unique(df$block[df$del == 1]))
      df <- df[df$del != "1",-c(ncol(df))]
   }
   return(df)
}


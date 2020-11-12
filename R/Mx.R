globalVariables(c("block","epoch","n","period","overlapping"))

Mx <- function(
   #Dataframes
   df, del_1 = NULL, del_2 = NULL, trigger = NULL,
   #Calculation settings
   blocksize = 3, epochsize = 20, freq = 1000,
   #Data Quality
   blockmin = 0.5, epochmin = 0.5,
   #Overlapping
   overlapping = FALSE,
   #Output
   output = "period", fast = FALSE
){
   df_cols <- colnames(df)

#OPTIMISE
   BinMean <- function(df, every, na.rm = FALSE){
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
   }

   if(fast)if(fast > 10 & freq > fast) { df <- BinMean(data,freq/fast); freq <- fast }

   #Global functions
   z_validation(df, "recording", 3)
   z_validation(del_1, "first deleter", 2)
   z_validation(del_2, "second deleter", 2)
   z_validation(trigger, "trigger", 2)

   colnames(df) <- c("time","val1","val2")
   df$n <- c(1:nrow(df))

   df <- z_trigger(df,trigger)
   df <- z_deleter(df,del_1)
   df <- z_deleter(df,del_2)
   df <- z_blocks(df,freq,blocksize)

   df_agg <- z_agg(df,freq,blocksize,blockmin,by_type=c("mean"),n_vars=2)
   df_agg <- z_epochs(df_agg,epochsize,epochmin,overlapping)
   df_cor <- z_cor(df_agg, cor_by = c("val1_mean","val2_mean"),overlapping)

   results <- cor_output(df, df_agg, df_cor, freq, output, cor_by = c("val1_mean","val2_mean"), overlapping)

   colnames(results)[colnames(results) == "val1"] <- paste0(df_cols[2],"_mean")
   colnames(results)[colnames(results) == "val2"] <- paste0(df_cols[3],"_mean")

   #FUNCTIONS ----
   return(results)
}

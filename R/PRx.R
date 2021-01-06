globalVariables(c("block","epoch","n","period","overlapping"))

PRx <- function(
   #Dataframes
   df, trigger = NULL, deleter = NULL,
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

   #OPTIMIZE
   df <- Z.fast(df,freq,fast)
   freq <- Z.fast_ftf(freq,fast)

   #DATA MANAGEMENT
   df <- Z.datamanagement(df, trigger, deleter, freq, blocksize, rec_col = 3)
   df.agg <- Z.aggregate(df,freq,blocksize,blockmin,by_type=c("mean"),n_vars=2)

   #CORRELATION ANALYSES
   df.agg <- Z.epochs(df.agg,epochsize,epochmin,overlapping)
   df.cor <- Z.cor(df.agg, cor_by = c("val1_mean","val2_mean"),overlapping)
   results <- Z.cor_output(df, df.agg, df.cor, freq, output, cor_by = c("val1_mean","val2_mean"), overlapping)

   colnames(results)[colnames(results) == "val1"] <- paste0(df_cols[2],"_mean")
   colnames(results)[colnames(results) == "val2"] <- paste0(df_cols[3],"_mean")

   return(results)
}

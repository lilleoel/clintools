globalVariables(c("block","epoch","n","period","overlapping"))

cm_co <- function(
   #Dataframes
   df, del_1 = NULL, del_2 = NULL, trigger = NULL,
   #Calculation settings
   blocksize = 3, freq = 1000,
   #Data Quality
   blockmin = 0.5,
   #Output
   output = "period"
){
   df_cols <- colnames(df)

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

   df_agg <- z_agg(df,freq,blocksize,blockmin,by_type=c("max","min","mean"),n_vars=2)

   results <- cm_co_output(df, df_agg, by_type=c("val1_max","val1_min","val2_mean"),freq, blocksize, output)

   #FUNCTIONS ----
   return(results)
}


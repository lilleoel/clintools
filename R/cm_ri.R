#Resistance index (RI) (End diastolic velocity – peak systolic velocity)/ peak systolic velocity
globalVariables(c("block","n","period"))

cm_ri <- function(
   #Dataframes
   df, del = NULL, trigger = NULL,
   #Calculation settings
   blocksize = 3, freq,
   #Data Quality
   blockmin = 0.5,
   #Output
   output = "period"
){
   df_cols <- colnames(df)

   #Global functions
   z_validation(df, "recording", 2)
   z_validation(del, "deleter", 2)
   z_validation(trigger, "trigger", 2)

   colnames(df) <- c("time","val1")
   df$n <- c(1:nrow(df))

   df <- z_trigger(df,trigger)
   df <- z_deleter(df,del)
   df <- z_blocks(df,freq,blocksize)

   df_agg <- z_agg(df,freq,blocksize,blockmin,by_type=c("max","min"),n_vars=1)

   results <- cm_ri_output(df, df_agg, by_type=c("val1_max","val1_min"),freq, blocksize, output)

   colnames(results)[colnames(results) == "val1_max"] <- paste0(df_cols[2],"_max")
   colnames(results)[colnames(results) == "val1_min"] <- paste0(df_cols[2],"_min")

   #FUNCTIONS ----
   return(results)
}

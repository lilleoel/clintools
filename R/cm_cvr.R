#Cvr formel - https://europepmc.org/article/pmc/pmc6054990
#CVR = mean BP/mean MCAv

globalVariables(c("block","epoch","n","period","overlapping"))

cm_cvr <- function(
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

   df_agg <- z_agg(df,freq,blocksize,blockmin,by_type=c("mean"),n_vars=2)

   results <- cm_cvr_output(df, df_agg, by_type=c("val1_mean","val2_mean"),freq, blocksize, output)

   colnames(results)[colnames(results) == "val1"] <- paste0(df_cols[2],"_mean")
   colnames(results)[colnames(results) == "val2"] <- paste0(df_cols[3],"_mean")

   #FUNCTIONS ----
   return(results)
}

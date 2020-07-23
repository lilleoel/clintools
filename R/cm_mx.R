globalVariables(c("block","epoch","n","period","overlapping"))

cm_mx <- function(
   #Dataframes
   df, del_pres = NULL, del_vel = NULL, trigger = NULL,
   #Calculation settings
   blocksize = 3, epochsize = 20, freq = 1000,
   #Data Quality
   blockmin = 0.5, epochmin = 0.50,
   #Overlapping
   overlapping = FALSE,
   #Output
   output = "period"
){
   df_cols <- colnames(df)

   #Global functions
   z_validation(df, "recording", 3)
   z_validation(del_pres, "pressure deleter", 2)
   z_validation(del_vel, "velocity deleter", 2)
   z_validation(trigger, "trigger", 2)

   colnames(df) <- c("time","val1","val2")
   df$n <- c(1:nrow(df))

   df <- z_trigger(df,trigger)
   df <- z_deleter(df,del_pres)
   df <- z_deleter(df,del_vel)
   df <- z_blocks(df,freq,blocksize)

   df_agg <- z_agg(df,freq,blocksize,blockmin,by_type=c("mean"),n_vars=2)
   df_agg <- z_epochs(df_agg,epochsize,epochmin,overlapping)
   df_cor <- z_cor(df_agg, cor_by = c("val1_mean","val2_mean"),overlapping)

   results <- cm_mx_output(df, df_agg, df_cor, freq, output, cor_by = c("val1_mean","val2_mean"), overlapping)

   colnames(results)[colnames(results) == "val1"] <- paste0(df_cols[2],"_mean")
   colnames(results)[colnames(results) == "val2"] <- paste0(df_cols[3],"_mean")

   #FUNCTIONS ----
   return(results)
}

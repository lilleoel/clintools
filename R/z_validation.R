
#Validation of dataframes

z_validation <- function(df, name, cols){
  #cols eg. c("start" , "end") 
  #ncol = length(cols)
  #check all numeric, check number of columns
  #remove colnames adding + and n-creation <- insert into code
  #
}

z_validation <- function(df, trigger, del_pres, del_vel){
   cat("..."); time <- Sys.time()

   #Validation of number of columns in 'df'
   if(ncol(df) != 3){ stop("Recording does not have the required columns, data must be 3 numeric columns with 'time', 'BP', and 'MCAv'") }
   if(sum((sapply(df,class) != "numeric")*1) > 0){ stop("At least one of the columns in the recording dataframe is not numeric") }
   #Validation of number of columns in 'trigger'
   if(!is.null(trigger)) if(ncol(trigger) != 2){ stop("Trigger does not have the required columns, data must be two columns with start and end time for periodes which will be analysed") }
   if(!is.null(trigger)) if(sum((sapply(trigger,class) != "numeric")*1) > 0){ stop("At least one of the columns in the trigger dataframe is not numeric") }
   #Validation of number of columns in 'deleter_p'
   if(!is.null(del_pres)) if(ncol(del_pres) != 2){ stop("Pressure deleter does not have the required columns, data must be two columns with start and end time for periodes which should be deleted") }
   if(!is.null(trigger)) if(sum((sapply(trigger,class) != "numeric")*1) > 0){ stop("At least one of the columns in the pressure deleter dataframe is not numeric") }
   #Validation of number of columns in 'deleter_p'
   if(!is.null(del_vel)) if(ncol(del_vel) != 2){ stop("Velocity deleter does not have the required columns, data must be two columns with start and end time for periodes which should be deleted") }
   if(!is.null(trigger)) if(sum((sapply(trigger,class) != "numeric")*1) > 0){ stop(" At least one of the columns in the velocity deleter dataframe is not numeric") }

   #Define data
   colnames(df) <- c("time","pres","mcav")
   df$n <- c(1:nrow(df))
   cat(paste0("Data is validated (", runtime(time)," s)...\n"))
   return(df)
}

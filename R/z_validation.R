
#Validation of dataframes

z_validation <- function(df, name, cols){

   if(!is.null(df)){
      if(sapply(df,class) != "numeric"){
         stop(paste0("The full \'", name, "\' dataframe must be numeric"))
      }else if(ncol(df) != cols){
         stop(paste0("The \'", name, "\' dataframe must contain ", cols, " columns."))
      }
   }
}

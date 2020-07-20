#Check for measurements in blocks and blocks in epochs
z_quality <- function(df, freq, blocksize, blockmin, epochsize, epochmin, overlapping){

   #Quality control of blocks
   if(!is.null(blockmin)){
      cat("..."); time <- Sys.time()

      df <- within(df,del <- ave(n,period,block, FUN = function(x) (length(x)<(freq*blocksize*blockmin))*1))
      del <- length(unique(df$block[df$del == 1]))
      df <- df[df$del != "1",-c(ncol(df))]
      cat(paste0("Quality control - ", del, " block(s) deleted (", runtime(time)," s)...\n"))
   }

   #Quality control of epochs
   if(!is.null(epochmin)){
      cat("..."); time <- Sys.time()
      if(!overlapping){

         df <- within(df,del <- ave(block,period,epoch, FUN = function(x) (length(unique(x))<(epochsize*epochmin))*1))
         del <- length(unique(df$epoch[df$del == 1]))
         df <- df[df$del != "1",-c(ncol(df))]

         cat(paste0("Quality control - ", del, " epoch(s) deleted (", runtime(time)," s)...\n"))
      }else{
         temp_del <- 0
         for(i in unique(df$period)){
            temp_epochs <- unique(unlist(strsplit(df$epoch[df$period == i],",")))
            temp_epochs <- temp_epochs[!is.na(temp_epochs)]
            for(j in temp_epochs){
               if(length(unique(df$block[grep(j,df$epoch[df$period == i])]))<epochsize*epochmin){
                  temp_del <- temp_del+1
                  df$epoch[df$period == i] <- gsub(j,",",df$epoch[df$period == i])
               }
            }

         }
         cat(paste0("Quality control - ", temp_del, " epoch(s) deleted (", runtime(time)," s)...\n"))
      }

   }
   return(df)
}

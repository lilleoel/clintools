#Define blocks
z_blocks <- function(df,freq,blocksize){
   cat("..."); time <- Sys.time()

   df <- within(df, block <- ave(n,period,FUN = function(x) x-min(x)+1))
   df <- within(df,block <- ave(block,period,FUN = function(x) ceiling(x/(blocksize*freq))))

   cat(paste0("Blocks created (", runtime(time)," s)...\n"))
   return(df)
}

#add quality into this function as default!!! 
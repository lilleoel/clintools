# 'runtime' counts how long the script was to execute.
runtime <- function(x){
   round(as.numeric(difftime(Sys.time(),x, units="secs")),digits=2)
}

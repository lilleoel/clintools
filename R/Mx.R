# ==== DOCUMENTATION ====

#' Mean flow index (Mx)
#'
#' `Mx()` returns a dataframe with results, missingness for every trigger-period or epoch depending on the chosen output.
#'
#' Using raw recording, this functions calculates
#' mean flow veolicty index (Mx), initially described
#' by Czosnyka et al. in 1996. \code{\link{Mx}}
#'
#' @name Mx
#'
#' @usage Mx(df, trigger = NULL, deleter = NULL,
#' blocksize = 3, epochsize = 20, freq = 1000,
#' blockmin = 0.5, epochmin = 0.5,
#' overlapping = FALSE,
#' output = 'period', fast = FALSE)
#'
#' @param df Raw recording with data in 3 columns: time (in seconds), pressure curve measurement (e.g. CPP, raw measurement), and velocity curve measurement (e.g. MCAv, raw measurement). \code{(dataframe)}
#' @param del Deleter with two columns, reference to pressure curve measurement: (1) start of deletion period and (2) end of deletion period. Every row is a deletion period. \code{"dataframe"}
#' @param trigger Trigger with two columns: (1) start of analysed period and (2) end of analysed period. Every row is a period for analysis. \code{"dataframe"}
#' @param blocksize Size of blocks, in seconds. \code{"numeric"}
#' @param epochsize Size of epochs, in number of blocks. \code{"numeric"}
#' @param freq Frequency of recorded data, in Hz. \code{"numeric"}
#' @param blockmin Minimum measurements required to create a block. \code{"numeric"}
#' @param epochmin Minimum blocks required to create an epoch. \code{"numeric"}
#' @param overlapping The number of block which should overlap, and remain blank if overlapping should not be utilized. \code{"numeric"}
#' @param output Select the output which has to be either one row per. (\code{"period"} or \code{"epoch"})
#' @param fast Select if you want the data to aggregated resulting in a faster, but perhabs more imprecise run. \code{"numeric"}
#'
#' @return Returns a dataframe with the results, with either
#' every period  or epoch as a rows, depending on the chosen output
#'
#' @examples
#' data <- data.frame(seq(1, 901, 0.01),
#'          rnorm(90001), rnorm(90001))
#' Mx(df=data, freq=100)
#'
#' @export
#
# ==== FUNCTION ====

globalVariables(c("block","epoch","n","period","overlapping"))

Mx <- function(
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

# ==== DOCUMENTATION ====

#' Transfer function analysis of dynamic cerebral autoregulation (TFA)
#'
#' `TFA()` calculates dynamic cerebral autoregulation trough a transfer function analysis from a *continuous* recording. This function follows the recommendations from Claassen et al. \[1\] and mimicks the matlab script created by David Simpsons in 2015 (\href{http://www.car-net.org/content/resources#tabTools}{Matlab TFA function}). `TFA()` also includes the possibility to analyse raw recordings with application of cyclic (beat-to-beat) average with the possiblity of utilizing interpolation. (see **details**).
#'
#' @name TFA
#'
#' @usage TFA(df, variables,
#' trigger = NULL, deleter = NULL,
#' freq = 1000, fast = 50, raw_data = FALSE,
#' interpolation = 3, output = "table",
#' vlf = c(0.02,0.07),lf = c(0.07,0.2),
#' hf = c(0.2,0.5), detrend = FALSE,
#' spectral_smoothing = 3,
#' coherence2_thresholds = cbind(c(3:15),
#' c(0.51,0.40,0.34,0.29,0.25,0.22,0.20,0.18,
#' 0.17,0.15,0.14,0.13,0.12)),
#' apply_coherence2_threshold = TRUE,
#' remove_negative_phase = TRUE,
#' remove_negative_phase_f_cutoff = 0.1,
#' normalize_ABP = FALSE,
#' normalize_CBFV = FALSE,
#' window_type = 'hanning',
#' window_length = 102.4,
#' overlap = 59.99,
#' overlap_adjust = TRUE,
#' na_as_mean = TRUE)
#'
#' @param df Raw *continuous* recording with numeric data and first column has to be time in seconds. (`dataframe`)
#'
#' @param variables Definition of the type and order of recorded variables as a list. Middle cerebral artery blood velocity (`'mcav'`) and arterial blood pressure (`'abp'`) is currently supported. (`list`)
#'
#' @param trigger Trigger with two columns:  first is start, and second is end of period to be analyzed. Every row is a period for analysis. Default is `NULL`, which results in analysis of the full dataframe.  (`dataframe`)
#'
#' @param deleter Deleter with two columns: first is start and second is end of period with artefacts, which need to be deleted. Every row is a period with artefacts. Default is `NULL`. (`dataframe`)
#'
#' @param freq Frequency of recorded data, in Hz. Default is `1000`. (`numeric`)
#'
#' @param fast Select if you want the data to aggregated resulting in a faster, but perhaps more imprecise run, in Hz. Default is `50` (`numeric`)
#'
#' @param raw_data Select `TRUE` if the data is raw and cyclic mean should be calculated. **NB:** this function have not been validated, why validated methods for calculating cyclic mean are preferred. Default is `FALSE` (`boolian`)
#'
#' @param interpolation Select the number of beats which should be interpolated. Default is up to `3` beats and `0` results in no interpolation. (`numeric`)
#'
#' @param output Select what the output should be. `'table'` results in a dataframe with values for the three frequencies defined by Claassen et al. \[1\]; `'long'` results in a dataframe with the results in a long format; `'plot'` results in a daframe which can help plot gain, phase and coherence; `'plot-peak'` results in a dataframe, which can be used to validate the cyclic average, and `'raw'` results in a nested list with results primarily for debugging. Default is `'table'`. (`string`)
#'
#' @param vlf,lf,hf,detrend,spectral_smoothing,coherence2_thresholds,apply_coherence2_threshold,remove_negative_phase,remove_negative_phase_f_cutoff,normalize_ABP,normalize_CBFV,window_type,window_length,overlap,overlap_adjust,na_as_mean See **TFA-parameters**
#'
#' @details
#'
#' Using a *continuous* raw recording, `TFA()` calculates dynamic cerebral autoregulation trough a transfer function analysis. This function utilizes the recommendations from Claassen et al \[1\] and mimicks the matlab script created by David Simpsons in 2015.
#'
#' ```
#' View(data)
#' ```
#' | `time` | `abp` | `mcav` |
#' | --: | --: | --: |
#' | `7.00` | `78` | `45` |
#' | `7.01` | `78` | `46` |
#' | `...` | `...` | `...` |
#' | `301.82` | `82` | `70` |
#' | `301.83` | `81` | `69` |
#'
#' To calculate the variables insert the data and select the relevant variables.
#'
#' ```
#' TFA(df=data, variables=c("abp","mcav"))
#' ```
#' See **Value** for output description.
#'
#' @return `TFA()` returns a dataframe depending on the output selected. `'table'` results in a dataframe with values for the three frequencies defined by Claassen et al. \[1\]; `'long'` results in a dataframe with the results in a long format; `'plot'` results in a daframe which can help plot gain, phase and coherence; `'plot-peak'` results in a dataframe, which can be used to validate the cyclic average, and `'raw'` results in a nested list with results primarily for debugging.
#'
#' Some generic variables are listed below:
#' - `abp_power` - The blood pressure power measured in mmHg^2.
#' - `cbfv_power` - The cerebral blood flow velocity power measured in cm^2\*s^-2
#' - `coherence` - Coherence.
#' - `gain_not_normal` - Not normalized gain measured in cm\*s^-1\*mmHg^-1.
#' - `gain_normal` - Normalized gain measured in %\*mmHg^-1.
#' - `phase` - Phase measured in radians.
#'
#' ## output = 'table'
#' Wide format output table with period, VLF, LF, and HF as columns, and the TFA-variables as rows.
#'
#' | `period` | `variable` | `vlf` | `lf` | `hf` |
#' | --- | --: | --: | --: | --: |
#' | `1` | `abp_power` | `6.25` | `1.56` | `0.21` |
#' | `1` | `cbfv_power` | `3.22` | `2.25` | `0.30` |
#' | `...` | `...` | `...` | `...` | `...` |
#' | `3` | `gain_normal` | `1.04` | `1.48` | `1.85` |
#' | `3` | `phase` | `53.0` | `25.4` | `9.38` |
#'
#'
#' ## output = 'long'
#' Long format output table which can be manipulated depending on the intended use, with period, interval, variables and values as columns.
#'
#' | `period` | `interval` | `variable` | `values` |
#' | --: | --- | --- | --: |
#' | `1` | `hf` | `abp_power` | `6.25` |
#' | `1` | `hf` | `cbfv_power` | `3.22` |
#' | `...` | `...` | `...` | `...` |
#' | `2` | `vlf` | `gain_norm` | `1.85` |
#' | `2` | `vlf` | `phase` | `9.38` |
#'
#' ## output = 'plot'
#' Plot format output table which can be used to draw figures with gain, phase and coherence depending on frequency.
#'
#' | `period` | `freq` | `gain` | `phase` | `coherence` |
#' | --: | --: | --: | --: | --: |
#' | `1` | `0.00` | `0.16` | `0.00` | `0.04` |
#' | `1` | `0.01` | `0.29` | `4.22` | `0.29` |
#' | `...` | `...` | `...` | `...` | `...` |
#' | `2` | `1.55` | `1.15` | `-43.2` | `0.64` |
#' | `2` | `1.56` | `1.16` | `-41.1` | `0.42` |
#'
#' @section TFA-paramters:
#' A series of parameters that control TFA analysis (window-length, frequency bands …). If this is not provided, default values, corresponding to those recommended in the white paper, will be used. These default values are given below for each parameter.
#'
#' - `vlf` Limits of *very low frequency* band (in Hz). This corresponds to the matematical inclusion of `[X:Y[`. Default is `c(0.02-0.07)`.
#' - `lf` Limits of *low frequency* band (in Hz). This corresponds to the matematical inclusion of `[X:Y[`. Default is `c(0.07-0.2)`.
#' - `hf` Limits of *high frequency* band (in Hz). This corresponds to the matematical inclusion of `[X:Y[`. Default is `c(0.2-0.5)`.
#' - `detrend` Linear detrending of data prior to TFA-analysis (detrending is carried out as one continuous trend over the whole length of the recording, not segment-by-segment). Default is `FALSE`.
#' - `spectral_smoothing` The length, in samples, of the triangular spectral smoothing function. Note that this must be an odd number, to ensure that smoothing is symmetrical around the centre frequency. Default is `3`.
#' - `coherence2_thresholds` The critical values (alpha=5%, second column) for coherence for a number of windows (first column, here from 3 to 15). These values were obtained by Monte Carlo simulation, using the default parameter settings for the TFA-analysis (Hanning window, overlap of 50% and 3-point spectral smoothing was assumed). These values should be recalculated for different settings. Note that if `overlap_adjust=TRUE`, the overlap will vary depending on the length of data. With an overlap of 60% (see below), the critical values increase by between 0.04 (for 3 windows) and 0.02 (for 15 windows). Default is `cbind(c(3:15),c(0.51,0.40,` `0.34,0.29,0.25,0.22,0.20,0.18,0.17,` `0.15,0.14,0.13,0.12))`.
#' - `apply_coherence2_threshold` Apply the thresholds given above to the TFA-estimates. All frequencies with magnitude-squared coherence below the threshold value are excluded from averaging when calculating the mean values of gain and phase across the bands. Note that low values of coherence are not excluded in the average of coherence across the bands. Default is `TRUE`.
#' - `remove_negative_phase` Remove (ignore) negative values of phase in averaging across bands. Negative phase values are removed only for frequencies below the frequency given below, when calculating the average phase in bands. Default is `TRUE`.
#' - `remove_negative_phase_f_cutoff` The cut-off frequency below-which negative phase values are neglected (only if `remove_negative_phase` is `TRUE`). Default is `0.1`.
#' - `normalize_ABP` Normalize ABP by dividing by the mean and multiplying by 100, to express ABP change in %. Note that mean-values are always removed from ABP prior to analysis. Default is `FALSE`.
#' - `normalize_CBFV` Normalize CBFV by dividing by the mean and multiplying by 100, to express CBFV change in %. Note that the band-average values of gain are always calculated both with and without normalization of CBFV, in accordance with the recommendations. Note also that mean-values are always removed from CBFV prior to analysis. Default is `FALSE`.
#' - `window_type` Chose window `'hanning'` or `'boxcar'`. Default is `'hanning'`.
#' - `window_length` Length of the data-window, in seconds. Default is `102.4`.
#' - `overlap` Overlap of the windows, in %. If `overlap_adjust` is `TRUE` (see below), then this value may be automatically reduced, to ensure that windows cover the full length of data. Default is `59.99%` rather than 60%, so that with data corresponding to 5 windows of 100 s at an overlap of 50%, 5 windows are indeed chosen.
#' - `overlap_adjust` Ensure that the full length of data is used (i.e. the last window finishes as near as possible to the end of the recording), by adjusting the overlap up to a maximum value given by params.overlap. Default is `TRUE`.
#' - `na_as_mean` Changes all missing non-interpolated values to the mean value of the corresponding variable. This have not been adressed in the paper by Claassen, and to ensure the dataframes are not 'gathered' this should generate the most stable results. Default is `TRUE`.
#'
#'
#' @references
#' 1. Claassen et al. (2016) J Cereb Blood Flow Metab. 2016 Apr;36(4):665-80. (\href{https://pubmed.ncbi.nlm.nih.gov/26782760/}{PubMed})
#'
#' @examples
#' data(tfa_sample_data)
#' TFA(tfa_sample_data[,c(1:3)], variables=c("abp","mcav"), freq=10)
#'
#' @export
#
# ==== TFA ====
globalVariables(c("n","period","overlapping"))

TFA <- function(
   #Dataframes
   df, variables,
   trigger = NULL, deleter = NULL,
   freq = 1000, fast = 50,
   raw_data = FALSE, interpolation = 3,
   output = "table",
   #TFA-settings
   vlf = c(0.02,0.07),
   lf = c(0.07,0.2),
   hf = c(0.2,0.5),
   detrend = FALSE,
   spectral_smoothing = 3,
   coherence2_thresholds = cbind(c(3:15),c(0.51,0.40,0.34,0.29,0.25,0.22,0.20,0.18,0.17,0.15,0.14,0.13,0.12)),
   apply_coherence2_threshold = TRUE,
   remove_negative_phase = TRUE,
   remove_negative_phase_f_cutoff = 0.1,
   normalize_ABP = FALSE,
   normalize_CBFV = FALSE,
   window_type = 'hanning', #alternative BOXCAR
   window_length = 102.4, #in s
   overlap = 59.99,
   overlap_adjust = TRUE,
   na_as_mean = TRUE
){
   colnames(df) <- c("t",variables)
   df <- df[,c("t","abp","mcav")]
   variables <- colnames(df)[-1]

   #OPTIMIZE
   df <- Z.fast(df,freq,fast)
   freq <- Z.fast_ftf(freq,fast)

   #DATA MANAGEMENT
   df <- Z.datamanagement(df, variables, trigger, deleter, blocksize = NULL, freq)

   if(raw_data){
      #PEAK IDENTIFICATION
      df <- Z.peak_identification(df, variables, deleter, freq)

      #INTERPOLATION
      df <- Z.interpolation(df, variables, interpolation, deleter)

      #PEAK IDENTIFICATION - PLOT
      if(output == "plot-peak"){
         df$abp_cyclicmean[!is.na(df$abp_cyclicmean)] <- df$abp[!is.na(df$abp_cyclicmean)]
         df$mcav_cyclicmean[!is.na(df$mcav_cyclicmean)] <- df$mcav[!is.na(df$mcav_cyclicmean)]
         return(df);
      }

      #Organize columns
      df$abp <- df$abp_cyclicinterpol
      df$mcav <- df$mcav_cyclicinterpol
      df <- df[,c("period","t",variables)]
   }

   result <- NULL
   if(output != "raw"){
      for(i in unique(df$period)){
         df_i <- df[df$period == i,]
         result <- Z.TFA_func(df_i[,c("abp","mcav")], freq, output, vlf, lf, hf,
                                detrend, spectral_smoothing, coherence2_thresholds,
                                apply_coherence2_threshold, remove_negative_phase,
                                remove_negative_phase_f_cutoff, normalize_ABP, normalize_CBFV,
                                window_type, window_length, overlap, overlap_adjust, na_as_mean)
         result <- as.data.frame(cbind(i,result))
         colnames(result)[1] <- "period"
      }
   }else{
      result <- Z.TFA_func(df_i[,c("abp","mcav")], freq, output, vlf, lf, hf,
                           detrend, spectral_smoothing, coherence2_thresholds,
                           apply_coherence2_threshold, remove_negative_phase,
                           remove_negative_phase_f_cutoff, normalize_ABP, normalize_CBFV,
                           window_type, window_length, overlap, overlap_adjust, na_as_mean)
   }

   return(result)
   #TFA calculation

}

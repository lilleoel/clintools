\name{Mxa}
\alias{Mxa}
\title{Mean flow index!}
\description{
Calculates Mxa, and further described in following publication ...
[Unifiying methodogy of calculation of Mxa (2019)]
}
\usage{
Mxa(data, deleter, trigger, blocksize, epochsize, freq, cyclic_mean,
  cyclic_w, cyclic_span, draw_trigger, draw_peaks, measurementmin, blockmin,
  epochmin, gather, interpolate, firstlast, outsidedeleter)
}
\arguments{
  \item{data}{Data imported with 3 columns: time (in seconds), blood pressure (raw measurement), and MCAv (raw measurement). If you extracted as cyclic mean from e.g. Labchart see 'cyclic_mean'. (dataframe)}
  \item{deleter}{Deleter periods with two columns, first column is start of deletion period and last column is end of deletion period, every row is a deletion period. (dataframe) }
  \item{trigger}{Trigger periods with two columns, first column is start of analysed period and last column is end of analysed period, every row is a period for analysis. (dataframe)}
  \item{blocksize}{Size of blocks created in seconds, default is 3 s. (numeric)}
  \item{epochsize}{Size of epochs defined as number of blocks, default is 20 blocks in every epoch. (numeric)}
  \item{freq}{Frequency of recorded data, default is 1000 Hz. (numeric)}
  \item{cyclic_mean}{Activates calculation of cyclic mean via script, default is TRUE, and if cyclic mean has been calculated with another software change the boolian to FALSE. (boolian)}
  \item{cyclic_w}{Threshold for identification of peaks, higher threshold decrease number of peaks, default is 400. (numeric)}
  \item{cyclic_span}{Details during identifications of peaks, lower span increase the accuracy of peak identification, default is 0.001 (numeric)}
  \item{draw_trigger}{Activates drawing of figure with peaks identified, default is TRUE, because one would need to verify sufficient peak identification for every measurement (boolian)}
  \item{draw_peaks}{- (boolian)}
  \item{measurementmin}{Minimum ratio of measurements in every block, default is 0.5 (numeric)}
  \item{blockmin}{Minimum ratio of blocks in every epoch, default is 0.5 (numeric)}
  \item{epochmin}{Minimum ratio of epochs to calculate Mxa, default is 0.8 (numeric)}
  \item{gather}{Activates aggregation of measurements so missing data is removed and data is continous, default is FALSE (boolian)}
  \item{interpolate}{... Not functional yet ...}
  \item{firstlast}{Activates removal of data before first peak and after last peak of every trigger period, default is TRUE (boolian)}
  \item{outsidedeleter}{Activates removal of data peak-periods with missing data, default is TRUE (boolian)}
}
\examples{
\donttest{Mxa(data, deleter = deleter, trigger = triggers)}
}

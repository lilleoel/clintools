\name{cm_dx}
\alias{cm_dx}
\title{Diastolic flow velocity index}
\description{
Calculates Dx, and further described in following publication ...
[Unifiying methodogy of calculation of Mxa (2019)]
}
\usage{
cm_dx(df, del_1, del_2, trigger, blocksize, epochsize, freq,
blockmin, epochmin, overlapping, output)
}
\arguments{
  \item{df}{Data imported with 3 columns: time (in seconds), blood pressure (raw measurement), and MCAv (raw measurement).}
  \item{del_1}{Deleter periods for pressure with two columns, first column is start of deletion period and last column is end of deletion period, every row is a deletion period. (dataframe) }
  \item{del_2}{Deleter periods for mcav with two columns, first column is start of deletion period and last column is end of deletion period, every row is a deletion period. (dataframe) }
  \item{trigger}{Trigger periods with two columns, first column is start of analysed period and last column is end of analysed period, every row is a period for analysis. (dataframe)}
  \item{blocksize}{Size of blocks created in seconds, default is 3 s. (numeric)}
  \item{epochsize}{Size of epochs defined as number of blocks, default is 20 blocks in every epoch. (numeric)}
  \item{freq}{Frequency of recorded data, default is 1000 Hz. (numeric)}
  \item{blockmin}{Minimum measurements ratio in every block, default is 0.5 (numeric)}
  \item{epochmin}{Minimum block ratio in every epoch, default is 0.8 (numeric)}
  \item{overlapping}{TRUE if overlapping calculations (boolian)}
  \item{output}{select the resolution of output, either 'period' or 'epoch' (boolian)}
}
\examples{
data <- data.frame(time=seq(7, 918, 0.001), pres=rnorm(911001),
                    mcav=rnorm(911001))
cm_dx(df=data)
}

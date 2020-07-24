\name{cm_ri}
\alias{cm_ri}
\title{Resistance index}
\description{
Calculates the resistance index, and further described in following publication ...
[clinmon: An R package for calculation of clinical monitoring indices (2019)]
}
\usage{
cm_ri(df, del, trigger, blocksize, freq, blockmin, output)
}
\arguments{
  \item{df}{Data imported with 2 columns: time (in seconds), physiological measurement (raw measurement).}
  \item{del}{Deleter periods for pressure with two columns, first column is start of deletion period and last column is end of deletion period, every row is a deletion period. (dataframe) }
  \item{trigger}{Trigger periods with two columns, first column is start of analysed period and last column is end of analysed period, every row is a period for analysis. (dataframe)}
  \item{blocksize}{Size of blocks created in seconds, default is 3 s. (numeric)}
  \item{freq}{Frequency of recorded data, default is 1000 Hz. (numeric)}
  \item{blockmin}{Minimum measurements ratio in every block, default is 0.5 (numeric)}
  \item{output}{select the resolution of output, either 'period' or 'block' (boolian)}
}
\examples{
data <- data.frame(time=seq(7, 918, 0.001), pres=rnorm(911001))
cm_ri(df=data)
}

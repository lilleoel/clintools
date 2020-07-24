\name{cm_co}
\alias{cm_co}
\title{Cardiovascular resistance}
\description{
Calculates the co, and further described in following publication ...
[clinmon: An R package for calculation of clinical monitoring indices (2019)]
Co - https ://www.ncbi.nlm.nih.gov/ pmc / articles /PMC5317099/
 CO (EST) = PP / (SBP+DBP) * HR
}
\usage{
cm_co(df, del_1, del_2, trigger, blocksize, freq, blockmin, output)
}
\arguments{
  \item{df}{Data imported with 2 columns: time (in seconds), physiological measurement (raw measurement).}
  \item{del_1}{Deleter periods for pressure with two columns, first column is start of deletion period and last column is end of deletion period, every row is a deletion period. (dataframe) }
  \item{del_2}{Deleter periods for pressure with two columns, first column is start of deletion period and last column is end of deletion period, every row is a deletion period. (dataframe) }
  \item{trigger}{Trigger periods with two columns, first column is start of analysed period and last column is end of analysed period, every row is a period for analysis. (dataframe)}
  \item{blocksize}{Size of blocks created in seconds, default is 3 s. (numeric)}
  \item{freq}{Frequency of recorded data, default is 1000 Hz. (numeric)}
  \item{blockmin}{Minimum measurements ratio in every block, default is 0.5 (numeric)}
  \item{output}{select the resolution of output, either 'period' or 'block' (boolian)}
}
\examples{
data <- data.frame(time=seq(7, 918, 0.001), pres=rnorm(911001),
                    hr=rnorm(911001))
cm_co(df=data)
}

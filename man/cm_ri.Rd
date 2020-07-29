\name{cm_ri}
\alias{cm_ri}
\title{Resistance index (RI)}
\description{
Calculates the resistance index, and further described in following publication ...
[clinmon: An R package for calculation of clinical monitoring indices (2020)]
}
\usage{
cm_ri(df, del = NULL, trigger = NULL,
      blocksize = 3, freq, blockmin = 0.5,
      output = "period")
}
\arguments{
  \item{df}{
    Raw recording with data in 2 columns: time (in seconds) and CBFv measurement (e.g. MCAv, raw measurement).
    \emph{[dataframe]} }
  \item{del}{
    Deleter with two columns: (1) start of deletion period and (2) end of deletion period.
    Every row is a deletion period. \emph{[dataframe]} }
  \item{trigger}{
    Trigger with two columns: (1) start of analysed period and (2) end of analysed period.
    Every row is a period for analysis. \emph{[dataframe]} }
  \item{blocksize}{
    Size of blocks, in seconds. \emph{[numeric]} }
  \item{freq}{
    Frequency of recorded data, in Hz. \emph{[numeric]} }
  \item{blockmin}{
    Minimum measurements required to create a block \emph{[numeric]} }
  \item{output}{
    Select the output which has to be either one row per 'period' or 'block'. \emph{[boolian]} }
}
\examples{
  data <- data.frame(time=seq(1, 901, 0.001),
                      pres=rnorm(900001))

  cm_ri(df=data, freq=1000)
}

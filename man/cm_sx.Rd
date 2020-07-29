\name{cm_sx}
\alias{cm_sx}
\title{Systolic flow velocity index (Sx)}
\description{
Calculates Sx, and further described in following publication ...
[clinmon: An R package for calculation of clinical monitoring indices (2020)]
}
\usage{
cm_sx(df, del_1 = NULL, del_2 = NULL,
      trigger = NULL, blocksize = 3,
      epochsize = 20, freq, blockmin = 0.5,
      epochmin = 0.5, overlapping = FALSE,
      output = 'period')
}
\arguments{
  \item{df}{
    Raw recording with data in 3 columns: time (in seconds), pressure curve measurement (e.g. CPP, raw measurement), and velocity curve measurement (e.g. MCAv, raw measurement).
    \emph{[dataframe]} }
  \item{del_1}{
    Deleter with two columns, reference to pressure curve measurement: (1) start of deletion period and (2) end of deletion period.
    Every row is a deletion period. \emph{[dataframe]} }
  \item{del_2}{
    Deleter with two columns, reference to velocity curve measurement: (1) start of deletion period and (2) end of deletion period.
    Every row is a deletion period. \emph{[dataframe]} }
  \item{trigger}{
  Trigger with two columns: (1) start of analysed period and (2) end of analysed period.
    Every row is a period for analysis. \emph{[dataframe]} }
  \item{blocksize}{
    Size of blocks, in seconds. \emph{[numeric]} }
  \item{epochsize}{
    Size of epochs, in number of blocks. \emph{[numeric]} }
  \item{freq}{
    Frequency of recorded data, in Hz. \emph{[numeric]} }
  \item{blockmin}{
    Minimum measurements required to create a block \emph{[numeric]} }
  \item{epochmin}{
    Minimum blocks required to create an epoch \emph{[numeric]} }
  \item{overlapping}{
    The number of block which should overlap, and remain blank if overlapping should not be utilized \emph{numeric} }
  \item{output}{Select the output which has to be either one row per 'period' or 'epoch'. \emph{[boolian]} }
}
\examples{
data <- data.frame(time=seq(1, 901, 0.001),
                  pres=rnorm(900001),
                  mcav=rnorm(900001))
cm_sx(df=data, freq=1000)
}

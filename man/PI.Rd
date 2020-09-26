\name{PI}
\alias{PI}
\title{Pulsatility index (Gosling's index, PI)}
\description{
Calculates the pulsatility index, and further described in following publication ...
[clinmon: An R package for calculation of clinical monitoring indices (2020)]
}
\usage{
PI(df, del = NULL, trigger = NULL,
      blocksize = 3, freq, blockmin = 0.5,
      output = 'period')
}
\arguments{
  \item{df}{
    Raw recording with data in 2 columns: time (in seconds) and pressure/velocity curve measurement (e.g. ABP, raw measurement).
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
\value{
Returns a dataframe with the results, with either every period  or block as a rows, depending on the chosen output
}
\examples{
data <- data.frame(time=seq(1, 901, 0.01),
                  pres=rnorm(90001))
PI(df=data, freq=100)
}

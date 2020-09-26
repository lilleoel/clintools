\name{PRx}
\alias{PRx}
\title{Pressure reactivity index (PRx)}
\description{
Calculates PRx, and further described in following publication ...
[clinmon: An R package for calculation of clinical monitoring indices (2020)]
}
\usage{
PRx(df, del_1 = NULL, del_2 = NULL,
      trigger = NULL, blocksize = 3,
      epochsize = 20, freq, blockmin = 0.5,
      epochmin = 0.5, overlapping = FALSE,
      output = 'period')
}
\arguments{
  \item{df}{
    Raw recording with data in 3 columns: time (in seconds), arterial blood pressure curve measurement (ABP, raw measurement), and intracranial pressure curve measurement (ICP, raw measurement).
    \emph{[dataframe]} }
  \item{del_1}{
    Deleter with two columns, reference to arterial blood pressure curve measurement: (1) start of deletion period and (2) end of deletion period.
    Every row is a deletion period. \emph{[dataframe]} }
  \item{del_2}{
    Deleter with two columns, reference to intracranial pressure curve measurement: (1) start of deletion period and (2) end of deletion period.
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
\value{
Returns a dataframe with the results, with either every period  or epoch as a rows, depending on the chosen output
}
\examples{
data <- data.frame(time=seq(1, 901, 0.01),
                  pres=rnorm(90001),
                  mcav=rnorm(90001))
PRx(df=data, freq=1000)
}

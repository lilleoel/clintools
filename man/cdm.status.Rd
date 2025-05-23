% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cdm.status.R
\name{cdm.status}
\alias{cdm.status}
\title{Trial status for central data monitoring (cdm.status)}
\usage{
cdm.status(d, sample.size, planned.years, rolling.average, caption)
}
\arguments{
\item{d}{list of dates for each participant included.}

\item{sample.size}{the planned sample size}

\item{planned.years}{the planned duration of the trial}

\item{rolling.average}{the number of participant to be included to calculate the predicted inclusion rate}

\item{caption}{boolean to add or remove a small description of the figure.}
}
\value{
returns a list where \verb{$fig} is the figure; \verb{$txt} is as summary
of the findings; and \verb{$df}is the dataframe generated in the function.
}
\description{
\code{cdm.status()} is a function to provide an overview of the trial status
}
\examples{
\dontrun{
   tmp <- cdm.status(d,sample.size=1808)
}

}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/PLR3000.R
\name{PLR3000}
\alias{PLR3000}
\title{NeurOpticsTM PLR-3000 pupillometer file to dataframe (PLR3000)}
\usage{
PLR3000(filename = NULL, df = NULL)
}
\arguments{
\item{filename}{path to the XLS-file with the measurements}

\item{df}{the dataframe can also be used for the function if data is already imported.}
}
\value{
Returns a list with two dataframe, one with the measurements (pupils) and one with the markers (markers).
}
\description{
\code{PLR3000()} is a function which converts the XLS file imported from the eurOpticsTM PLR-3000 pupillometer to a nested list with two dataframes.
}
\examples{
 \dontrun{
   PLR3000("C:/PLR3000/R_20200105_205901.xls")
 }

}

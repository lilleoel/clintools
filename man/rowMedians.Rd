% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rowMedians.R
\name{rowMedians}
\alias{rowMedians}
\title{find median of rows similar to rowMeans (rowMedians)}
\usage{
rowMedians(x, na.rm=FALSE)
}
\arguments{
\item{x}{a dataframe where median of row should be calculated. (\code{data.frame})}

\item{na.rm}{Should missing values be omitted fro the calculations? (\code{boolian})}
}
\value{
Returns a list of the median of each row in the inputted dataframe.
}
\description{
\code{rowMedians()} converts a dataframe to a list of row medians.
}
\examples{
\dontrun{
   rowMedians(df[,c("test1","test2","test3")])
}

}

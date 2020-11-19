#| ------------ |
#|    data10    |
#| ------------ |
#' Test-data (10 Hz)
#'
#' Recording with four columns: time (t), non-invasive arterial
#' blood pressure (nabp), middle cerebral artery velocity measured
#' using transcranial Doppler (tcd), and heart rate (hr).
#'
#' @docType data
#'
#' @usage data(testdata)
#'
#' @format An object of class \code{"dataframe"}; an example of the
#' usage in \code{\link[clinmon]{Mx}}.
#'
#' @keywords datasets
#'
#' @references Olsen MH et al. (Unpublished data, 2020)
#' (\href{https://github.com/lilleoel/clinmon}{GitHub})
#'
#' @source \href{https://github.com/lilleoel/clinmon}{GitHub}
#'
#' @examples
#' data(testdata)
#' Mx(data10[,c(1:3)],freq=10)
"data10"

#| ------------ |
#|   data1000   |
#| ------------ |
#' Test-data (1000 Hz)
#'
#' Recording with four columns: time (t), non-invasive arterial
#' blood pressure (nabp), middle cerebral artery velocity measured
#' using transcranial Doppler (tcd), and heart rate (hr).
#'
#' @docType data
#'
#' @usage data(testdata)
#'
#' @format An object of class \code{"dataframe"}; an example of the
#' usage in \code{\link[clinmon]{Mx}}.
#'
#' @keywords datasets
#'
#' @references Olsen MH et al. (Unpublished data, 2020)
#' (\href{https://github.com/lilleoel/clinmon}{GitHub})
#'
#' @source \href{https://github.com/lilleoel/clinmon}{GitHub}
#'
#' @examples
#' data(testdata)
#' Mx(data1000[,c(1:3)],freq=1000)
"data1000"

#| ------------ |
#|      del     |
#| ------------ |
#' Test-deleter
#'
#' Deleter dataframe with two columns: start (start) and
#' end (end) of the deleter-period.
#'
#' @docType data
#'
#' @usage data(testdata)
#'
#' @format An object of class \code{"dataframe"}; an example of the
#' usage in \code{\link[clinmon]{Mx}}.
#'
#' @keywords datasets
#'
#' @references Olsen MH et al. (Unpublished data, 2020)
#' (\href{https://github.com/lilleoel/clinmon}{GitHub})
#'
#' @source \href{https://github.com/lilleoel/clinmon}{GitHub}
#'
#' @examples
#' data(testdata)
#' Mx(data1000[,c(1:3)],del=del,freq=1000)
"del"

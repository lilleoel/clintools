#| ------------ |
#|    data10    |
#| ------------ |
#' Test-data (10 Hz)
#'
#' Recording with four columns: time (t), non-invasive arterial
#' blood pressure (abp), middle cerebral artery velocity measured
#' using transcranial Doppler (mcav), and heart rate (hr).
#'
#' @docType data
#'
#' @usage data(testdata)
#'
#' @format An object of class \code{"dataframe"}; an example of the
#' usage in \code{\link[clinmon]{clinmon}}.
#'
#' @keywords datasets
#'
#' @references Olsen MH et al. (Unpublished data, 2020) (\href{https://github.com/lilleoel/clinmon}{GitHub})
#'
#' @source \href{https://github.com/lilleoel/clinmon}{GitHub}
#'
#' @examples
#' data(testdata)
#' variables <- c("abp","mcav","hr)
#' clinmon(df.data10,variables,freq=10)
"df.data10"

#| ------------ |
#|   data1000   |
#| ------------ |
#' Test-data (1000 Hz)
#'
#' Recording with four columns: time (t), non-invasive arterial
#' blood pressure (abp), middle cerebral artery velocity measured
#' using transcranial Doppler (mcav), and heart rate (hr).
#'
#' @docType data
#'
#' @usage data(testdata)
#'
#' @format An object of class \code{"dataframe"}; an example of the
#' usage in \code{\link[clinmon]{clinmon}}.
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
#' variables <- c("abp","mcav","hr)
#' clinmon(df.data1000,variables)
"df.data1000"

#| ------------ |
#|    deleter   |
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
#' usage in \code{\link[clinmon]{clinmon}}.
#'
#' @keywords datasets
#'
#' @references Olsen MH et al. (Unpublished data, 2020) (\href{https://github.com/lilleoel/clinmon}{GitHub})
#'
#' @source \href{https://github.com/lilleoel/clinmon}{GitHub}
#'
#' @examples
#' data(testdata)
#' variables <- c("abp","mcav","hr)
#' clinmon(df.data1000,variables,deleter=df.deleter)
"df.deleter"

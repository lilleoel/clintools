#| ------------ |
#|    data10    |
#| ------------ |
#' Test-data - 10 Hz
#'
#' Recording with four columns: time (`t`), non-invasive arterial
#' blood pressure (`abp`), middle cerebral artery velocity measured
#' using transcranial Doppler (`mcav`), and heart rate (`hr`).
#'
#' @name testdata10
#'
#' @docType data
#'
#' @usage data(testdata)
#'
#' @format An object of class \code{"dataframe"}; an example of the
#' usage in \code{\link[clintools]{clinmon}}-function.
#'
#' @keywords datasets
#'
#' @references Olsen MH et al. (Unpublished data, 2020) (\href{https://github.com/lilleoel/clintools}{GitHub})
#'
#' @examples
#' data(testdata)
#' variables <- c("abp","mcav","hr")
#' clinmon(df.data10,variables,freq=10)
"df.data10"

#| ------------ |
#|   data1000   |
#| ------------ |
#' Test-data - 1000 Hz
#'
#' Recording with four columns: time (`t`), non-invasive arterial
#' blood pressure (`abp`), middle cerebral artery velocity measured
#' using transcranial Doppler (`mcav`), and heart rate (`hr`).
#'
#' @docType data
#'
#' @usage data(testdata)
#'
#' @format An object of class \code{"dataframe"}; an example of the
#' usage in \code{\link[clintools]{clinmon}}-function.
#'
#' @keywords datasets
#'
#' @references Olsen MH et al. (Unpublished data, 2020)
#' (\href{https://github.com/lilleoel/clintools}{GitHub})
#'
#' @examples
#' data(testdata)
#' variables <- c("abp","mcav","hr")
#' clinmon(df.data1000,variables,fast=50)
"df.data1000"

#| ------------ |
#|    deleter   |
#| ------------ |
#' Test-deleter
#'
#' Deleter dataframe with two columns: start (`start`) and
#' end (`end`) of the deleter-period.
#'
#' @docType data
#'
#' @usage data(testdata)
#'
#' @format An object of class \code{"dataframe"}; an example of the
#' usage in \code{\link[clintools]{clinmon}}-function.
#'
#' @keywords datasets
#'
#' @references Olsen MH et al. (Unpublished data, 2020) (\href{https://github.com/lilleoel/clintools}{GitHub})
#'
#' @examples
#' data(testdata)
#' variables <- c("abp","mcav","hr")
#' clinmon(df.data1000,variables,deleter=df.deleter,fast=50)
"df.deleter"

#| ------------- |
#|tfa_sample_data|
#| ------------- |
#' TFA sample data
#'
#' Dataframe with data provided by Prof. Simpsons, with time (t), arterial blood pressure (abp), left MCAv (mcav_l), right MCAv (mcav_r), and end-tidal CO2 (etco2).
#'
#' @docType data
#'
#' @usage data(tfa_sample_data)
#'
#' @format An object of class \code{"dataframe"}; an example of the
#' usage in \code{\link[clintools]{TFA}}-function.
#'
#' @keywords datasets
#'
#' @references
#' * Simpsons D (2015) (\href{https://www.car-net.org/tools}{Cerebral Autoregulation Research Network})
#' * Claassen et al. (2016) J Cereb Blood Flow Metab. 2016 Apr;36(4):665-80. (\href{https://pubmed.ncbi.nlm.nih.gov/26782760/}{PubMed})
#'
#' @source \href{https://github.com/lilleoel/clintools}{GitHub}
#'
#' @examples
#' data(tfa_sample_data)
#' TFA(tfa_sample_data[,c(1:3)], variables=c("abp","mcav"), freq=10)
"tfa_sample_data"

#| --------------- |
#|tfa_sample_data_1|
#| --------------- |
#' TFA sample data - 1
#'
#' Dataframe with data provided by Prof. Simpsons, with time (t), arterial blood pressure (abp), left MCAv (mcav_l), right MCAv (mcav_r), and end-tidal CO2 (etco2).
#'
#' @docType data
#'
#' @usage data(tfa_sample_data)
#'
#' @format An object of class \code{"dataframe"}; an example of the
#' usage in \code{\link[clintools]{TFA}}-function.
#'
#' @keywords datasets
#'
#' @references
#' * Simpsons D (2015) (\href{https://www.car-net.org/tools}{Cerebral Autoregulation Research Network})
#' * Claassen et al. (2016) J Cereb Blood Flow Metab. 2016 Apr;36(4):665-80. (\href{https://pubmed.ncbi.nlm.nih.gov/26782760/}{PubMed})
#'
#' @source \href{https://github.com/lilleoel/clintools}{GitHub}
#'
#' @examples
#' data(tfa_sample_data)
#' TFA(tfa_sample_data_1[,c(1:3)], variables=c("abp","mcav"), freq=10)
"tfa_sample_data_1"


#| --------------- |
#|tfa_sample_data_2|
#| --------------- |
#' TFA sample data - 2
#'
#' Dataframe with data provided by Prof. Simpsons, with time (t), arterial blood pressure (abp), left MCAv (mcav_l), right MCAv (mcav_r), and end-tidal CO2 (etco2).
#'
#' @docType data
#'
#' @usage data(tfa_sample_data)
#'
#' @format An object of class \code{"dataframe"}; an example of the
#' usage in \code{\link[clintools]{TFA}}-function.
#'
#' @keywords datasets
#'
#' @references
#' * Simpsons D (2015) (\href{https://www.car-net.org/tools}{Cerebral Autoregulation Research Network})
#' * Claassen et al. (2016) J Cereb Blood Flow Metab. 2016 Apr;36(4):665-80. (\href{https://pubmed.ncbi.nlm.nih.gov/26782760/}{PubMed})
#'
#' @source \href{https://github.com/lilleoel/clintools}{GitHub}
#'
#' @examples
#' data(tfa_sample_data)
#' TFA(tfa_sample_data_2[,c(1:3)], variables=c("abp","mcav"), freq=10)
"tfa_sample_data_2"

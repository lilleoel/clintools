# ==== DOCUMENTATION ====

#' Quiet any output (quietly)
#'
#' `quietly()` is a small function which suppresses any output
#'
#' @name quietly
#'
#' @usage quietly(x)
#'
#' @param x input to be suppressed
#'
#' @return Returns x, but without any output
#'
#' @examples
#' \dontrun{
#' tmp <- quietly(print("hello"))
#' }
#' @export
#
# ==== FUNCTION ====

quietly <- function(x) {
   sink("NUL")
   tryCatch(suppressMessages(x), finally = sink())
}

# ==== DOCUMENTATION ====

#' Load or install multiple packages (libraries)
#'
#' `libraries()` is a small function which can load and/or install multiple packages
#'
#' @name libraries
#'
#' @usage libraries(packages)
#'
#' @param packages list of packages
#'
#' @return outputs the loaded and/or installed packages
#'
#' @examples
#' \dontrun{
#' libraries(c("rjson", "data.table"))
#' }
#'
#' @importFrom utils install.packages
#'
#' @export
#
# ==== FUNCTION ====

libraries <- function(packages){
   for (pkg in packages) {
      if (!require(pkg, character.only = TRUE)) {
         message("Installing ", pkg)
         install.packages(pkg)
         library(pkg, character.only = TRUE)
         message("Loaded: ", pkg)
      } else {
         message("Loaded: ", pkg)
      }
   }
}

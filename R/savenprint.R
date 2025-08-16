# ==== DOCUMENTATION ====

#' Save and print (savenprint)
#'
#' `savenprint()` is a small function which saves figures as *.jpg and prints the figure in a markdown document.
#'
#' @name savenprint
#'
#' @usage savenprint(plot, height, width, dpi, filename, folder, print)
#'
#' @param plot input a ggplot
#' @param height height in inches
#' @param width width in inches
#' @param dpi prefer 600 dpi to print (which is default)
#' @param filename the filename - if null then automatically generated
#' @param folder the folder where the figures whould be stored
#' @param print boolean to say if plot will be printed
#'
#' @return prints and save a figure
#'
#' @examples
#' \dontrun{
#' p <- ggplot(dfa,aes(x=Var2,y=prop,color=Var1)) +
#' geom_vline(aes(xintercept=98),linetype="dotted") +
#' geom_line() + geom_point() +
#' theme_classic()
#'
#' savenprint(p, height = 4, width = 8,
#'    filename = "Figure1", folder="figures")
#' }
#'
#' @importFrom ggplot2 ggsave
#' @export
#
# ==== FUNCTION ====

savenprint <- function(plot, height, width, dpi = 600,
      filename = NULL, folder = NULL, print = TRUE) {
   # Hvis folder er angivet, lav den hvis den ikke findes
   if (!is.null(folder)) {
      if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
   }

   # Generér unikt navn hvis filename ikke er angivet
   if (is.null(filename)) {
      uid <- paste0(sample(c(letters, LETTERS, 0:9), 6, replace = TRUE), collapse = "")
      filename <- sprintf("Fig-%s-%s", format(Sys.Date(), "%Y%m%d"), uid)
   }

   # Tilføj .jpg hvis det mangler
   if (!grepl("\\.jpg$", filename, ignore.case = TRUE)) {
      filename <- paste0(filename, ".jpg")
   }

   # Saml fuld sti
   filepath <- if (!is.null(folder)) file.path(folder, filename) else filename

   # Gem figuren
   ggsave(filepath, plot, width = width, height = height, units = "in", dpi = dpi)

   # Inkluder i output
   if (print) {
      if (knitr::is_latex_output()) {
         knitr::asis_output(sprintf(
            "\\includegraphics[width=\\textwidth,keepaspectratio]{%s}", filepath
         ))
      } else {
         knitr::asis_output(sprintf(
            '<img src="%s" style="width:100%%; height:auto;">', filepath
         ))
      }
   }
}

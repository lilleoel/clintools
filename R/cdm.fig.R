# ==== DOCUMENTATION ====

#' Central data monitoring to assess deviations (cdm.fig)
#'
#' `cdm.fig()` is a function to assess any deviations
#'
#' @name cdm.fig
#'
#' @usage cdm.fig(df, cols, site, meta_title, seedno, output, nmin)
#'
#' @param df      dateframe to be assessed for missing data
#' @param cols     columns to be assessed
#' @param site    column with sites
#' @param meta_title Y-axis lab, if empty then it is the column name
#' @param seedno  the numeric site, if empty it is just `Sys.Date()`
#' @param output  if 'fig' then the figure is the output, any other will output
#' the blinded site table
#' @param nmin    minimum number of variables in site to be presented
#'
#' @return Returns a full markdown output.
#'
#' @examples
#' \dontrun{
#'    cdm.fig(df,cols="Mode of birth",
#'       site="maternal_trial_site")
#'    cdm.fig(df,cols="Gestational Age at birth",
#'       site="maternal_trial_site")
#'    library(knitr)
#'    kable(cdm.fig(df,cols="Mode of birth",
#'       site="maternal_trial_site",output = ""),row.names=F)
#' }
#'
#' @importFrom stringi stri_rand_strings
#' @importFrom ggplot2 geom_boxplot geom_hline labs geom_bar scale_fill_brewer
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom scales percent_format
#'
#' @export
#
# ==== FUNCTION ====

cols=c("mechvent","F05_weightatfollowup")
site="site_id"
meta_title=c("Mechanical Ventilation","Weight")
seedno=NA
output = "fig"
nmin = 5

cdm.fig <- function(df, cols, site = NA, meta_title = NA, seedno=NA,
                    output = "fig", nmin = 5){
   for(i in 1:length(cols)){
      tmp <- df[,c(site,cols[[i]])]
      tmp <- tmp[!is.na(tmp[[cols[[i]]]]),]
      tmp[nchar(as.character(tmp[[cols[[i]]]])) == 0 | is.na(tmp[[cols[[i]]]]),cols[[i]]] <- NA
      if(!is.na(site)){
         if(is.na(seedno)) seedno <- as.numeric(Sys.Date())
         tmp[[site]] <- as.character(tmp[[site]])

         set.seed(seedno)
         blind <- NULL
         blind$site <- unique(tmp[[site]])
         blind$blind <- unique(stringi::stri_rand_strings(999, 2))[1:length(blind$site)]
         blind <- as.data.frame(blind)
         tmp$blind_site <- tmp[[site]]
         for(j in 1:nrow(blind)){
            tmp$blind_site[tmp$blind_site == blind$site[j]] <- blind$blind[j]
         }
         zite <- "blind_site"
      }else{
         tmp$site <- "All"
         zite <- "zite"
      }

      for(j in unique(tmp[[zite]])){
         tmp[[zite]] <- as.character(tmp[[zite]])
         antal <- length(tmp[!is.na(tmp[[zite]]) & tmp[[zite]] == j,zite])
         if(antal < nmin) tmp[tmp[[zite]] == j,cols[[i]]] <- NA

         tmp[!is.na(tmp[[zite]]) & tmp[[zite]] == j,zite] <- paste0(tmp[!is.na(tmp[[zite]]) & tmp[[zite]] == j,zite], " (n=", antal,")")
         tmp[[zite]] <- as.factor(tmp[[zite]])
      }

      if(output != "fig"){
         blind <- blind[order(blind$blind),]
         colnames(blind) <- c("Site ID","Blinded name")
         return(blind)
      }
      if(is.na(meta_title[[i]])) meta_title[[i]] <- cols[[i]]

      #Continuous
      if(class(tmp[[cols[[i]]]]) %in% c("numeric","integer")){
         meanval <- mean(tmp[[cols[[i]]]],na.rm=T)
         g1 <- ggplot() +
            geom_boxplot(aes(y=tmp[[cols[[i]]]],x=tmp[[zite]])) +
            geom_hline(aes(yintercept=meanval),color="red",linetype="dotted") +
            theme_classic() +
            labs(y=meta_title) +
            theme(axis.title.x = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.text.x = element_text(angle=90, vjust = 0.25,hjust=0))
      }

      #Categorical
      if(class(tmp[[cols[[i]]]]) %in% c("factor","character")){
         tmp[[cols[[i]]]] <- factor(tmp[[cols[[i]]]])
         g1 <- ggplot() +
            geom_bar(aes(x=tmp[[zite]],fill=tmp[[cols[[i]]]])) +
            scale_fill_brewer(palette="Paired",na.translate = F) +
            theme_classic() +
            labs(y=meta_title) +
            theme(axis.title.x = element_blank(),
                  axis.ticks.y = element_blank(),
                  axis.text.x = element_text(angle=90, vjust = 0.25,hjust=0),
                  legend.title = element_blank(),
                  legend.position = "top")
      }
      if(length(cols) > 1){
         suppressWarnings(
            print(g1))
         cat("\n\n")
      }else{
         return(g1)
      }
   }
}

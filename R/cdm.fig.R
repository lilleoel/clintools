# ==== DOCUMENTATION ====

#' Central data monitoring to assess deviations (cdm.fig)
#'
#' `cdm.fig()` is a function to assess any deviations
#'
#' @name cdm.fig
#'
#' @usage cdm.fig(df, col, site, meta_title, seedno, output, nmin)
#'
#' @param df      dateframe to be assessed for missing data
#' @param col     column to be assessed
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
#'    cdm.fig(df,col="Mode of birth",
#'       site="maternal_trial_site")
#'    cdm.fig(df,col="Gestational Age at birth",
#'       site="maternal_trial_site")
#'
#'    knitr::kable(cdm.fig(df,col="Mode of birth",
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

cdm.fig <- function(df, col, site = NA, meta_title = NA, seedno=NA,
                    output = "fig", nmin = 5){
   tmp <- df[,c(site,col)]
   tmp <- tmp[!is.na(tmp[[col]]),]
   tmp[nchar(tmp[[col]]) == 0 | is.na(tmp[[col]]),col] <- NA
   if(!is.na(site)){
      if(is.na(seedno)) seedno <- as.numeric(Sys.Date())
      tmp[[site]] <- as.character(tmp[[site]])

      set.seed(seedno)
      blind <- NULL
      blind$site <- unique(tmp[[site]])
      blind$blind <- unique(stringi::stri_rand_strings(999, 2))[1:length(blind$site)]
      blind <- as.data.frame(blind)
      tmp$blind_site <- tmp[[site]]
      for(i in 1:nrow(blind)){
         tmp$blind_site[tmp$blind_site == blind$site[i]] <- blind$blind[i]
      }
      site <- "blind_site"
   }else{
      tmp$site <- "All"
      site <- "site"
   }

   for(i in unique(tmp[[site]])){
      tmp[[site]] <- as.character(tmp[[site]])
      antal <- length(tmp[!is.na(tmp[[site]]) & tmp[[site]] == i,site])
      if(antal < nmin) tmp[tmp[[site]] == i,col] <- NA

      tmp[!is.na(tmp[[site]]) & tmp[[site]] == i,site] <- paste0(tmp[!is.na(tmp[[site]]) & tmp[[site]] == i,site], " (n=", antal,")")
      tmp[[site]] <- as.factor(tmp[[site]])
   }

   if(output != "fig"){
      blind <- blind[order(blind$blind),]
      colnames(blind) <- c("Site ID","Blinded name")
      return(blind)
   }
   if(is.na(meta_title)) meta_title <- col

   #Continuous
   if(class(tmp[[col]]) %in% c("numeric","integer")){
      meanval <- mean(tmp[[col]],na.rm=T)
      g1 <- ggplot() +
         geom_boxplot(aes(y=tmp[[col]],x=tmp[[site]])) +
         geom_hline(aes(yintercept=meanval),color="red",linetype="dotted") +
         theme_classic() +
         labs(y=meta_title) +
         theme(axis.title.x = element_blank(),
               axis.ticks.y = element_blank(),
               axis.text.x = element_text(angle=90, vjust = 0.25,hjust=0))
   }

   #Categorical
   if(class(tmp[[col]]) %in% c("factor","character")){
      tmp[[col]] <- factor(tmp[[col]])

      g1 <- ggplot() +
         geom_bar(aes(x=tmp[[site]],fill=tmp[[col]]), position="fill") +
         scale_fill_brewer(palette="Paired") +
         scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
         theme_classic() +
         labs(y=meta_title) +
         theme(axis.title.x = element_blank(),
               axis.ticks.y = element_blank(),
               axis.text.x = element_text(angle=90, vjust = 0.25,hjust=0),
               legend.title = element_blank(),
               legend.position = "bottom")
   }
   return(g1)

}

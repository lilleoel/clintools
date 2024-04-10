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
#'    library(knitr)
#'    kable(cdm.fig(df,col="Mode of birth",
#'       site="maternal_trial_site",output = ""),row.names=F)
#' }
#'
#' @importFrom stringi stri_rand_strings
#' @importFrom ggplot2 geom_boxplot geom_hline labs geom_bar scale_fill_brewer
#' @importFrom ggplot2 scale_y_continuous
#' @importFrom scales percent_format
#' @importFrom ggpubr ggarrange
#'
#' @export
#
# ==== FUNCTION ====
# df <- df2
# df$F03_mechanicvent <- as.factor(df$F03_mechanicvent)
# col=c("F03_mechanicvent")
# site="site_id"
# meta_title=c("Mechanical Ventilation","Weight")
# seedno=NA
# output = "fig"
# nmin = 5

cdm.fig <- function(df, col, site = NA, meta_title = NA, seedno=NA,
                    output = "fig", nmin = 5){
   if(is.na(seedno)) seedno <- as.numeric(Sys.Date())

   tmp <- NULL
   tmp$col <- df[[col]]

   if(is.na(site)){
      tmp$site <- "All"
      zite <- "site"
   }else{
      tmp$site <- as.character(df[[site]])
      set.seed(seedno)
      blind <- NULL
      blind$site <- unique(tmp$site)
      blind$blind <- unique(stringi::stri_rand_strings(999, 2))[1:length(blind$site)]
      blind <- as.data.frame(blind)
      tmp$blind_site <- tmp$site
      for(j in 1:nrow(blind)){
         tmp$blind_site[tmp$blind_site == blind$site[j]] <- blind$blind[j]
      }
      zite <- "blind_site"
   }
   tmp <- data.frame(tmp)
   tmp$col[nchar(as.character(tmp$col)) == 0 | is.na(tmp$col)] <- NA
   tmp <- tmp[!is.na(tmp$col),]

   zite_size <- NULL
   for(j in unique(tmp[[zite]])){
      tmp[[zite]] <- as.character(tmp[[zite]])
      antal <- length(tmp[!is.na(tmp[[zite]]) & tmp[[zite]] == j,zite])
      if(antal < nmin) tmp$col[tmp[[zite]] == j] <- NA

      zite_size <- c(zite_size,antal)

      tmp[!is.na(tmp[[zite]]) & tmp[[zite]] == j,zite] <-
         paste0(tmp[!is.na(tmp[[zite]]) & tmp[[zite]] == j,zite], " (n=", antal,")")
      tmp[[zite]] <- as.factor(tmp[[zite]])
   }
   if(output != "fig"){
      blind <- blind[order(blind$blind),]
      colnames(blind) <- c("Site ID","Blinded name")
      return(blind)
   }

   if(all(zite_size < nmin)) return(cat("No sites large enough to present data for",col,"\n"))
   #Figures
   if(is.na(meta_title)) meta_title <- col

   #Continuous
   if(class(tmp$col) %in% c("numeric","integer")){
      meanval <- mean(tmp$col,na.rm=T)
      g1 <- ggplot() +
         geom_boxplot(aes(y=tmp$col,x=tmp[[zite]])) +
         geom_hline(aes(yintercept=meanval),color="red",linetype="dotted") +
         theme_classic() +
         labs(y=meta_title) +
         theme(axis.title.x = element_blank(),
               axis.ticks.y = element_blank(),
               axis.text.x = element_text(angle=90, vjust = 0.25,hjust=0))
   }

   #Categorical
   if(class(tmp$col) %in% c("factor","character")){
      tmp$col <- factor(tmp$col)

      ga <- ggplot() +
         geom_bar(aes(x=paste0("All (n=",nrow(tmp),")"), fill=tmp$col)) +
         scale_fill_brewer(palette="Paired",na.translate = F) +
         scale_y_continuous(expand=c(0,0)) +
         theme_classic() +
         labs(y=paste(meta_title,"\n(n)")) +
         theme(axis.title.x = element_blank(),
               axis.text.x = element_text(angle=90, vjust = 0.25,hjust=0),
               legend.title = element_blank(),
               legend.position = "top",
               plot.margin = margin())

      gb <- ggplot() +
         geom_bar(aes(x=tmp[[zite]],fill=tmp$col)) +
         scale_fill_brewer(palette="Paired",na.translate = F) +
         scale_y_continuous(expand=c(0,0)) +
         theme_classic() +
         labs(y=meta_title) +
         theme(axis.title.x = element_blank(),
               axis.text.x = element_text(angle=90, vjust = 0.25,hjust=0),
               legend.title = element_blank(),
               legend.position = "top",
               plot.margin = margin())

      if(length(unique(tmp[[zite]])) > 1){
         g1 <- ggarrange(ga,gb + theme(axis.title.y=element_blank()),align="h",common.legend = T, legend="top")
      }else{
         g1 <- gb
      }


   }
   return(g1)
}


# ==== DOCUMENTATION ====

#' Central data monitoring to assess deviations (cdm.fig)
#'
#' `cdm.fig()` is a function to assess any deviations
#'
#' @name cdm.fig
#'
#' @usage cdm.fig(df, col, site, meta_title, seedno, output, nmin, setting, blind)
#'
#' @param df      dateframe to be assessed for missing data
#' @param col     column to be assessed
#' @param site    column with sites
#' @param meta_title Y-axis lab, if empty then it is the column name
#' @param seedno  the numeric site, if empty it is just `Sys.Date()`
#' @param output  if 'fig' then the figure is the output, any other will output
#' the blinded site table
#' @param nmin    minimum number of variables in site to be presented
#' @param setting if "full" as default it will show all data, if anything else than "full" no site strata will be presented.
#' @param blind should sites be blinded (default is T)
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
#' @importFrom ggplot2 scale_y_continuous sec_axis position_stack coord_flip
#' @importFrom scales percent_format
#' @importFrom ggpubr ggarrange
#'
#' @export
#
# ==== FUNCTION ====
# df=df
# col=longnames[1]
# site="site"
# meta_title = NA
# seedno = NA
# output="fig"
# nmin=5
# setting="SKM"

cdm.fig <- function(df, col, site = NA, meta_title = NA, seedno=NA,
                    output = "fig", nmin = 5, setting="full", blind=T){
   if(is.na(seedno)) seedno <- as.numeric(Sys.Date())

   tmp <- NULL
   tmp$col <- df[[col]]

   if(is.na(site)){
      tmp$site <- "All"
      zite <- "site"
   }else if(blind){
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
   }else{
      zite <- "site"
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
      if(setting=="full"){
         meanval <- mean(tmp$col,na.rm=T)
         tmp_all <- cbind(paste0("All (n=",nrow(tmp),")"),as.character(tmp$col))
         colnames(tmp_all) <- c(zite,"col")
         tmp <- data.frame(rbind(tmp[,c(zite,"col")],tmp_all))
      }else{
         tmp[[zite]] <- paste0("All (n=",nrow(tmp),")")
      }

      tmp$col <- as.numeric(tmp$col)
      tmp[[zite]] <- as.factor(tmp[[zite]])
      tmp[[zite]] <- factor(tmp[[zite]], levels=unique(c(tmp[grepl("^All",tmp[[zite]]),zite],
                                                         tmp[!grepl("^All",tmp[[zite]]),zite])))

      g1 <- ggplot() +
         geom_boxplot(aes(y=tmp$col,x=tmp[[zite]])) +
         theme_classic() +
         labs(y=meta_title) +
         theme(axis.ticks.y = element_blank(),
               axis.text.x = element_text(angle=90, vjust = 0.25,hjust=0))

      if(setting=="full"){
         g1 <- g1 +
            geom_vline(aes(xintercept=1.5), linetype="dashed",alpha=0.1) +
            geom_hline(aes(yintercept=meanval),color="red",linetype="dotted", alpha=0.5) +
            theme(axis.title.x = element_blank())
      }else{
         g1 <- g1 + coord_flip() +
            theme(axis.title.y = element_blank())
      }

   }

   #Categorical
   if(class(tmp$col) %in% c("factor","character")){
      if(setting=="full"){
         tmp_all <- cbind(paste0("All (n=",nrow(tmp),")"),as.character(tmp$col))
         colnames(tmp_all) <- c(zite,"col")
         tmp <- data.frame(table(rbind(tmp[,c(zite,"col")],tmp_all)))
      }else{
         tmp[[zite]] <- paste0("All (n=",nrow(tmp),")")
         tmp <- data.frame(table(tmp[,c(zite,"col")]))

      }

      for(i in unique(tmp[[zite]])){
         tmp$sum[tmp[[zite]] == i] <- sum(tmp$Freq[tmp[[zite]] == i])
      }
      tmp$prop <- tmp$Freq/tmp$sum
      tmp$perc <- paste0(round(tmp$prop*100),"%")

      tmp[[zite]] <- as.factor(tmp[[zite]])
      tmp[[zite]] <- factor(tmp[[zite]], levels=unique(c(tmp[grepl("^All",tmp[[zite]]),zite],
                                                  tmp[!grepl("^All",tmp[[zite]]),zite])))

      tmp$n_Freq <- tmp$Freq
      tmp$perc[grepl("NaN%|^0%$",tmp$perc)] <- ""

      # second axis
      if(setting == "full"){
         allvssite <- max(tmp[!grepl("^All",tmp[[zite]]),"sum"])/
            max(tmp[grepl("^All",tmp[[zite]]),"sum"])
         tmp[!grepl("^All",tmp[[zite]]),"n_Freq"] <- tmp[!grepl("^All",tmp[[zite]]),"Freq"]/allvssite
      }

      g1 <- ggplot() +
      geom_bar(aes(x=tmp[[zite]], y=tmp$n_Freq, fill=tmp$col), stat = "identity",
               position = position_stack(reverse = TRUE)) +
      ggfittext::geom_bar_text(aes(x=tmp[[zite]], y=tmp$n_Freq,label= tmp$perc),
                    position="stack",
                     color = "black",
                    vjust = 1.2,
                    size = 3 * ggplot2::.pt,
                    min.size = 3 * ggplot2::.pt,
                    padding.x = grid::unit(0, "pt"),
                    padding.y = grid::unit(0, "pt"),
                    outside = TRUE) +
         scale_fill_brewer(palette="Paired",na.translate = F) +
         theme_classic() +
         labs(y=paste(meta_title,"\n(n)")) +
         theme(
               axis.text.x = element_text(angle=90, vjust = 0.25,hjust=0),
               legend.title = element_blank(),
               legend.position = "top",
               plot.margin = margin())

      if(setting=="full"){
         g1 <- g1 +  scale_y_continuous(expand=c(0,0),
            sec.axis = sec_axis(~ . * allvssite)) +
         geom_vline(aes(xintercept=1.5), linetype="dashed") +
            theme(axis.title.x = element_blank())
      }else{
         g1 <- g1 + coord_flip() +
            scale_y_continuous(expand=c(0,0)) +
            theme(axis.title.y = element_blank())
      }

   }
   return(g1)
}


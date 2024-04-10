# ==== DOCUMENTATION ====

#' Trial status for central data monitoring (cdm.status)
#'
#' `cdm.status()` is a function to provide an overview of the trial status
#'
#' @name cdm.status
#'
#' @usage cdm.status(d, sample.size, planned.years, rolling.average)
#'
#' @param d list of dates for each participant included.
#' @param sample.size the planned sample size
#' @param planned.years the planned duration of the trial
#' @param rolling.average the number of participant to be included to calculate the predicted inclusion rate
#'
#' @return returns a list where `$fig` is the figure; `$txt` is as summary
#' of the findings; and `$df`is the dataframe generated in the function.
#'
#' @examples
#' \dontrun{
#'    tmp <- cdm.status(d,sample.size=1808)
#' }
#'
#' @importFrom ggplot2 scale_x_date coord_cartesian
#'
#' @export
#
# ==== FUNCTION ====

cdm.status <- function(d, sample.size, planned.years=2, rolling.average=10){
   out <- NULL

   tmp <- as.Date(d[order(d)])
   tmp <- aggregate(tmp,by=list(tmp),length)
   tmp <- rbind(tmp,data.frame(`Group.1`=Sys.Date(),`x`=0))
   colnames(tmp) <- c("Date","n")
   tmp <- tmp[order(tmp$Date),]
   tmp <- rbind(c(NA,0),tmp)
   tmp$Date[1] <- tmp$Date[2]-1
   tmp$cum.n <- cumsum(tmp$n)
   tmp$diffdates <- as.numeric(difftime(tmp$Date,tmp$Date[1],units="days"))

   #Y-axis
   breaks.y <- c(0,round((sample.size/4*c(1:3)/10))*10,sample.size)

   # Planned line
   tmp$planned.n <- tmp$diffdates*sample.size/(planned.years*365.25)

   # Rolling average line
   if(!is.na(rolling.average) & max(tmp$cum.n,na.rm=T) > rolling.average){
      first <- tmp$diffdates[tmp$cum.n >= max(tmp$cum.n)-rolling.average][1]
      last <- tmp$diffdates[match(max(tmp$cum.n), tmp$cum.n)]
      avg.days <- (last-first)/rolling.average
      days.to.end <- (sample.size-max(tmp$cum.n))*avg.days
      rolav <- data.frame(`Date`=c(tmp$Date,max(tmp$Date)+days.to.end),
                          `pred.n`=c(tmp$cum.n,sample.size))
      tmp <- merge(tmp,rolav,all=T)

      lbls <- c("Included participants",
                paste0("Planned inclusion of ", planned.years," years"),
                paste0("Predicted inclusion rate"))
      out$fig <- ggplot(tmp,aes(x=Date)) +
         geom_line(aes(y=pred.n,color="pred.n"), linetype="dotted",
                   alpha=0.8)
   }else{
      lbls <- c("Included participants",
                paste0("Planned inclusion of ", planned.years," years"))
      out$fig <- ggplot(tmp,aes(x=Date))
   }

   out$fig <- out$fig +
      geom_line(aes(y=cum.n,color="actual.n")) +
      geom_line(aes(y=planned.n,color="planned.n"), alpha=0.8) +
      geom_hline(aes(yintercept=sample.size), linetype="dashed") +
      scale_y_continuous(expand=c(0,NA), breaks=breaks.y, name="Participants") +
      scale_color_manual(values=c("black","#51c6e9","#E97451"),
                         labels=lbls) +
      scale_x_date(expand=c(0,0), name="Date") +
      coord_cartesian(ylim=c(0,sample.size)) +
      theme_classic() +
      theme(legend.position = "bottom", legend.title = element_blank(),
            plot.margin = margin(t=10),
            plot.caption.position = "plot",
            plot.caption = element_text(hjust = 0))

   out$df <- tmp

   out$txt <- paste0("The trial has been running for ",
                     round(max(tmp$diffdates[!is.na(tmp$cum.n)])/365.25,1), " years (",
                     min(tmp$Date[tmp$cum.n != 0],na.rm=T), "). Until now ",max(tmp$cum.n,na.rm=T),
                     " participants has been included."  )

   if(!is.na(rolling.average) & max(tmp$cum.n,na.rm=T) > rolling.average){
      out$txt <- paste(out$txt,paste0("Based on the last ", rolling.average, " participants there is an average of ", avg.days, " days between each inclusion. Thus, inclusion must be continued for another ", round(days.to.end/365.25,1), " years (", max(tmp$Date,na.rm=T),") to reach the required sample size (n=", sample.size,").\n\n"))
   }

   return(out)
}

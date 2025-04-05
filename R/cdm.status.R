# ==== DOCUMENTATION ====

#' Trial status for central data monitoring (cdm.status)
#'
#' `cdm.status()` is a function to provide an overview of the trial status
#'
#' @name cdm.status
#'
#' @usage cdm.status(d, sample.size, planned.years, rolling.average, caption)
#'
#' @param d list of dates for each participant included.
#' @param sample.size the planned sample size
#' @param planned.years the planned duration of the trial
#' @param rolling.average the number of participant to be included to calculate the predicted inclusion rate
#' @param caption boolean to add or remove a small description of the figure.
#'
#' @return returns a list where `$fig` is the figure; `$txt` is as summary
#' of the findings; and `$df`is the dataframe generated in the function.
#'
#' @examples
#' \dontrun{
#'    tmp <- cdm.status(d,sample.size=1808)
#' }
#'
#' @importFrom ggplot2 scale_x_date coord_cartesian geom_segment
#'
#' @export
#
# ==== FUNCTION ====

# d <- c("2024-06-05 10:42:34","2024-07-02 11:17:51","2024-07-16 10:55:19","2024-07-31 10:40:33","2024-07-10 10:03:20","2024-07-24 11:47:25","2024-08-06 10:54:57","2024-08-13 10:25:34","2024-09-05 11:14:55","2024-10-03 10:47:49","2024-09-27 10:59:20","2024-08-22 10:26:25","2024-09-10 11:03:24","2024-10-02 10:08:11","2024-07-03 11:09:52","2024-07-01 11:12:23","2024-06-19 11:50:58","2024-06-18 13:01:26","2024-06-25 10:21:26","2024-07-09 11:09:54","2024-07-15 11:54:33","2024-07-30 10:31:45","2024-08-07 11:44:39","2024-09-12 12:24:47","2024-09-24 11:35:03","2024-09-30 10:39:47","2024-10-16 11:20:12","2024-10-21 10:56:57","2024-10-22 10:33:55","2024-10-14 12:50:24","2024-10-28 11:39:13","2024-10-18 11:18:56","2024-06-07 10:30:45","2024-07-12 11:56:39","2024-07-29 11:31:46","2024-07-18 10:08:49","2024-08-08 10:26:23","2024-08-15 11:29:19","2024-08-28 11:23:42","2024-08-22 11:54:10","2024-09-03 11:52:56","2024-09-18 11:44:00","2024-09-13 12:34:04","2024-09-26 11:45:14","2024-10-07 11:00:22","2024-10-15 10:59:50","2024-10-30 12:55:42","2024-10-10 11:46:58","2024-10-17 11:52:46")
# sample.size <- 400
# planned.years <- 1.5
# rolling.average <- 10

cdm.status <- function(d, sample.size, planned.years=2, rolling.average=10, caption=T){
   out <- NULL

   # Create data.frame
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
   plx <- c(`x`=tmp$Date[1],`xend`=tmp$Date[1]+planned.years*365.25)
   ply <- c(`y`=0,`yend`=sample.size)

   # Rolling average line
   if(!is.na(rolling.average) & max(tmp$cum.n,na.rm=T) > rolling.average){
      first <- tmp$diffdates[tmp$cum.n >= max(tmp$cum.n)-rolling.average][1]
      last <- tmp$diffdates[match(max(tmp$cum.n), tmp$cum.n)]
      avg.days <- (last-first)/rolling.average
      days.to.end <- (sample.size-max(tmp$cum.n))*avg.days
      rlx <- c(`x`=max(tmp$Date),`xend`=max(tmp$Date)+days.to.end)
      rly <- c(`y`=max(tmp$cum.n),`yend`=sample.size)

      lbls <- c("Included participants",
                paste0("Planned inclusion of ", planned.years," years"),
                paste0("Predicted inclusion rate"))
      out$fig <- ggplot(tmp,aes(x=Date)) +
         geom_segment(aes(x=rlx[1],xend=rlx[2],y=rly[1],yend=rly[2],
                          color="pred.n"), linetype="dotted",alpha=0.8)
   }else{
      lbls <- c("Included participants",
                paste0("Planned inclusion of ", planned.years," years"))
      out$fig <- ggplot(tmp,aes(x=Date))
   }

   out$fig <- out$fig +
      geom_segment(aes(x=plx[1],xend=plx[2],y=ply[1],yend=ply[2],color="planned.n"), alpha=0.8) +
      geom_hline(aes(yintercept=sample.size), linetype="dashed") +
      geom_line(aes(y=cum.n,color="actual.n")) +
      scale_y_continuous(expand=c(0,NA), breaks=breaks.y, name="Participants") +
      scale_color_manual(values=c("black","#51c6e9","#E97451"),
                         labels=lbls) +
      scale_x_date(expand=c(0,0), name="Date") +
      coord_cartesian(ylim=c(0,sample.size)) +
      theme_classic() +
      theme(legend.position = "bottom", legend.title = element_blank(),
            plot.margin = margin(t=10),
            plot.caption = element_text(face="italic", hjust=0),
            plot.caption.position = "plot")

   if(caption){
      out$fig <- out$fig +
         labs(caption="Overview of the enrollment rate.")
   }

   out$df <- tmp

   out$txt <- paste0("The trial has been running for ",
                     round(max(tmp$diffdates[!is.na(tmp$cum.n)])/365.25,1), " years (",
                     min(tmp$Date[tmp$cum.n != 0],na.rm=T), "). Until now ",max(tmp$cum.n,na.rm=T),
                     " participants have been included."  )

   if(!is.na(rolling.average) & max(tmp$cum.n,na.rm=T) > rolling.average){
      out$txt <- paste(out$txt,paste0("Based on the last ", rolling.average, " participants there is an average of ", avg.days, " days between each inclusion. Thus, inclusion must be continued for another approx. ", round(days.to.end/365.25,1), " years (", rlx[2],") to reach the required sample size (n=", sample.size,").\n\n"))
   }

   return(out)
}

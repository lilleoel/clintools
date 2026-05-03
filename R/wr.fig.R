# ==== DOCUMENTATION ====
#' Win Ratio Plot
#'
#' Computes and visualises a win ratio analysis for a hierarchical composite
#' endpoint using the \pkg{WinRatio} package. The function compares two
#' treatment groups across multiple clinical components in a prioritised order
#' and returns a \pkg{ggplot2} horizontal stacked bar chart showing wins, ties,
#' and losses per component alongside the overall win ratio.
#'
#' @param df A \code{data.frame} containing patient-level data.
#' @param id A \code{character} string giving the name of the column in
#'   \code{df} that contains the unique patient identifier.
#' @param group A \code{character} string giving the name of the column in
#'   \code{df} that indicates treatment group. The column must contain exactly
#'   two unique values. May be a \code{factor}
#'   or \code{character}. The first group (first level for factors, first unique
#'   value otherwise) is used as the reference.
#' @param components A \code{character} vector of column names in \code{df}
#'   representing the hierarchical component endpoints. Components are ranked in
#'   the order supplied: the first element carries the highest priority. All
#'   components are expected to be binary (0/1 or a two-level factor/character).
#'
#' @details
#' The function performs the following steps:
#' \enumerate{
#'   \item Removes rows with \code{group == "EHR-review"} from the dataset.
#'   \item Constructs a component hierarchy for use in
#'         \code{\link[WinRatio]{winratio}}, where all components are of type
#'         \code{"c"} (categorical) with direction \code{">"}.
#'   \item Runs \code{WinRatio::winratio()} and collects component-level
#'         results.
#'   \item Computes cumulative wins/losses, ties, and win difference per
#'         component.
#'   \item Returns a \pkg{ggplot2} plot with a horizontal stacked bar
#'         visualisation annotated with the overall win ratio, confidence
#'         interval, p-value, and win difference as a percentage.
#' }
#'
#' The overall win difference is defined as:
#' \deqn{\frac{\text{total wins} - \text{total losses}}
#'            {\text{total wins} + \text{total losses} + \text{total ties}}
#'       \times 100\%}
#'
#' @return A \code{ggplot} object. The plot contains:
#' \itemize{
#'   \item A horizontal stacked bar per component showing the proportion of
#'         wins (green), ties (blue), and losses (red).
#'   \item Cumulative wins and losses displayed as a transparent layer.
#'   \item Win difference (wins minus losses as a percentage) shown on the
#'         right-hand side.
#'   \item An annotation with the overall win ratio, 95\% confidence interval,
#'         and p-value.
#'   \item A bottom row displaying the total underlined win difference.
#' }
#'
#' @seealso
#' \code{\link[WinRatio]{winratio}} for the underlying statistical method,
#' \code{\link[ggplot2]{ggplot}} for customising the returned plot object.
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(WinRatio)
#'
#' # Example with synthetic data
#' set.seed(42)
#' n <- 200
#' df <- data.frame(
#'   id      = 1:n,
#'   group   = sample(c("Treatment", "Control"), n, replace = TRUE),
#'   death   = sample(c("Yes", "No"), n, replace = TRUE),
#'   hosp    = sample(c("Yes", "No"), n, replace = TRUE),
#'   symptom = sample(c("Yes", "No"), n, replace = TRUE)
#' )
#'
#' p <- wr.fig
#'   df         = df,
#'   id         = "id",
#'   group      = "group",
#'   components = c("death", "hosp", "symptom")
#' )
#'
#' print(p)
#' }
#'
#' @importFrom WinRatio winratio
#' @importFrom tidyr pivot_longer
#' @importFrom scales percent
#' @importFrom ggplot2 ggplot aes geom_bar annotate geom_text scale_fill_manual
#'   scale_color_manual scale_x_continuous theme_classic coord_cartesian theme
#'   element_blank element_text margin sec_axis labs
#' @importFrom ggtext element_markdown
#'
#' @export
# ==== FUNCTION ====

wr.fig <- function(df, id, group, components, caption = NULL){
   dwr <- df
   # Definer hierarki

   if(is.null(caption)){
      if(class(dwr[[group]]) == "factor"){
         groups <- levels(droplevels(dwr[[group]]))
      }else{
         groups <- unique(dwr[[group]])
      }
      grp_title <- paste(groups[1], "vs.", groups[2])
   }else{
      grp_title <- caption
   }


   dwr$time <- 1
   dwr[,components] <- lapply(dwr[,components], FUN=function(x)
      as.numeric(as.factor(x))-1)

   make_hierarchy <- function(outcome_names, type="c", direction=">") {
      outc_names <- paste0("outc", seq_along(outcome_names))
      hierarchy <- setNames(
         lapply(outcome_names, function(x) c(x, type, direction)),
         outc_names
      )
      return(hierarchy)
   }

   hierarchy <- make_hierarchy(components)

   m1 <- WinRatio::winratio(id=id,trt=group,outcomes = hierarchy,
                            fu="time",data=dwr)

   mm <- data.frame(`no`=1:length(components),
                    `components`=components,
                    `wins`=m1$wins,
                    `loss`=m1$loss,
                    `total`=(m1$total.loss+m1$total.ties+m1$total.wins))
   mm$wins_cumm <- cumsum(mm$wins)-mm$wins
   mm$loss_cumm <- cumsum(mm$loss)-mm$loss
   mm$ties <- mm$total-cumsum(mm$wins)-cumsum(mm$loss)
   mm$windiff <- mm$wins-mm$loss

   mml <- tidyr::pivot_longer(
      mm,
      cols = c(wins, loss, wins_cumm, loss_cumm,ties,windiff),
      names_to = "type",
      values_to = "value"
   )
   mml$prop <- mml$value/mml$total
   mml$prop_txt <- scales::percent(mml$prop, accuracy = 0.1)
   mml$prop_txt[mml$type %in% c("wins_cumm","loss_cumm")] <- ""
   mml$prop[mml$type == "windiff"] <- NA

   mml$components <- as.factor(mml$components)
   mml$components <- factor(mml$components,levels=c("\n\nWin ratio",rev(components)))

   mml$type <- as.factor(mml$type)
   mml$type <- factor(mml$type, levels=c("wins_cumm","wins","ties",
                                         "loss","loss_cumm","windiff"))

   mml$x_txt <- NA
   mml$x_txt[mml$type == "wins"] <- 1.25
   mml$x_txt[mml$type == "loss"] <- -0.25
   mml$x_txt[mml$type == "ties"] <- 0.5
   mml$x_txt[mml$type == "windiff"] <- 1.75
   tst <- mml$type == "windiff"& mml$no != max(mml$no)
   mml$prop_txt[tst] <- paste0("\n",mml$prop_txt[tst],"\n+")
   tst <- mml$type == "windiff"& mml$no == max(mml$no)
   mml$prop_txt[tst] <- paste0("\n",mml$prop_txt[tst],"\n=")

   tmp <- data.frame(max(mml$no)+1,"\n\nWin ratio",NA,"windiff",NA,NA,"",1.75)
   colnames(tmp) <- colnames(mml)

   mml <- rbind(mml,tmp)

   labels_colored <- c(
      "-0.25" = "<span style='color:darkred'><b>Loss</b></span>",
      "0.5"   = "<span style='color:darkblue'><b>Ties</b></span>",
      "1.25"  = "<span style='color:darkgreen'><b>Wins</b></span>",
      "1.75"  = "<span style='color:black'><b>Win difference</b></span>"
   )

   # Win ratio results

   results <- paste0('frac(plain("',
                     round(sum(mm$wins)/unique(mm$total)*100,1),'%"), plain("',
                     round(sum(mm$loss)/unique(mm$total)*100,1),'%")) == plain("',
                     round(m1$wr,2),'") ~ plain("(95% CI: ',
                     round(m1$wr.lower,2),' - ', round(m1$wr.upper,2),')") ~~~~~~ italic(P) == plain("',
                     round(m1$p.value,4),'")')
   # results <- gsub("·","\u00B7",results)

   # Win difference Total
   win_diff_pct_total <- 100 * (m1$total.wins - m1$total.loss) / (m1$total.wins + m1$total.loss + m1$total.ties)
   windiff_pm <- paste0('bar(underline(underline("',round(win_diff_pct_total,1),'% ")))')

   g1 <- ggplot(mml, aes(y = components, x = prop, fill = type, color=type)) +
      geom_bar(stat = "identity", position = "stack") +
      annotate("text", x = -0.4, y = 0.60, label = results, hjust = 0, parse=T, size=3) + annotate("text", x = 1.75, y = 0.60, label = windiff_pm, hjust = 0.5, parse=T, size=3) +
      geom_text(
         aes( x = x_txt,
              label = prop_txt
         ), size = 3,
         color = "black"
      ) +
      scale_fill_manual(
         values=c(`wins`="#20854e",`wins_cumm`="transparent",
                  `loss`="#bc3c29", `loss_cumm`="transparent",
                  `ties`="#99c6e1")) +
      scale_color_manual(
         values=c(`wins`="black",`wins_cumm`="transparent",
                  `loss`="black", `loss_cumm`="transparent",
                  `ties`="black")) +
      scale_x_continuous(
         sec.axis = sec_axis(~ .,
                             breaks = c(-.25, 0.5, 1.25,1.75), labels = function(x) c(
                                "<span style='color:darkred'><b>Loss</b></span>",
                                "<span style='color:darkblue'><b>Ties</b></span>",
                                "<span style='color:darkgreen'><b>Wins</b></span>",
                                "<span style='color:black'><b>Win difference</b></span>"
                             ))) +
      theme_classic() +
      coord_cartesian(ylim=c(0,NA)) +
      theme(legend.position = "none",
            axis.text.x.top = ggtext::element_markdown(),
            axis.text.x.bottom = element_blank(),
            axis.text.y = element_text(face="bold"),
            plot.subtitle = ggtext::element_markdown(hjust=0, size = 11),
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.title = element_blank(),
            plot.title.position = "plot",
            plot.margin = margin(r=25))

   if(nchar(grp_title) > 0){
     g1 <- g1 + labs(subtitle = paste0("**", grp_title, "**"))
   }

   return(g1)
}

# ==== DOCUMENTATION ====

#' Calculation of MetS-Z score (metsz)
#'
#' `metsz()` is a small function which calculate the MetS-Z score from danish SI values.
#'
#' @name metsz
#'
#' @usage metsz(d, id, ethnicity, sex, wc, hdl, sbp, tri, glu)
#'
#' @param d the dataframe with the data
#' @param id colname of the id column
#' @param ethnicity colname of the ethnicity column
#' @param sex colname of the sex column (must be labelled Male and Female)
#' @param wc colname of the waist circumference
#' @param hdl colname of HDL
#' @param sbp colname of systolic blood pressure
#' @param tri colname of Triglyceride
#' @param glu colname of Glucose
#'
#' @return Returns a dataframe with ssid and MetS-Z score
#'
#' @examples
#' \dontrun{
#' mm <- metsz(df,id="ssid",ethnicity = "SCRE.SC05_ethnicity",
#' sex = "Sex", wc = paste0(x,".wc"),
#' hdl = paste0(x,".BR15_cholesterol_hdl_p"),
#' sbp = paste0(x,".sbp"), tri = paste0(x,".BR17_triglycerid_p"),
#' glu = paste0(x,".BR01_glucose_p"))
#' }
#' @export
#
# ==== FUNCTION ====

metsz <- function(d, id, ethnicity, sex, wc, hdl, sbp, tri, glu){
   # d <- df
   # id <- "ssid"
   # ethnicity <- "SCRE.SC05_ethnicity"
   # sex <- "Sex"
   # wc <- paste0(x,".wc")
   # hdl <- paste0(x,".BR15_cholesterol_hdl_p")
   # sbp <- paste0(x,".sbp")
   # tri <- paste0(x,".BR17_triglycerid_p")
   # glu <- paste0(x,".BR01_glucose_p")

   # START
   d[[hdl]] <- as.numeric(d[[hdl]])*38.67
   d[[tri]] <- as.numeric(d[[tri]])*88.57
   d[[glu]] <- as.numeric(d[[glu]])*18.0156

   # Identify coefficients
   d[!is.na(d[[ethnicity]]) & d[[ethnicity]] == 0 &
        !is.na(d[[sex]]) & d[[sex]] == "Male","coefs"] <- "coefs1"
   coefs1 <- c(`all`=-5.4559,`wc`=0.0125,
               `hdl`=-0.0251,`sbp`=0.0047,`tri`=0.8244,`glu`=0.0106)
   tst <- !is.na(d$coefs) & d$coefs == "coefs1"
   d[tst,"MetSZ"] <- coefs1['all']+
      coefs1['wc']*d[tst,wc]+coefs1['hdl']*d[tst,hdl]+
      coefs1['sbp']*d[tst,sbp]+coefs1['tri']*log(d[tst,tri])+
      coefs1['glu']*d[tst,glu]

   d[!is.na(d[[ethnicity]]) & d[[ethnicity]] == 0 &
        !is.na(d[[sex]]) & d[[sex]] == "Female","coefs"] <- "coefs2"
   coefs2 <- c(`all`=-7.2591,`wc`=0.0254,
               `hdl`=-0.0120,`sbp`=0.0075,`tri`=0.5800,`glu`=0.0203)
   tst <- !is.na(d$coefs) & d$coefs == "coefs2"
   d[tst,"MetSZ"] <- coefs2['all']+
      coefs2['wc']*d[tst,wc]+coefs2['hdl']*d[tst,hdl]+
      coefs2['sbp']*d[tst,sbp]+coefs2['tri']*log(d[tst,tri])+
      coefs2['glu']*d[tst,glu]

   d[!is.na(d[[ethnicity]]) & d[[ethnicity]] == 1 &
        !is.na(d[[sex]]) & d[[sex]] == "Male","coefs"] <- "coefs3"
   coefs3 <- c(`all`=-6.3767,`wc`=0.0232,
               `hdl`=-0.0175,`sbp`=0.0040,`tri`=0.5400,`glu`=0.0203)
   tst <- !is.na(d$coefs) & d$coefs == "coefs3"
   d[tst,"MetSZ"] <- coefs3['all']+
      coefs3['wc']*d[tst,wc]+coefs3['hdl']*d[tst,hdl]+
      coefs3['sbp']*d[tst,sbp]+coefs3['tri']*log(d[tst,tri])+
      coefs3['glu']*d[tst,glu]

   d[!is.na(d[[ethnicity]]) & d[[ethnicity]] == 1 &
        !is.na(d[[sex]]) & d[[sex]] == "Female","coefs"] <- "coefs4"
   coefs4 <- c(`all`=-7.1913,`wc`=0.0304,
               `hdl`=-0.0157,`sbp`=0.0084,`tri`=0.8872,`glu`=0.0206)
   tst <- !is.na(d$coefs) & d$coefs == "coefs4"
   d[tst,"MetSZ"] <- coefs4['all']+
      coefs4['wc']*d[tst,wc]+coefs4['hdl']*d[tst,hdl]+
      coefs4['sbp']*d[tst,sbp]+coefs4['tri']*log(d[tst,tri])+
      coefs4['glu']*d[tst,glu]

      return(d[,c(id,"MetSZ")])

}

# ==== DOCUMENTATION ====

#' Calculate scores from questionaire (questionaire)
#'
#' `questionaire()` is a function which calculates scores from a questionaire.
#'
#' @name questionaire
#'
#' @usage questionaire(df,id,questions,scale,setting,prefix)
#'
#' @param df dataframe. (`df`)
#' @param id Column name of participant id (`string`)
#' @param questions Column names of ordered list of questions (`list`)
#' @param scale name of the questionaire (`string`)
#' @param setting questionaire specific settings (`string`)
#' @param prefix prefix of column names of questionaire scores (`string`)
#'
#' @return Returns summarised information in dataframe.
#'
#' @examples
#' \dontrun{
#'
#'    df <- df_b[,grepl("ssid|KIDSCREEN52_D_",colnames(df_b))][,c(1:53)]
#'    k52 <- questionaire(df,id = "ssid",
#'    questions=colnames(df)[grepl("KIDSCREEN",colnames(df))],
#'    scale="Kidscreen-52",
#'    setting="proxy")#'
#' }
#'
#' @importFrom dplyr recode
#' @export
#
# ==== FUNCTION ====

#Function - Questionaire
questionaire <- function(df,id,questions,scale,setting=NA,prefix=""){
   d <- df[,c(questions,id)]
   o <- NULL
   if(scale == "Kidscreen-52"){
      #Reverse scoring
      d[,questions[c(1,12:18,21:23,50:52)]] <- lapply(d[,questions[c(1,12:18,21:23,50:52)]],
         FUN=function(x){dplyr::recode(x, `1`="5",`2`="4",`3`="2",`4`="3",`5`="1")})
      d[,questions] <- lapply(d[,questions],as.numeric)

      if(setting == "proxy"){
         o$k52phy <- dplyr::recode(rowSums(d[,questions[1:5]]),
            `5`=9.35,`6`=18.38,`7`=23.14,`8`=26.30,`9`=28.78,
            `10`=30.92,`11`=32.88,`12`=34.77,`13`=36.70,`14`=38.78,
            `15`=41.08,`16`=43.66,`17`=46.50,`18`=49.54,`19`=52.68,
            `20`=55.89,`21`=59.38,`22`=63.68,`23`=71.23)
         o$k52pwb <- dplyr::recode(rowSums(d[,questions[6:11]]),
            `6`=9.42,`7`=14.75,`8`=17.75,`9`=20.01,`10`=21.85,
            `11`=23.44,`12`=24.88,`13`=26.28,`14`=27.69,`15`=29.21,
            `16`=30.87,`17`=32.72,`18`=34.75,`19`=36.88,`20`=39.05,
            `21`=41.22,`22`=43.47,`23`=45.95,`24`=48.87,`25`=52.12,
            `26`=55.25,`27`=58.18,`28`=61.09,`29`=64.41,`30`=69.88)
         o$k52emo <- dplyr::recode(rowSums(d[,questions[12:18]]),
            `7`=-2.01,`8`=5.60,`9`=9.63,`10`=12.57,`11`=14.98,
            `12`=17.07,`13`=18.97,`14`=20.72,`15`=22.36,`16`=23.93,
            `17`=25.45,`18`=26.95,`19`=28.43,`20`=29.91,`21`=31.42,
            `22`=32.96,`23`=34.56,`24`=36.22,`25`=37.97,`26`=39.81,
            `27`=41.77,`28`=43.87,`29`=46.12,`30`=48.57,`31`=51.28,
            `32`=54.36,`33`=58.00,`34`=62.68,`35`=70.82)
         o$k52sel <- dplyr::recode(rowSums(d[,questions[19:23]]),
            `5`=6.93,`6`=16.33,`7`=21.18,`8`=24.52,`9`=27.09,
            `10`=29.21,`11`=31.06,`12`=32.73,`13`=34.29,`14`=35.81,
            `15`=37.33,`16`=38.88,`17`=40.51,`18`=42.28,`19`=44.25,
            `20`=46.48,`21`=49.11,`22`=52.27,`23`=56.18,`24`=61.43,
            `25`=70.98)
         o$k52aut <- dplyr::recode(rowSums(d[,questions[24:28]]),
            `5`=4.74,`6`=11.69,`7`=15.68,`8`=18.99,`9`=22.19,
            `10`=25.45,`11`=28.55,`12`=31.25,`13`=33.58,`14`=35.66,
            `15`=37.60,`16`=39.51,`17`=41.43,`18`=43.48,`19`=45.72,
            `20`=48.22,`21`=50.95,`22`=53.87,`23`=57.07,`24`=61.01,
            `25`=67.95)
         o$k52par <- dplyr::recode(rowSums(d[,questions[29:34]]),
            `6`=5.70,`7`=12.11,`8`=15.55,`9`=18.10,`10`=20.20,
            `11`=22.08,`12`=23.83,`13`=25.53,`14`=27.20,`15`=28.89,
            `16`=30.61,`17`=32.39,`18`=34.25,`19`=36.17,`20`=38.16,
            `21`=40.20,`22`=42.33,`23`=44.54,`24`=46.87,`25`=49.38,
            `26`=52.12,`27`=55.13,`28`=58.45,`29`=62.45,`30`=69.22)
         o$k52fin <- dplyr::recode(rowSums(d[,questions[35:37]]),
             `3`=23.96,`4`=29.10,`5`=32.35,`6`=35.23,`7`=37.95,
             `8`=40.59,`9`=43.31,`10`=46.06,`11`=48.85,`12`=51.90,
             `13`=55.39,`14`=59.33,`15`=65.02)
         o$k52soc <- dplyr::recode(rowSums(d[,questions[38:43]]),
             `6`=8.28,`7`=14.69,`8`=18.24,`9`=21.03,`10`=23.53,
             `11`=25.93,`12`=28.27,`13`=30.54,`14`=32.68,`15`=34.71,
             `16`=36.67,`17`=38.60,`18`=40.52,`19`=42.46,`20`=44.42,
             `21`=46.43,`22`=48.52,`23`=50.73,`24`=53.05,`25`=55.44,
             `26`=57.87,`27`=60.37,`28`=63.16,`29`=66.69,`30`=73.08)
         o$k52sch <- dplyr::recode(rowSums(d[,questions[44:49]]),
             `6`=9.55,`7`=16.89,`8`=21.01,`9`=23.85,`10`=26.01,
             `11`=27.81,`12`=29.43,`13`=30.95,`14`=32.45,`15`=34.00,
             `16`=35.64,`17`=37.40,`18`=39.29,`19`=41.28,`20`=43.31,
             `21`=45.39,`22`=47.52,`23`=49.75,`24`=52.09,`25`=54.52,
             `26`=57.01,`27`=59.60,`28`=62.47,`29`=66.08,`30`=72.50)
         o$k52bul <- dplyr::recode(rowSums(d[,questions[50:52]]),
              `3`=3.22,`4`=10.47,`5`=14.74,`6`=18.25,`7`=21.46,
              `8`=24.53,`9`=27.63,`10`=30.90,`11`=34.63,`12`=39.34,
              `13`=44.83,`14`=50.55,`15`=58.83)
      }else if(setting == "self"){
         o$k52phy <- dplyr::recode(rowSums(d[,questions[1:5]]),
            `5`=12.13,`6`=20.70,`7`=25.07,`8`=28.13,`9`=30.57,
            `10`=32.69,`11`=34.65,`12`=36.55,`13`=38.47,`14`=40.45,
            `15`=42.53,`16`=44.73,`17`=47.08,`18`=49.63,`19`=52.43,
            `20`=55.60,`21`=59.36,`22`=64.30,`23`=73.20)
         o$k52pwb <- dplyr::recode(rowSums(d[,questions[6:11]]),
            `6`=9.86,`7`=16.65,`8`=20.36,`9`=23.07,`10`=25.23,
            `11`=27.04,`12`=28.63,`13`=30.08,`14`=31.45,`15`=32.80,
            `16`=34.13,`17`=35.50,`18`=36.91,`19`=38.37,`20`=39.91,
            `21`=41.53,`22`=43.25,`23`=45.10,`24`=47.12,`25`=49.34,
            `26`=51.78,`27`=54.49,`28`=57.60,`29`=61.55,`30`=68.49)
         o$k52emo <- dplyr::recode(rowSums(d[,questions[12:18]]),
            `7`=7.92,`8`=15.94,`9`=19.81,`10`=22.46,`11`=24.52,
            `12`=26.23,`13`=27.71,`14`=29.04,`15`=30.27,`16`=31.42,
            `17`=32.51,`18`=33.58,`19`=34.61,`20`=35.65,`21`=36.70,
            `22`=37.76,`23`=38.86,`24`=40.00,`25`=41.21,`26`=42.50,
            `27`=43.91,`28`=45.44,`29`=47.15,`30`=49.09,`31`=51.34,
            `32`=54.02,`33`=57.40,`34`=62.06,`35`=70.91)
         o$k52sel <- dplyr::recode(rowSums(d[,questions[19:23]]),
            `5`=12.10,`6`=21.36,`7`=25.83,`8`=28.88,`9`=31.24,
            `10`=33.20,`11`=34.89,`12`=36.43,`13`=37.85,`14`=39.21,
            `15`=40.52,`16`=41.83,`17`=43.17,`18`=44.58,`19`=46.09,
            `20`=47.78,`21`=49.76,`22`=52.19,`23`=55.38,`24`=60.11,
            `25`=69.78)
         o$k52aut <- dplyr::recode(rowSums(d[,questions[24:28]]),
            `5`=10.19,`6`=18.58,`7`=23.05,`8`=26.39,`9`=29.16,
            `10`=31.57,`11`=33.70,`12`=35.61,`13`=37.35,`14`=38.98,
            `15`=40.54,`16`=42.06,`17`=43.59,`18`=45.17,`19`=46.85,
            `20`=48.70,`21`=50.77,`22`=53.22,`23`=56.27,`24`=60.52,
            `25`=68.75)
         o$k52par <- dplyr::recode(rowSums(d[,questions[29:34]]),
            `6`=9.93,`7`=17.00,`8`=20.61,`9`=23.19,`10`=25.27,
            `11`=27.06,`12`=28.68,`13`=30.18,`14`=31.61,`15`=32.97,
            `16`=34.32,`17`=35.66,`18`=36.98,`19`=38.33,`20`=39.69,
            `21`=41.10,`22`=42.55,`23`=44.09,`24`=45.72,`25`=47.50,
            `26`=49.50,`27`=51.81,`28`=54.65,`29`=58.53,`30`=65.87)
         o$k52fin <- dplyr::recode(rowSums(d[,questions[35:37]]),
            `3`=23.24,`4`=29.15,`5`=32.48,`6`=35.12,`7`=37.47,
            `8`=39.71,`9`=41.92,`10`=44.18,`11`=46.59,`12`=49.28,
            `13`=52.41,`14`=56.35,`15`=62.86)
         o$k52soc <- dplyr::recode(rowSums(d[,questions[38:43]]),
            `6`=9.40,`7`=17.78,`8`=21.95,`9`=24.88,`10`=27.22,
            `11`=29.19,`12`=30.94,`13`=32.54,`14`=34.03,`15`=35.44,
            `16`=36.81,`17`=38.15,`18`=39.49,`19`=40.83,`20`=42.2,
            `21`=43.6,`22`=45.08,`23`=46.66,`24`=48.35,`25`=50.24,
            `26`=52.39,`27`=54.93,`28`=58.14,`29`=62.66,`30`=71.46)
         o$k52sch <- dplyr::recode(rowSums(d[,questions[44:49]]),
            `6`=14.02,`7`=21.82,`8`=25.68,`9`=28.35,`10`=30.46,
            `11`=32.25,`12`=33.86,`13`=35.35,`14`=36.77,`15`=38.15,
            `16`=39.53,`17`=40.92,`18`=42.35,`19`=43.82,`20`=45.34,
            `21`=46.94,`22`=48.61,`23`=50.37,`24`=52.23,`25`=54.22,
            `26`=56.40,`27`=58.88,`28`=61.87,`29`=65.94,`30`=73.80)
         o$k52bul <- dplyr::recode(rowSums(d[,questions[50:52]]),
            `3`=10.99,`4`=18.72,`5`=22.38,`6`=24.99,`7`=27.15,
            `8`=29.13,`9`=31.08,`10`=33.13,`11`=35.44,`12`=38.29,
            `13`=42.20,`14`=48.07,`15`=58.85)
      }else{
         stop("For Kidscreen-52 'setting' must be either 'proxy' or 'self'")
      }
   }
   o <- cbind(d[[id]],data.frame(o))
   colnames(o)[1] <- id
   return(o)
}

   # OTHER----
   # if(scale == "PedsQL4"){ # https://www.pedsql.org/PedsQL-Scoring.pdf
   #    df[,questions] <- lapply(df[,questions],FUN=function(x){
   #       dplyr::recode(x, `0`="100",`1`="75",`2`="50",`3`="25",`4`="0")
   #    })
   #    df[,questions] <- lapply(df[,questions], FUN=function(x){
   #       as.numeric(as.character(x))
   #    })
   #
   #    pedsql4_total <- rowMeans(df[,c(questions)],na.rm=T)
   #    pedsql4_physical <- rowMeans(df[,c(questions)[1:8]],na.rm=T)
   #    pedsql4_emotional <- rowMeans(df[,c(questions)[9:13]],na.rm=T)
   #    pedsql4_social <- rowMeans(df[,c(questions)[14:18]],na.rm=T)
   #    pedsql4_school <- rowMeans(df[,c(questions)[19:23]],na.rm=T)
   #    pedsql4_psychosocial <- rowMeans(df[,c(questions)[9:23]],na.rm=T)
   #
   #    pedsql4_total[rowMeans(is.na(df[,c(questions)])) > 0.5] <- NA
   #    pedsql4_physical[rowMeans(is.na(df[,c(questions)[1:8]])) > 0.5] <- NA
   #    pedsql4_emotional[rowMeans(is.na(df[,c(questions)[9:13]])) > 0.5] <- NA
   #    pedsql4_social[rowMeans(is.na(df[,c(questions)[14:18]])) > 0.5] <- NA
   #    pedsql4_school[rowMeans(is.na(df[,c(questions)[19:13]])) > 0.5] <- NA
   #    pedsql4_psychosocial[rowMeans(is.na(df[,c(questions)[9:13]])) > 0.5] <- NA
   #
   #    if(only){
   #       df <- cbind(df,pedsql4_total)
   #       colnames(df)[ncol(df)] <-
   #          paste0(prefix,colnames(df)[ncol(df)])
   #    }else{
   #       df <- cbind(df,pedsql4_total,pedsql4_physical,pedsql4_emotional,
   #                   pedsql4_social,pedsql4_school,pedsql4_psychosocial)
   #       colnames(df)[c((ncol(df)-5):ncol(df))] <-
   #          paste0(prefix,colnames(df)[c((ncol(df)-5):ncol(df))])
   #    }
   #
   #
   #
   # }else if(scale == "RCADS"){ # https://www.childfirst.ucla.edu/resources/
   #    df[,questions] <- lapply(df[,questions], FUN=function(x){
   #       as.numeric(as.character(x))
   #    })
   #
   #    c_sad <- c(5,9,17,18,33,45,46)
   #    c_gad <- c(1,13,22,27,35,37)
   #    c_pd <- c(3,14,24,26,28,34,36,39,41)
   #    c_soc <- c(4,7,8,12,20,30,32,38,43)
   #    c_ocd <- c(10,16,23,31,42,44)
   #    c_depression <- c(2,6,11,15,19,21,25,29,40,47)
   #    c_anxiety <- c(1,3,4,5,7,8,9,10,12,13,14,16,17,18,20,22,23,24,26,27,28,30,
   #                   31,32,33,34,35,36,37,38,39,41,42,43,44,45,46)
   #    c_total <- c(1:47)
   #
   #    rcadschild_sad <- rowSums(df[,questions[c_sad]],na.rm=T)/
   #       (7-rowSums(is.na(df[,questions[c_sad]])))*7
   #    rcadschild_gad <- rowSums(df[,questions[c_gad]],na.rm=T)/
   #       (6-rowSums(is.na(df[,questions[c_gad]])))*6
   #    rcadschild_pd <- rowSums(df[,questions[c_pd]],na.rm=T)/
   #       (9-rowSums(is.na(df[,questions[c_pd]])))*9
   #    rcadschild_soc <- rowSums(df[,questions[c_soc]],na.rm=T)/
   #       (9-rowSums(is.na(df[,questions[c_soc]])))*9
   #    rcadschild_ocd <- rowSums(df[,questions[c_ocd]],na.rm=T)/
   #       (6-rowSums(is.na(df[,questions[c_ocd]])))*6
   #    rcadschild_depression <- rowSums(df[,questions[c_depression]],na.rm=T)/
   #       (10-rowSums(is.na(df[,questions[c_depression]])))*10
   #    rcadschild_anxiety <- rowSums(df[,questions[c_anxiety]],na.rm=T)/
   #       (37-rowSums(is.na(df[,questions[c_anxiety]])))*37
   #    rcadschild_total <- rowSums(df[,questions[c_total]],na.rm=T)/
   #       (47-rowSums(is.na(df[,questions[c_total]])))*47
   #
   #    rcadschild_sad[rowSums(is.na(df[,questions[c_sad]])) > 2] <- NA
   #    rcadschild_gad[rowSums(is.na(df[,questions[c_gad]])) > 2] <- NA
   #    rcadschild_pd[rowSums(is.na(df[,questions[c_pd]])) > 2] <- NA
   #    rcadschild_soc[rowSums(is.na(df[,questions[c_soc]])) > 2] <- NA
   #    rcadschild_ocd[rowSums(is.na(df[,questions[c_ocd]])) > 2] <- NA
   #    rcadschild_depression[rowSums(is.na(df[,questions[c_depression]])) > 2] <- NA
   #
   #    rcadschild_anxiety[rowSums(is.na(df[,questions[c_anxiety]])) > 10] <- NA
   #    rcadschild_anxiety[which(rowSums(is.na(cbind(rcadschild_sad,rcadschild_gad,
   #                                                 rcadschild_pd,rcadschild_soc,rcadschild_ocd))) > 0)] <- NA
   #
   #
   #    rcadschild_total[rowSums(is.na(df[,questions[c_total]])) > 12] <- NA
   #    rcadschild_total[which(rowSums(is.na(cbind(rcadschild_sad,rcadschild_gad,
   #                                               rcadschild_pd,rcadschild_soc,rcadschild_ocd,rcadschild_depression))) > 0)] <- NA
   #
   #    if(only){
   #       df <- cbind(df,rcadschild_total)
   #       colnames(df)[ncol(df)] <-
   #          paste0(prefix,colnames(df)[ncol(df)])
   #    }else{
   #       df <- cbind(df,rcadschild_sad,rcadschild_gad,rcadschild_pd,
   #                   rcadschild_soc,rcadschild_ocd,rcadschild_depression)
   #       df <- cbind(df,rcadschild_anxiety,rcadschild_total)
   #
   #       colnames(df)[c((ncol(df)-7):ncol(df))] <-
   #          paste0(prefix,colnames(df)[c((ncol(df)-7):ncol(df))])
   #    }
   #
   # }
   # df <- df[,!colnames(df) %in% c(questions)]
   # return(df)


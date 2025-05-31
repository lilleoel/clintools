# ==== DOCUMENTATION ====

#' Calculate scores from questionaires (questionaire)
#'
#' `questionaire()` is a function which calculates scores from a questionaire.
#'
#' @name questionaire
#'
#' @usage questionaire(df,id,questions,scale,prefix,...)
#'
#' @param df dataframe. (`df`)
#' @param id Column name of participant id (`string`)
#' @param questions Column names of ordered list of questions (`list`)
#' @param scale name of the questionaire (`string`)
#' @param prefix prefix of column names of questionaire scores (`string`)
#' @param ... additional questionaire specific parameters
#'
#' @return Returns summarised information in dataframe.
#'
#' @details
#'
#' ## PARCA-R (Parent Report of Children's Abilities-Revised)
#'
#' This only calculates the scores for the 34 Non-verbal cognitive score. The questionaire needs to have 34 questions, `scale = "PARCA-R"`, and the following additional parameters need to be set:
#' * `birthday` | The name of the column for the birthday; if children are born before 37 weeks of gestatoin it is suggested to have corrected age; and then the column should be date of term.
#' * `date` | The name of the column for the date of when the participants went through the questionaire.
#' * `sex` | The name of the column for the sex of the participants. Male's must be coded as `M` and females as `F`.
#'
#' The calculation summarizes both the raw score and the standard score. As per the PARCA-R manual from 2019 and from the protocol from SafeBoosC-III follow-up study \href{https://pubmed.ncbi.nlm.nih.gov/37805539/}{Rasmussen et al. 2029} moderate-or-severe cognitive impairment is defined as < -2SD. This corresponds to a standard score below 70.
#'
#' ## CBI (Copenhagen Burnout Inventory)
#'
#' Using the 19 questions from the CBI it will both calculate `score` and `group`.
#'
#' Using setting it can either be the English version (default; `score = 'english'`)
#' or the Danish version (`score = 'danish'`). In the English version the Likert-scale
#' is converted to 100, 75, 50, 25, 0. Here, no more than 3 questions may be missing
#' in each subscore \href{https://nfa.dk/da/Vaerktoejer/Sporgeskemaer/Sporgeskema-til-maaling-af-udbraendthed/Copenhagen-Burnout-Inventory-CBI}{English CBI}.
#'
#' The groups are based on the average score:
#' * 0-25: 'no burnout'
#' * 25-50: 'light burnout'
#' * 50-75: 'moderate burnout'
#' * 75-100: 'severe burnout'
#'
#' In the Danish score the questions are summed up from 0 to 4 points. Here, all questions must be answered. \href{https://nfa.dk/da/Vaerktoejer/Sporgeskemaer/Sporgeskema-til-maaling-af-udbraendthed}{Danish CBI}
#'
#' The groups are based on the summed score:
#' * 0-5: 'no burnout' (0-6 for Work-related burnout)
#' * 6-11: 'light burnout' (7-13 for Work-related burnout)
#' * 12-17: 'moderate burnout' (14-20 for Work-related burnout)
#' * 18+: 'severe burnout' (21+ for Work-related burnout)
#'
#' ## KIDSCREEN-52
#' Works, but not documented
#'
#' ## SF-36
#' Works, but not documented. However, used \href{this documentation}{https://www.rand.org/health-care/surveys_tools/mos/36-item-short-form/scoring.html}
#'
#' ## CBCL
#' Not yet documented
#'
#' ## AFEQ
#' Not yet documented
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
#' @importFrom utils read.table
#' @export
#
# ==== FUNCTION ====

# # FOR TESTS

# # FOR TESTS

questionaire <- function(df,id,questions,scale,prefix="",...){
   list2env(list(...), envir = environment())

   if(!exists("setting")) setting <- NA
   o <- NULL

   if(scale == "AFEQ"){
      # ***********************
      # AFEQ - UNVALIDATED ####
      # ***********************
      if(length(questions) != 48) stop("There must be 48 questions to calculate AFEQ")
      d <- df[,c(id,questions)]
      d[,questions] <- lapply(d[,questions],as.numeric)

      # Change reverse questions
      d[d == 0]  <- NA
      recoding_rules <- list(
         list(indices=c(1,6,8,9,11,12,14,15,22,27,30,32,36), mapping=setNames(c(5:1),c(1:5)))
      )
      for (rule in recoding_rules) {
         d[,questions[rule$indices]] <- lapply(d[,questions[rule$indices]],
                                               function(x) dplyr::recode(x, !!!rule$mapping))
      }

      # Calculate domains
      domains <- list(
         afeq_exp = c(1:13),
         afeq_fl = c(14:22),
         afeq_cdus = c(23:36),
         afeq_cs = c(37:48),
         afeq_tot = c(1:48)
      )

      for(i in 1:length(domains)){

         d[[names(domains)[i]]] <- rowSums(d[,questions[domains[[i]]]],na.rm=T)

         # Imputate missing data
         d[[names(domains)[i]]] <- d[[names(domains)[i]]]+
            rowSums(is.na(d[,questions[domains[[i]]]]))*rowMeans(d[,questions],na.rm=T)
         # Missing data handling
         if(names(domains[i]) == "afeq_tot"){ miss_ok <- 9 }else{ miss_ok <- 3 }
         d[[names(domains)[i]]][rowSums(is.na(d[,questions[domains[[i]]]])) > miss_ok] <- NA
      }

      # Create output
      o <- d[,!(colnames(d) %in% questions)]

   }else if(scale == "CBCL"){
      # ***********************
      # CBCL - UNVALIDATED ####
      # ***********************
      if(length(questions) != 99) stop("There must be 99 questions to calculate CBCL")
      d <- df[,c(id,questions)]
      d[,questions] <- lapply(d[,questions],as.numeric)

      # Calculate domains
      domains <- list(
         cbcl_tot   = 1:99,
         cbcl_int   = c(1,2,4,7,10,12,19,21,23,24,33,37,39,43,45,46,47,51,52,
                        62,67,68,70,71,78,79,82,83,86,87,90,92,93,97,98,99),
         cbcl_ext   = c(5,6,8,15,16,18,20,27,29,35,40,42,44,53,56,58,59,66,
                        69,81,85,88,95,96),
         cbcl_aff   = c(13,24,38,43,49,50,71,74,89,90),
         cbcl_anx   = c(10,22,28,32,37,47,48,51,87,99),
         cbcl_asd   = c(3,4,7,21,23,25,63,67,70,76,80,92,98),
         cbcl_adhd  = c(5,6,8,16,36,59),
         cbcl_odd   = c(15,20,44,81,85,88),
         cbcl_sleep = c(22,38,48,64,74,84,94)
      )

      for(i in 1:length(domains)){
         d[[names(domains)[i]]] <- rowSums(d[,questions[domains[[i]]]],na.rm=T) # Calculate means
         # Ensure more than 90% of the questions has been answered
         d[[names(domains)[i]]][rowSums(is.na(d[,questions[domains[[i]]]])) > length(domains[[i]])/10] <- NA
      }

      # Create output
      o <- d[,!(colnames(d) %in% questions)]
   }else if(scale == "MDI"){
      # ***********************
      # MDI - UNVALIDATED ####
      # ***********************
      if(length(questions) != 13) stop("There must be 13 questions to calculate MDI")
      d <- df[,c(id,questions)]
      d[,questions] <- lapply(d[,questions],as.numeric)

      d[,questions[8]] <- apply(df[,questions[8:9]],1,max)
      d[,questions[10]] <- apply(df[,questions[10:11]],1,max)
      d[,questions[12]] <- apply(df[,questions[12:13]],1,max)

      # Calculate domains
      domains <- list(
         mdi_tot   = c(1:8,10,12)
      )

      for(i in 1:length(domains)){
         d[[names(domains)[i]]] <- rowSums(d[,questions[domains[[i]]]],na.rm=T) # Calculate means
         # Ensure more than 90% of the questions has been answered
         d[[names(domains)[i]]][rowSums(is.na(d[,questions[domains[[i]]]])) > length(domains[[i]])/10] <- NA
      }

      # Create output
      o <- d[,!(colnames(d) %in% questions)]

   }else if(scale == "MPCA"){
      # ***********************
      # MPCA - UNVALIDATED ####
      # ***********************
      if(length(questions) != 23) stop("There must be 23 questions to calculate MPCA")
      d <- df[,c(id,questions)]
      d[,questions] <- lapply(d[,questions],as.numeric)

      # Change reverse questions
      recoding_rules <- list(
         list(indices=c(8,9,11,14,16,20,22,23), mapping=setNames(c(5:1),c(1:5)))
      )
      for (rule in recoding_rules) {
         d[,questions[rule$indices]] <- lapply(d[,questions[rule$indices]],
                                               function(x) dplyr::recode(x, !!!rule$mapping))
      }

      # Calculate domains
      d$mpca_tot <- rowSums(d[,questions],na.rm=T)
      d$mpca_mean <- rowMeans(d[,questions],na.rm=T)
      d[rowSums(is.na(d[,questions])) > 0,c("mpca_tot","mpca_mean")] <- NA

      # Create output
      o <- d[,!(colnames(d) %in% questions)]


   }else if(scale == "PedsQL4"){
      # *************************
      # PedQL4 - UNVALIDATED ####
      # *************************
      if(!(length(questions) %in% c(21,23))) stop("There must be either 21 or 23 questions to calculate PedsQL for 4.0 Generic Core ages 2-7")
      d <- df[,c(id,questions)]
      d[,questions] <- lapply(d[,questions],as.numeric)

      # Change reverse questions
      recoding_rules <- list(
         list(indices=c(1:length(questions)), mapping=setNames(c(4:0/4*100),c(0:4)))
      )
      for (rule in recoding_rules) {
         d[,questions[rule$indices]] <- lapply(d[,questions[rule$indices]],
                                               function(x) dplyr::recode(x, !!!rule$mapping))
      }

      # Calculate domains
      domains <- list(
         pedsql4_tot   = 1:length(questions),
         peqsql4_psysoc = 9:length(questions),
         peqsql4_phys = 1:8
      )

      for(i in 1:length(domains)){
         d[[names(domains)[i]]] <- rowMeans(d[,questions[domains[[i]]]],na.rm=T) # Calculate means
         # Ensure more than 90% of the questions has been answered
         d[[names(domains)[i]]][rowSums(is.na(d[,questions[domains[[i]]]])) > length(domains[[i]])/2] <- NA
      }

      # Create output
      o <- d[,!(colnames(d) %in% questions)]

   }else if(scale == "PRFQ"){
      # *************************
      # PRFQ - UNVALIDATED ######
      # *************************
      if(length(questions) != 18) stop("There must be 18 questions to calculate PRFQ")
      d <- df[,c(id,questions)]
      d[,questions] <- lapply(d[,questions],as.numeric)

      # Change reverse questions
      recoding_rules <- list(
         list(indices=c(11,18), mapping=setNames(c(7:1),c(1:7)))
      )
      for (rule in recoding_rules) {
         d[,questions[rule$indices]] <- lapply(d[,questions[rule$indices]],
                                               function(x) dplyr::recode(x, !!!rule$mapping))
      }

      # Calculate domains
      domains <- list(
         prfq_pm   = c(1,4,7,10,13,16),
         prfq_cm = c(2,5,8,11,14,17),
         prfq_ic = c(3,6,9,12,15,18)
      )

      for(i in 1:length(domains)){
         d[[names(domains)[i]]] <- rowMeans(d[,questions[domains[[i]]]],na.rm=T) # Calculate means
         # Ensure more than 90% of the questions has been answered
         d[[names(domains)[i]]][rowSums(is.na(d[,questions[domains[[i]]]])) > length(domains[[i]])/10] <- NA
      }

      # Create output
      o <- d[,!(colnames(d) %in% questions)]



   }else if(scale == "PARCA-R"){
      # **************************
      # PARCA-R - UNVALIDATED ####
      # **************************
      if(length(questions) != 34) stop("There must be 34 questions to calculate PARCAR.")
      if(!exists("age")) stop("There must be a column for age in the data frame.")
      d <- df[,c(id,age,questions)]
      d$`PARCA-R raw` <- rowSums(d[,questions],na.rm=T)
      d[rowSums(is.na(d[,questions])) > 4,"PARCA-R raw"] <- NA
      if(age >= 23*30.5+16 & age <= 24*30.5+15 & sex == "M"){
         d$`PARCA-R standard` <- dplyr::recode(d$`PARCA-R raw`,
                                               `0`=49,`1`=49,`2`=49,`3`=49,`4`=49,`5`=49,`6`=49,`7`=49,`8`=49,`9`=49,
                                               `10`=49,`11`=50,`12`=52,`13`=54,`14`=57,`15`=59,`16`=62,`17`=65,`18`=68,`19`=72,
                                               `20`=75,`21`=78,`22`=81,`23`=84,`24`=87,`25`=91,`26`=95,`27`=98,`28`=102,`29`=107,
                                               `30`=112,`31`=117,`32`=124,`33`=130,`34`=137)
      }else if(age >= 23*30.5+16 & age <= 24*30.5+15 & sex == "F"){
         d$`PARCA-R standard` <- dplyr::recode(d$`PARCA-R raw`,
                                               `0`=10,`1`=10,`2`=10,`3`=10,`4`=10,`5`=12,`6`=17,`7`=22,`8`=26,`9`=31,
                                               `10`=35,`11`=39,`12`=42,`13`=46,`14`=49,`15`=53,`16`=56,`17`=59,`18`=62,`19`=66,
                                               `20`=69,`21`=72,`22`=75,`23`=79,`24`=82,`25`=86,`26`=89,`27`=93,`28`=97,`29`=102,
                                               `30`=107,`31`=112,`32`=118,`33`=124,`34`=133)
      }else if(age >= 24*30.5+16 & age <= 25*30.5+15 & sex == "M"){
         d$`PARCA-R standard` <- dplyr::recode(d$`PARCA-R raw`,
                                               `0`=49,`1`=49,`2`=49,`3`=49,`4`=49,`5`=49,`6`=49,`7`=49,`8`=49,`9`=49,
                                               `10`=49,`11`=50,`12`=51,`13`=53,`14`=55,`15`=58,`16`=61,`17`=64,`18`=67,`19`=70,
                                               `20`=73,`21`=76,`22`=79,`23`=82,`24`=86,`25`=89,`26`=93,`27`=97,`28`=101,`29`=105,
                                               `30`=110,`31`=115,`32`=122,`33`=129,`34`=135)
      }else if(age >= 24*30.5+16 & age <= 25*30.5+15 & sex == "F"){
         d$`PARCA-R standard` <- dplyr::recode(d$`PARCA-R raw`,
                                               `0`=10,`1`=10,`2`=10,`3`=10,`4`=10,`5`=12,`6`=16,`7`=21,`8`=25,`9`=30,
                                               `10`=34,`11`=38,`12`=41,`13`=45,`14`=48,`15`=52,`16`=55,`17`=58,`18`=61,`19`=64,
                                               `20`=68,`21`=71,`22`=74,`23`=78,`24`=81,`25`=85,`26`=88,`27`=92,`28`=96,`29`=101,
                                               `30`=106,`31`=111,`32`=117,`33`=124,`34`=132)
      }else if(age >= 25*30.5+16 & age <= 26*30.5+15 & sex == "M"){
         d$`PARCA-R standard` <- dplyr::recode(d$`PARCA-R raw`,
                                               `0`=49,`1`=49,`2`=49,`3`=49,`4`=49,`5`=49,`6`=49,`7`=49,`8`=49,`9`=49,
                                               `10`=49,`11`=49,`12`=50,`13`=52,`14`=54,`15`=57,`16`=59,`17`=62,`18`=65,`19`=68,
                                               `20`=71,`21`=75,`22`=78,`23`=81,`24`=84,`25`=88,`26`=91,`27`=95,`28`=99,`29`=103,
                                               `30`=108,`31`=114,`32`=120,`33`=128,`34`=133)
      }else if(age >= 25*30.5+16 & age <= 26*30.5+15 & sex == "F"){
         d$`PARCA-R standard` <- dplyr::recode(d$`PARCA-R raw`,
                                               `0`=10,`1`=10,`2`=10,`3`=10,`4`=10,`5`=11,`6`=15,`7`=19,`8`=24,`9`=28,
                                               `10`=32,`11`=36,`12`=40,`13`=43,`14`=47,`15`=50,`16`=53,`17`=57,`18`=60,`19`=63,
                                               `20`=66,`21`=69,`22`=73,`23`=76,`24`=79,`25`=83,`26`=87,`27`=90,`28`=95,`29`=99,
                                               `30`=104,`31`=109,`32`=116,`33`=123,`34`=130)
      }else if(age >= 26*30.5+16 & age <= 27*30.5+15 & sex == "M"){
         d$`PARCA-R standard` <- dplyr::recode(d$`PARCA-R raw`,
                                               `0`=49,`1`=49,`2`=49,`3`=49,`4`=49,`5`=49,`6`=49,`7`=49,`8`=49,`9`=49,
                                               `10`=49,`11`=49,`12`=50,`13`=52,`14`=54,`15`=56,`16`=59,`17`=62,`18`=65,`19`=68,
                                               `20`=71,`21`=74,`22`=77,`23`=80,`24`=84,`25`=87,`26`=90,`27`=94,`28`=98,`29`=103,
                                               `30`=107,`31`=113,`32`=120,`33`=127,`34`=132)
      }else if(age >= 26*30.5+16 & age <= 27*30.5+15 & sex == "F"){
         d$`PARCA-R standard` <- dplyr::recode(d$`PARCA-R raw`,
                                               `0`=10,`1`=10,`2`=10,`3`=10,`4`=10,`5`=11,`6`=13,`7`=18,`8`=22,`9`=27,
                                               `10`=31,`11`=35,`12`=38,`13`=42,`14`=45,`15`=48,`16`=52,`17`=55,`18`=58,`19`=61,
                                               `20`=65,`21`=68,`22`=71,`23`=74,`24`=78,`25`=81,`26`=85,`27`=89,`28`=93,`29`=97,
                                               `30`=102,`31`=108,`32`=114,`33`=122,`34`=127)
      }
   }else if(scale == "ADOS-2"){
      #**************************
      # ADOS-2 - UNVALIDATED ####
      #**************************


      if(length(questions) != 46) stop("There must be 46 questions to calculate ADOS-2")
      if(is.null(df[[age.months]])) stop("There must be a column for age in months in the data frame.")
      if(is.null(df[[module]])) stop("There must be a column for module in the data frame.")

      d <- df[,c(id,questions,age.months,module)]
      d[,questions] <- lapply(d[,questions],as.numeric)

      # ADOS QUALITY
      d$ados_quality8 <- rowSums(d[,questions] == 8,na.rm=T)
      d$ados_quality9 <- rowSums(d[,questions] == 9,na.rm=T)

      # Change 5-7 to 0
      d[,questions] <- lapply(d[,questions], function(x) {
         if (is.numeric(x)) { x[x >= 5 & x <= 7] <- 0 }; return(x) })
      # Change 3 to 2
      d[,questions[2:length(questions)]] <- lapply(d[,questions[2:length(questions)]], function(x) {
         if (is.numeric(x)) { x[x == 3] <- 2 }; return(x) })

      # Identify correct algorithm
      # - Toddler-module
      d[rowSums(!is.na(d[,c(module,age.months,questions[1])])) == 3 &
           d[[module]] == 0 & (
              (d[[age.months]] >= 12 & d[[age.months]] < 21) |
                 (d[[questions[1]]] %in% c(3,4) &
                     d[[age.months]] >= 21 & d[[age.months]] <= 30)
           ),"algorithm"] <- "Toddler1"
      d[rowSums(!is.na(d[,c(module,age.months,questions[1])])) == 3 &
           d[[module]] == 0 & d[[questions[1]]] %in% c(0:2) &
           d[[age.months]] >= 21 & d[[age.months]] <= 30,"algorithm"] <- "Toddler2"
      # - Module 1
      d[rowSums(!is.na(d[,c(module,age.months,questions[1])])) == 3 &
           d[[module]] == 1 & d[[questions[1]]] %in% c(3,4)
        ,"algorithm"] <- "Module11"
      d[rowSums(!is.na(d[,c(module,age.months,questions[1])])) == 3 &
           d[[module]] == 1 & d[[questions[1]]] %in% c(0:2)
        ,"algorithm"] <- "Module12"
      # - Module 2
      d[rowSums(!is.na(d[,c(module,age.months,questions[1])])) == 3 &
           d[[module]] == 2,"algorithm"] <- "Module2"
      # - Module 3
      d[rowSums(!is.na(d[,c(module,age.months,questions[1])])) == 3 &
           d[[module]] == 3,"algorithm"] <- "Module3"

      # Calculate domains
      domains <- list(
         Toddler1.ados_sa_raw = c(3,9,11,14,15,16,24,25,28,29),
         Toddler1.ados_ir_raw = c(4,38,39,42),
         Toddler2.ados_sa_raw = c(8,11,14,15,17,18,19,25,29,32,34),
         Toddler2.ados_ir_raw = c(38,39,42),
         Module11.ados_sa_raw = c(3,9,11,13,14,15,19,22,23,24),
         Module11.ados_ir_raw = c(4,38,39,41),
         Module12.ados_sa_raw = c(3,8,9,11,13,14,15,19,22,24),
         Module12.ados_ir_raw = c(6,38,39,41),
         Module2.ados_sa_raw = c(7,8,11,12,13,15,16,18,23,24),
         Module2.ados_ir_raw = c(5,38,39,41),
         Module3.ados_sa_raw = c(8,9,10,11,12,14,17,19,22,23),
         Module3.ados_ir_raw = c(5,38,39,41)
      )

      # Calculate raw scores
      for(i in unique(na.omit(d$algorithm))){
         # i <- "Module11"
         cur_dom <- domains[grepl(i,names(domains))]
         names(cur_dom) <- gsub(paste0(i,"\\."),"",names(cur_dom))

         for(j in 1:length(cur_dom)){
            tst <- !is.na(d$algorithm) & d$algorithm == i
            n_miss <- rowSums(is.na(d[tst,questions[cur_dom[[j]]]]))
            # Convert 8 and 9 to average
            d[tst,questions[cur_dom[[j]]]] <-
               lapply(d[tst,questions[cur_dom[[j]]]], function(x) {
               if (is.numeric(x)) { x[x >= 8 & x <= 9] <- NA }; return(x) })
            tmp <- round(rowMeans(d[tst,questions[cur_dom[[j]]]],na.rm=T))
            for(k in 1:sum(tst)){
               d[tst,questions[cur_dom[[j]]]][k,][is.na(d[tst,questions[cur_dom[[j]]]][k,])] <- tmp[[k]]
            }

            # Now calculate sum
            d[tst,names(cur_dom)[j]] <-
               rowSums(d[tst,questions[cur_dom[[j]]]],na.rm=T)
            # Ensure more than 90% of the questions has been answered
            d[tst,n_miss/length(cur_dom[[j]]) > 0.1] <- NA
         }
      }
      d$ados_tot_raw <- d$ados_sa_raw+d$ados_ir_raw

      # Convert to scale score
      rawtoscales <- questionaire_helper()$ados2
      for(i in unique(na.omit(d$algorithm))){
         # i <- "Toddler1"
         cur_r2s <- rawtoscales[grepl(i,names(rawtoscales))]
         names(cur_r2s) <- gsub(paste0(i,"\\."),"",names(cur_r2s))

         for(j in 1:length(cur_r2s)){
            # j <- 1
            rtst <- !is.na(d$algorithm) & d$algorithm == i
            ctst <- grepl(substr(names(cur_r2s[j]),1,8),colnames(d)) &
               !grepl("css",colnames(d))

            if(is.numeric(cur_r2s[[j]])){
               if(sum(rtst) > 0){
                  d[rtst,names(cur_r2s)[j]] <- suppressWarnings(dplyr::recode(d[rtst,ctst],!!!cur_r2s[[j]]))
               }
            }else if(is.data.frame(cur_r2s[[j]])){
               for(k in 1:ncol(cur_r2s[[j]])){

                  rtst2 <- !is.na(d[[age.months]]) &
                     floor(d[[age.months]]/12) == colnames(cur_r2s[[j]])[k]

                  nmd_lst <- cur_r2s[[j]][,k]
                  names(nmd_lst) <- rownames(cur_r2s[[j]])
                  if(sum(rtst & rtst2) > 0){
                     d[rtst & rtst2,names(cur_r2s)[j]] <- suppressWarnings(dplyr::recode(
                        d[rtst & rtst2,ctst],!!!nmd_lst))
                  }
               }
            }
         }
      }

      # o <- d[,!(colnames(d) %in% c("algorithm",age.months,module,questions))]
      o <- d[,!(colnames(d) %in% c(module,questions))]

   }else if(scale == "Vineland-3"){
      #*******************************
      # Vineland-3 - UNVALIDATED #####
      #*******************************
      if(length(questions) != 476) stop("There must be 476 questions to calculate Vineland-3")
      if(is.null(d[[age.months]])) stop("There must be a column for age in months in the data frame.")
      if (!"module" %in% names(list(...))) {
         stop("Der skal defineres et modul - fx 'est' eller noget andet")
      }

      d <- df[,c(id,age.months,questions)]
      d[,questions] <- lapply(d[,questions],as.numeric)

      # Calculate raw score domains
      domains <- list(
         v_lyt = c(1:39),
         v_tal = c(40:88),
         v_laes = c(89:126),
         v_per = c(127:181),
         v_hje = c(182:211),
         v_naer = c(212:251),
         v_rel = c(252:286),
         v_leg = c(287:322),
         v_til = c(323:355),
         v_gmo = c(356:398),
         v_fmo = c(399:432),
         v_ma = c(433:445),
         v_mb = c(446:456),
         v_mc = c(457:476)

      )

      if(module == "est"){
         suffix <- "est"
      }else{
         suffix <- "raw"
      }
      # Calculate raw scores
      for(i in 1:length(domains)){
         n_miss <- rowSums(is.na(d[,questions[domains[[i]]]]))

         d[[paste0(names(domains)[i],"_",suffix)]] <- rowSums(d[,questions[domains[[i]]]],na.rm=T)

         #Missing
         if(module != "est"){
            collapsed_rows <- apply(d[, questions[domains[[i]]]], 1, function(x) paste0(x, collapse = ""))

            d[n_miss == length(domains[[i]]) |
                 (grepl("NA",collapsed_rows) &
                     !grepl("00000NA",collapsed_rows)),
              paste0(names(domains)[i],"_",suffix)] <- NA
         }else{
            d[[paste0(names(domains)[i],"_",suffix)]] <-
               round(d[[paste0(names(domains)[i],"_",suffix)]] / length(domains[[i]]),3)
         }
      }

      if(module != "est"){

         # Convert to scale score
         rawtoscales <- questionaire_helper()$vineland3
         for(i in names(rawtoscales)){
            # i <- names(rawtoscales)[1]
            rtst <- !is.na(d[[age.months]]) &
               d[[age.months]] >= as.numeric(substr(i,2,3)) &
               d[[age.months]] < as.numeric(substr(i,5,6))
            cur_r2s <- data.frame(rawtoscales[grepl(i,names(rawtoscales))])
            names(cur_r2s) <- gsub(paste0(i,"\\."),"",names(cur_r2s))

            for(j in 1:ncol(cur_r2s)){
               # j <- 1
               ctst <- grepl(paste0("^v_",substr(names(cur_r2s[j]),1,3)),colnames(d)) &
                  !grepl("ss",colnames(d))

               nmd_lst <- cur_r2s[[j]]
               names(nmd_lst) <- rownames(cur_r2s)
               if(sum(rtst,na.rm=T) > 0){
                  d[rtst,paste0("v_",names(cur_r2s)[j])] <- dplyr::recode(
                     d[rtst,ctst],!!!nmd_lst)
               }
            }
         }

         #Calculate domain score
         domains <- questionaire_helper()$vineland3$domains

         d$v_kom_domscore <- dplyr::recode(
            rowSums(d[,c("v_lyt_ss","v_tal_ss","v_laes_ss")]),
            !!!setNames(domains$kom_domainscore, rownames(domains)))
         d$v_fdd_domscore <- dplyr::recode(
            rowSums(d[,c("v_per_ss","v_hje_ss","v_naer_ss")]),
            !!!setNames(domains$fdd_domainscore, rownames(domains)))
         d$v_soc_domscore <- dplyr::recode(
            rowSums(d[,c("v_rel_ss","v_leg_ss","v_til_ss")]),
            !!!setNames(domains$soc_domainscore, rownames(domains)))
         d$v_mot_domscore <- dplyr::recode(
            rowSums(d[,c("v_gmo_ss","v_fmo_ss")]),
            !!!setNames(domains$mot_domainscore, rownames(domains)))

         #Calculate gaf
         gaf <- questionaire_helper()$vineland3$gaf
         d$v_gaf <- dplyr::recode(
            rowSums(d[,c("v_kom_domscore","v_fdd_domscore","v_soc_domscore")]),
            !!!setNames(gaf$GAF, rownames(gaf)))
      }


      o <- d[,!(colnames(d) %in% c(age.months,questions))]


   }else if(scale == "WHOQOL-BREF"){
      # ******************************
      # WHOQOL-BREF - UNVALIDATED ####
      # ******************************
      if(length(questions) != 26) stop("There must be 26 questions to calculate WHOQOL-BREF")
      d <- df[,c(id,questions)]
      d[,questions] <- lapply(d[,questions],as.numeric)

      # Change reverse questions
      recoding_rules <- list(
         list(indices=c(3,4,9,26), mapping=setNames(c(5:1),c(1:5)))
      )
      for (rule in recoding_rules) {
         d[,questions[rule$indices]] <- lapply(d[,questions[rule$indices]],
                                               function(x) dplyr::recode(x, !!!rule$mapping))
      }

      # Calculate domains
      domains <- list(
         whoqolbref_oa = c(1,2),
         whoqolbref_phys = c(3,4,10,15,16,17,18),
         whoqolbref_psych = c(5,6,7,11,19,26),
         whoqolbref_soc = c(20,21,22),
         whoqolbref_env = c(8,9,12,13,14,23,24,25)
      )

      for(i in 1:length(domains)){
         n_miss <- rowSums(is.na(d[,questions[domains[[i]]]]))

         d[[names(domains)[i]]] <-
            (
               (rowSums(d[,questions[domains[[i]]]],na.rm=T)-
                   (length(domains[[i]])-n_miss)
               )/
                  ((length(domains[[i]])-n_miss)*4)
            )*100# Calculate means

         # Ensure more than 90% of the questions has been answered
         if(i %in% c(2,3)){
            d[[names(domains)[i]]][rowSums(is.na(d[,questions[domains[[i]]]])) > 1] <- NA
         }else{
            d[[names(domains)[i]]][rowSums(is.na(d[,questions[domains[[i]]]])) > 0] <- NA
         }
      }

      # Create output
      o <- d[,!(colnames(d) %in% questions)]

   }else if(scale == "SRS-2"){
      # ******************************
      # SRS-2 - UNVALIDATED ####
      # ******************************
      if(length(questions) != 65) stop("There must be 65 questions to calculate SRS-2")
      d <- df[,c(id,questions)]
      d[,questions] <- lapply(d[,questions],as.numeric)

      # Calculate domains
      domains <- list(
         srs_bev_raw = c(2,7,25,32,45,52,54,56),
         srs_kog_raw = c(5,10,15,17,30,40,42,44,48,58,59,62),
         srs_kom_raw = c(12,13,16,18,19,21,22,26,33,35,36,37,38,41,46,47,51,53,55,57,60,61),
         srs_mot_raw = c(1,3,6,9,11,23,27,34,43,64,65),
         srs_bira_raw = c(4,8,14,20,24,28,29,31,39,49,50,63),
         srs_ski_raw = c(2,7,25,32,45,52,54,56,
                         5,10,15,17,30,40,42,44,48,58,59,62,
                         12,13,16,18,19,21,22,26,33,35,36,37,38,41,46,47,51,53,55,57,60,61,
                         1,3,6,9,11,23,27,34,43,64,65),
         srs_tot_raw = c(1:65)
      )

      for(i in 1:length(domains)){
         n_miss <- rowSums(is.na(d[,questions[domains[[i]]]]))

         d[[names(domains)[i]]] <- rowSums(d[,questions[domains[[i]]]],na.rm=T)

         # Ensure more than 90% of the questions has been answered
         if(i %in% c(2,3)){
            d[[names(domains)[i]]][rowSums(is.na(d[,questions[domains[[i]]]])) > 1] <- NA
         }else{
            d[[names(domains)[i]]][rowSums(is.na(d[,questions[domains[[i]]]])) > 0] <- NA
         }
      }

      # Calculate T-scores
      tscore <- list(
         raw = 0:195,
         srs_bev = c(29,33,36,49,43,47,50,53,57,69,64,67,71,74,78,81,84,88,91,95,
                     98,102,105,109,112),
         srs_kog = c(34,36,39,41,44,46,49,51,54,57,59,62,64,67,69,72,74,77,79,82,
                     84,87,89,92,94,97,100,102,105,107,110,112,115,117,120,122,125),
         srs_kom = c(36,37,39,41,43,44,46,48,50,51,53,55,57,59,60,62,64,66,67,69,
                     71,73,75,76,78,80,82,83,85,87,89,90,92,94,96,98,99,101,103,
                     105,106,108,110,112,114,115,117,119,121,122,124,126,128,130,
                     131,133,135,137,138,140,142,144,145,147,149,151,153),
         srs_mot = c(34,37,49,43,46,48,51,54,57,60,63,66,69,72,75,78,81,84,87,90,
                     93,96,99,102,105,107,110,113,116,119,122,125,128,131),
         srs_bira = c(41,43,46,48,50,53,55,57,60,62,65,67,69,72,74,76,79,81,84,
                      86,88,91,93,95,98,100,103,105,107,110,112,114,117,119,122,
                      124,126),
         srs_ski = c(30,31,32,33,33,34,35,36,36,37,38,39,39,40,41,42,42,43,44,45,
                     45,46,47,48,48,49,50,51,51,52,53,54,54,55,56,57,57,58,59,60,
                     60,61,62,63,63,64,65,66,66,67,68,69,69,70,71,72,72,73,74,75,
                     75,76,77,78,78,79,80,81,81,82,83,84,84,85,86,87,87,88,89,90,
                     90,91,92,93,93,94,95,96,96,97,98,99,99,100,101,102,102,103,
                     104,105,105,106,107,108,108,109,110,111,111,112,113,114,114,
                     115,116,117,117,118,119,120,120,121,122,123,123,124,125,126,
                     126,127,128,129,129,130,131,132,132,133,134,135,135,136,137,
                     138,138,139,140,141,141,142,143,144,144,145,146,147,147,148,
                     149,150),
         srs_tot = c(32,33,33,34,34,35,36,36,37,37,38,39,39,40,40,41,42,42,43,43,
                     44,45,45,46,46,47,48,48,49,49,50,51,51,52,52,53,54,54,55,55,
                     56,57,57,58,58,59,60,60,61,61,62,63,63,64,64,65,66,66,67,67,
                     68,69,69,70,70,71,72,72,73,73,74,74,75,76,76,77,77,78,79,79,
                     80,80,81,82,82,83,83,84,85,85,86,86,87,88,88,89,89,90,91,91,
                     92,92,93,94,94,95,95,96,97,97,98,98,99,100,100,101,101,102,
                     103,103,104,104,105,106,106,107,107,108,109,109,110,110,111,
                     112,112,113,113,114,115,115,116,116,117,118,118,119,119,120,
                     121,121,122,122,123,124,124,125,125,126,126,127,128,128,129,
                     129,130,131,131,132,132,133,134,134,135,135,136,137,137,138,
                     138,139,140,140,141,141,142,143,143,144,144,145,146,146,147,
                     147,148,149)
      )

      for(i in 2:length(tscore)){
         tscore_df <- data.frame(tscore$raw[1:length(tscore[[i]])],
                                 tscore[[i]])
         raw_col <- colnames(d)[grepl(names(tscore)[i],colnames(d)) &
                                   !grepl("tscore",colnames(d))]

         d[[paste0(names(tscore)[i],"_tscore")]] <- tscore_df[match(d[[raw_col]], tscore_df[[1]]),2]
      }

      # Create output
      o <- d[,!(colnames(d) %in% questions)]

   }else if(scale == "Mullen"){
      # ******************************
      # Mullen - UNVALIDATED ####
      # ******************************
      if(length(questions) != 1) stop("There must be 1 raw score calculate Mulle t-score")
      if(is.null(df[[age.months]])) stop("There must be a column for age in months in the data frame.")
      d <- df[,c(id,questions,age.months)]
      d[[questions]] <- as.numeric(d[[questions]])

      # Calculate T-scores from individual age
      tage <- list(NA,
                   c(23:24),c(25:26),c(27:28),c(29:30),c(31:33),c(34:36),c(37:39),
                   c(40:42),c(43:45),c(46:48),c(49:51),c(52:54),c(55:57),c(58:60),
                   c(61:63),c(64:66),c(67:96))

      tscore <- list(
         raw = 50:0,
         `23` = c(80,80,80,80,80,80,80,80,80,80,80,80,80,80,80,80,79,77,74,71,68,
                  66,63,59,56,52,50,47,44,40,38,35,32,30,27,24,20,20,20,20,20,20,
                  20,20,20,20,20,20,20,20,20,20),
         `25` = c(80,80,80,80,80,80,80,80,80,80,80,80,80,80,79,76,74,71,69,66,64,
                  61,57,54,51,48,46,43,40,37,34,32,30,26,23,20,20,20,20,20,20,20,
                  20,20,20,20,20,20,20,20,20),
         `27` = c(80,80,80,80,80,80,80,80,80,80,80,80,79,77,74,72,69,67,65,62,59,
                  56,53,50,47,45,42,39,36,34,31,29,26,23,21,20,20,20,20,20,20,20,
                  20,20,20,20,20,20,20,20,20),
         `29` = c(80,80,80,80,80,80,80,80,80,80,79,76,74,72,70,68,66,63,61,58,55,
                  52,49,46,44,41,38,36,33,31,29,26,23,20,20,20,20,20,20,20,20,20,
                  20,20,20,20,20,20,20,20,20),
         `31` = c(80,80,80,80,80,80,80,80,79,77,75,73,70,69,67,65,62,59,57,54,51,
                  49,46,43,40,38,35,33,30,28,25,22,20,20,20,20,20,20,20,20,20,20,
                  20,20,20,20,20,20,20,20,20),
         `34` = c(80,80,80,80,80,79,77,75,73,71,70,68,66,64,61,58,86,83,81,49,46,
                  44,41,38,35,33,30,27,24,20,20,20,20,20,20,20,20,20,20,20,20,20,
                  20,20,20,20,20,20,20,20,20),
         `37` = c(80,80,80,80,78,76,74,72,70,68,66,63,61,58,56,54,51,49,47,45,42,
                  39,37,34,31,27,24,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,
                  20,20,20,20,20,20,20,20,20),
         `40` = c(80,80,80,78,76,73,71,68,66,63,61,58,56,54,52,50,78,46,43,41,38,
                  35,32,29,25,21,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,
                  20,20,20,20,20,20,20,20,20),
         `43` = c(80,80,80,77,73,70,67,64,61,59,56,54,52,50,48,46,44,41,39,36,33,
                  30,27,24,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,
                  20,20,20,20,20,20,20,20,20),
         `46` = c(80,80,78,73,69,65,62,59,57,55,53,51,49,47,45,42,40,37,35,32,29,
                  26,23,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,
                  20,20,20,20,20,20,20,20,20),
         `49` = c(80,80,74,69,65,61,59,56,54,51,49,47,45,43,40,38,36,33,31,28,26,
                  23,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,
                  20,20,20,20,20,20,20,20,20),
         `52` = c(80,80,70,65,61,57,55,52,50,47,45,43,41,39,36,34,32,30,28,25,23,
                  21,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,
                  20,20,20,20,20,20,20,20,20),
         `55` = c(80,80,67,62,57,54,51,48,46,43,41,39,37,34,33,31,29,26,24,22,20,
                  20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,
                  20,20,20,20,20,20,20,20,20),
         `58` = c(80,80,63,58,83,50,47,44,42,39,37,35,33,31,30,27,25,23,21,20,20,
                  20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,
                  20,20,20,20,20,20,20,20,20),
         `61` = c(80,80,60,53,49,46,43,40,37,35,33,31,29,27,24,22,20,20,20,20,20,
                  20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,
                  20,20,20,20,20,20,20,20,20),
         `64` = c(80,80,54,49,44,41,38,36,33,31,28,26,24,21,20,20,20,20,20,20,20,
                  20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,
                  20,20,20,20,20,20,20,20,20),
         `67` = c(80,80,51,44,41,38,35,32,30,27,24,22,20,20,20,20,20,20,20,20,20,
                  20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,20,
                  20,20,20,20,20,20,20,20,20)
      )

      for(i in 2:length(tscore)){
         tscore_df <- data.frame(tscore$raw[1:length(tscore[[i]])],
                                 tscore[[i]])

         d[floor(d[[age.months]]) %in% tage[[i]],"mullen_ss"] <- tscore_df[match(d[floor(d[[age.months]]) %in% tage[[i]],questions], tscore_df[[1]]),2]
      }

      # Create output
      o <- d[,!(colnames(d) %in% c(questions,age.months))]

   }else if(scale == "NewReynell"){
      # ******************************
      # NewReynell - UNVALIDATED ####
      # ******************************
      if(length(questions) != 8) stop("There must be 8 questions to calculate NewReynell")
      if(is.null(df[[age.months]])) stop("There must be a column for age in months in the data frame.")
      d <- df[,c(id,questions,age.months)]
      d[,questions] <- lapply(d[,questions],as.numeric)

      # Calculate domains
      domains <- list(
         reynell_tot_raw = c(1:8)
      )
      n_miss <- rowSums(is.na(d[,questions[domains[[1]]]]))
      d[[names(domains)[1]]] <- rowSums(d[,questions[domains[[1]]]],na.rm=T)
      d[[names(domains)[1]]][rowSums(is.na(d[,questions[domains[[1]]]])) > 0] <- NA

      # Calculate T-scores from individual age
      tage <- list(NA,
                   c(24:26),c(27:29),c(30:32),c(33:35),c(36:38),c(39:41),c(42:44),c(45:47),
                   c(48:50),c(51:53),c(54:56),c(57:59),c(60:62),c(63:65),c(66:68),c(69:71),
                   c(72:96))

      tscore <- list(
         raw = 0:72,
         `24` = c(71,72,73,74,75,77,78,79,80,81,82,83,85,86,87,88,89,90,92,93,94,
                  95,96,97,98,100,101,102,103,104,105,106,108,109,110,111,112,113,
                  115,116,117,118,119,120,121,123,124,125,126,127,128,130,131,131,
                  131,131,131,131,131,131,131,131,131,131,131,131,131,131,131,131,
                  131,131,131),
         `27` = c(69,69,69,69,69,70,71,72,73,74,76,77,78,79,80,81,83,84,85,86,87,
                  88,90,91,92,93,94,95,97,98,99,100,101,102,104,105,106,107,108,
                  109,111,112,113,114,115,116,118,119,120,121,122,123,125,126,127,
                  128,129,130,131,131,131,131,131,131,131,131,131,131,131,131,131,
                  131,131),
         `30` = c(69,69,69,69,69,69,69,69,69,69,69,70,71,72,74,75,76,77,78,80,81,
                  82,83,84,86,89,88,89,90,92,93,94,95,96,98,99,100,101,102,104,
                  105,106,107,108,110,111,112,113,114,116,117,118,119,120,122,123,
                  124,125,126,128,129,130,131,131,131,131,131,131,131,131,131,131,
                  131),
         `33` = c(69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,70,72,73,74,
                  75,77,78,79,80,82,83,84,85,87,88,89,90,9293,94,95,97,98,99,100,
                  102,103,104,105,107,108,109,110,112,103,114,115,117,118,119,120,
                  122,123,124,125,127,128,129,130,131,131,131,131,131,131,131),
         `36` = c(69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,
                  69,70,71,72,74,75,76,78,79,80,82,83,84,86,87,88,90,91,92,94,95,
                  96,98,99,100,102,103,104,106,107,108,109,111,112,113,115,116,
                  117,119,120,121,123,124,125,127,128,129,131,131,131,131,131),
         `39` = c(69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,
                  69,69,69,69,69,69,70,71,72,74,75,77,78,80,81,82,84,85,87,88,89,
                  91,92,94,95,96,98,99,101,102,104,105,106,108,109,111,112,113,
                  115,116,118,119,120,122,123,125,126,128,129,130,131,131),
         `42` = c(69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,
                  69,69,69,69,69,69,69,69,69,69,69,70,72,73,75,76,78,79,81,82,84,
                  85,87,88,90,91,93,94,96,97,99,100,102,103,105,106,108,110,111,
                  113,114,116,117,119,120,122,123,125,126,128,129,131),
         `45` = c(69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,
                  69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,70,71,73,75,76,78,
                  80,81,83,84,86,88,89,91,93,94,96,98,99,101,102,104,106,107,109,
                  111,112,114,116,117,119,121,122,124,125,127,129),
         `48` = c(69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,
                  69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,70,72,
                  74,76,77,79,81,83,84,86,88,90,92,93,95,97,99,100,102,104,106,
                  107,109,111,113,115,116,118,120,122,123,125,127),
         `51` = c(69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,
                  69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,
                  69,70,72,74,76,78,80,82,84,86,87,89,91,93,95,97,99,101,103,105,
                  106,108,110,112,114,116,118,120,122,124,125),
         `54` = c(69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,
                  69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,
                  69,69,69,70,72,74,76,78,80,82,84,86,88,90,92,94,96,98,100,102,
                  104,106,108,110,112,114,116,118,120,122,124),
         `57` = c(69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,
                  69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,
                  69,69,69,69,69,71,73,75,77,80,82,84,86,88,90,92,94,96,98,100,
                  102,105,107,109,111,113,115,117,119,121,123),
         `60` = c(69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,
                  69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,
                  69,69,69,69,69,70,72,74,76,79,81,83,85,87,89,91,93,95,97,99,
                  102,104,106,108,110,112,114,116,118,120,123),
         `63` = c(69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,
                  69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,
                  69,69,69,69,69,69,71,73,75,77,79,81,83,86,87,89,92,94,96,98,100,
                  102,104,106,108,110,112,114,116,118,120),
         `66` = c(69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,
                  69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,
                  69,69,69,69,69,69,71,73,75,77,79,81,83,86,87,89,91,93,95,97,99,
                  101,103,104,106,108,110,112,114,116,118),
         `69` = c(69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,
                  69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,
                  69,69,69,69,69,69,70,73,75,77,79,80,82,85,87,89,90,92,95,97,99,
                  101,102,104,106,107,109,111,112,114,116),
         `72` = c(69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,
                  69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,69,
                  69,69,69,69,69,69,70,73,75,77,79,80,82,85,87,89,90,92,95,97,99,
                  101,102,104,106,107,109,111,112,114,116)
      )


      for(i in 2:length(tscore)){
         tscore_df <- data.frame(tscore$raw[1:length(tscore[[i]])],
                                 tscore[[i]])

         d[floor(d[[age.months]]) %in% tage[[i]],"reynell_ss"] <- tscore_df[match(d[floor(d[[age.months]]) %in% tage[[i]],"reynell_tot_raw"], tscore_df[[1]]),2]
      }

      # Create output
      o <- d[,!(colnames(d) %in% c(questions,age.months))]

   }else if(scale == "WPPSI-IV"){
      # ******************************
      # WPPSI-IV - UNVALIDATED ####
      # ******************************
      if(length(questions) != 4) stop("There must be 4 rawscores to calculate WPPSI-IV")
      if(is.null(d[[age.months]])) stop("There must be a column for age in months in the data frame.")
      d <- df[,c(id,questions,age.months)]
      d[,questions] <- lapply(d[,questions],as.numeric)

      tage <- questionaire_helper()$wppsi$age
      tscore <- questionaire_helper()$wppsi$scores

      for(i in 1:length(tage)){
         tst <- floor(d[[age.months]]) %in% tage[[i]]

         tmp <- data.frame(cbind(rownames(tscore),
                                 tscore[,which(colnames(tscore) == "OR")[i]]))
         d[tst,"wppsi_ord_ss"] <-
            tmp[match(d[tst,questions[[1]]], tmp[[1]]),2]

         tmp <- data.frame(cbind(rownames(tscore),
                                 tscore[,which(colnames(tscore) == "IN")[i]]))
         tmp[] <- lapply(tmp[],as.numeric)
         d[tst,"wppsi_info_ss"] <-
            tmp[match(d[tst,questions[[2]]], tmp[[1]]),2]

         tmp <- data.frame(cbind(rownames(tscore),
                                 tscore[,which(colnames(tscore) == "BL")[i]]))
         tmp[] <- lapply(tmp[],as.numeric)
         d[tst,"wppsi_blok_ss"] <-
            tmp[match(d[tst,questions[[3]]], tmp[[1]]),2]

         tmp <- data.frame(cbind(rownames(tscore),
                                 tscore[,which(colnames(tscore) == "PS")[i]]))
         tmp[] <- lapply(tmp[],as.numeric)
         d[tst,"wppsi_pusle_ss"] <-
            tmp[match(d[tst,questions[[4]]], tmp[[1]]),2]

      }
      colz <- c("wppsi_ord_ss","wppsi_info_ss","wppsi_blok_ss","wppsi_pusle_ss")
      d[,colz] <- lapply(d[,colz],as.numeric)
      d[["wppsi_lang_tot_ss"]] <- rowSums(d[,c("wppsi_ord_ss","wppsi_info_ss")])/2
      d[["wppsi_visual_tot_ss"]] <- rowSums(d[,c("wppsi_blok_ss","wppsi_pusle_ss")])/2

      # Create output
      o <- d[,!(colnames(d) %in% c(questions,age.months))]

   }else if(scale == "CBI"){
      # **************************
      # CBI ######################
      # **************************
      if(length(questions) != 19) stop("There must be 19 questions to calculate CBI.")
      d <- df[,c(id,questions)]
      d[,questions] <- lapply(d[,questions],function(x)
         dplyr::recode(x,`0`=0,`1`=1,`2`=2,`3`=3,`4`=4,`25`=1,`50`=2,`75`=3,`100`=4))

      if(!is.na(setting) & setting == "danish"){
         d$`Personal burnout` <- rowSums(d[,questions[1:6]])
         d$`Personal burnout - group`[d$`Personal burnout` %in% c(0:5)] <- "No burnout"
         d$`Personal burnout - group`[d$`Personal burnout` %in% c(6:11)] <- "Light burnout"
         d$`Personal burnout - group`[d$`Personal burnout` %in% c(12:17)] <- "Moderate burnout"
         d$`Personal burnout - group`[d$`Personal burnout` >= 18] <- "Severe burnout"

         d$`Work-related burnout` <- rowSums(d[,questions[7:13]],na.rm=T)
         d$`Work-related burnout - group`[d$`Work-related burnout` %in% c(0:6)] <- "No burnout"
         d$`Work-related burnout - group`[d$`Work-related burnout` %in% c(7:13)] <- "Light burnout"
         d$`Work-related burnout - group`[d$`Work-related burnout` %in% c(14:20)] <- "Moderate burnout"
         d$`Work-related burnout - group`[d$`Work-related burnout` >= 21] <- "Severe burnout"

         d$`Patient-related burnout` <- rowSums(d[,questions[14:19]],na.rm=T)
         d$`Patient-related burnout - group`[d$`Patient-related burnout` %in% c(0:5)] <- "No burnout"
         d$`Patient-related burnout - group`[d$`Patient-related burnout` %in% c(6:11)] <- "Light burnout"
         d$`Patient-related burnout - group`[d$`Patient-related burnout` %in% c(12:17)] <- "Moderate burnout"
         d$`Patient-related burnout - group`[d$`Patient-related burnout` >= 18] <- "Severe burnout"
      }else{
         d$`Personal burnout` <- rowMeans(d[,questions[1:6]],na.rm=T)*25
         d$`Personal burnout`[rowSums(is.na(d[,questions[1:6]])) > 3] <- NA
         d$`Personal burnout - group`[d$`Personal burnout` < 25] <- "No burnout"
         d$`Personal burnout - group`[d$`Personal burnout` >= 25 & d$`Personal burnout` < 50] <- "Light burnout"
         d$`Personal burnout - group`[d$`Personal burnout` >= 50 & d$`Personal burnout` < 75] <- "Moderate burnout"
         d$`Personal burnout - group`[d$`Personal burnout` >= 75] <- "Severe burnout"

         d$`Work-related burnout` <- rowMeans(d[,questions[7:13]],na.rm=T)*25
         d$`Work-related burnout`[rowSums(is.na(d[,questions[7:13]])) > 3] <- NA
         d$`Work-related burnout - group`[d$`Work-related burnout` < 25] <- "No burnout"
         d$`Work-related burnout - group`[d$`Work-related burnout` >= 25 & d$`Work-related burnout` < 50] <- "Light burnout"
         d$`Work-related burnout - group`[d$`Work-related burnout` >= 50 & d$`Work-related burnout` < 75] <- "Moderate burnout"
         d$`Work-related burnout - group`[d$`Work-related burnout` >= 75] <- "Severe burnout"

         d$`Patient-related burnout` <- rowMeans(d[,questions[14:19]],na.rm=T)*25
         d$`Patient-related burnout`[rowSums(is.na(d[,questions[14:19]])) > 3] <- NA
         d$`Patient-related burnout - group`[d$`Patient-related burnout` < 25] <- "No burnout"
         d$`Patient-related burnout - group`[d$`Patient-related burnout` >= 25 & d$`Patient-related burnout` < 50] <- "Light burnout"
         d$`Patient-related burnout - group`[d$`Patient-related burnout` >= 50 & d$`Patient-related burnout` < 75] <- "Moderate burnout"
         d$`Patient-related burnout - group`[d$`Patient-related burnout` >= 75] <- "Severe burnout"
      }
      o <- d[,!(colnames(d) %in% questions)]

   }else if(scale == "Kidscreen-52"){
      # **************************
      # Kidscreen-52 ##########
      # **************************
      #Reverse scoring
      if(length(questions) != 52) stop("There must be 52 questions to calculate KIDSCREEN-52.")
      d <- df[,c(id,questions)]
      d[,questions] <- lapply(d[,questions],as.numeric)
      d[,questions[c(1,12:18,21:23,50:52)]] <- lapply(d[,questions[c(1,12:18,21:23,50:52)]],
                                                      FUN=function(x){dplyr::recode(x, `1`=5,`2`=4,`3`=2,`4`=3,`5`=1,
                                                                                    .default = NA_real_)})
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
         o <- cbind(d[[id]],data.frame(o))
         colnames(o)[1] <- id
      }else{
         stop("For Kidscreen-52 'setting' must be either 'proxy' or 'self'")
      }
   }else if(scale == "SF-36"){
      # **************************
      # SF-36 ####################
      # **************************
      if(length(questions) != 36) stop("There must be 36 questions to calculate SF-36")
      d <- df[,c(id,questions)]
      d[,questions] <- lapply(d[,questions],as.numeric)

      # Calculate normative values
      recoding_rules <- list(
         list(indices=c(1,2,20,22,34,36), mapping=setNames(c(4:0/4*100),c(1:5))),
         list(indices=c(3,4,5,6,7,8,9,10,11,12), mapping=setNames(c(0:2/2*100),c(1:3))),
         list(indices=c(13,14,15,16,17,18,19), mapping=setNames(c(0:1/1*100),c(1:2))),
         list(indices=c(21,23,26,27,30), mapping=setNames(c(5:0/5*100),c(1:6))),
         list(indices = c(24,25,28,29,31), mapping=setNames(c(0:5/5*100),c(1:6))),
         list(indices = c(32,33,35), mapping=setNames(c(0:4/4*100),c(1:5)))
      )
      for (rule in recoding_rules) {
         d[,questions[rule$indices]] <- lapply(d[,questions[rule$indices]],
                                               function(x) dplyr::recode(x, !!!rule$mapping))
      }

      # Calculate domains
      domains <- list(
         # https://www.researchgate.net/profile/John-Ware-6/publication/292390260_SF-36_Physical_and_Mental_Health_Summary_Scales_a_User%27s_Manual/links/5af580264585157136caee31/SF-36-Physical-and-Mental-Health-Summary-Scales-a-Users-Manual.pdf
         `sf36_pf`=c(3,4,5,6,7,8,9,10,11,12),
         `sf36_rp`=c(13,14,15,16),
         `sf36_re`=c(17,18,19),
         `sf36_vt`=c(23,27,29,31),
         `sf36_mh`=c(24,25,26,28,30),
         `sf36_sf`=c(20,32),
         `sf36_bp`=c(21,22),
         `sf36_gh`=c(1,33,34,35,36)
      )
      for(i in 1:length(domains)){
         # Calculate values
         d[[names(domains)[i]]] <- rowMeans(d[,questions[domains[[i]]]],na.rm=T)
         # Ensure more than 50% of the questions has been answered
         d[[names(domains)[i]]][rowSums(is.na(d[,questions[domains[[i]]]])) > length(domains[[i]])/2] <- NA
      }

      d[["sf36_pf_z"]] <- (d[["sf36_pf"]]-84.52404)/22.89490
      d[["sf36_rp_z"]] <- (d[["sf36_rp"]]-81.19907)/33.78729
      d[["sf36_bp_z"]] <- (d[["sf36_bp"]]-75.49196)/23.55879
      d[["sf36_gh_z"]] <- (d[["sf36_gh"]]-72.21316)/20.16964
      d[["sf36_vt_z"]] <- (d[["sf36_vt"]]-61.05453)/20.86942
      d[["sf36_sf_z"]] <- (d[["sf36_sf"]]-83.59753)/22.37642
      d[["sf36_re_z"]] <- (d[["sf36_re"]]-81.29467)/33.02717
      d[["sf36_mh_z"]] <- (d[["sf36_mh"]]-74.84212)/18.01189

      d[["sf36_pcs_z"]] <-
         (d[["sf36_pf_z"]]*0.42402)+(d[["sf36_rp_z"]]*0.35119)+
         (d[["sf36_bp_z"]]*0.31754)+(d[["sf36_gh_z"]]*0.24954)+
         (d[["sf36_vt_z"]]*0.02877)+(d[["sf36_sf_z"]]*-0.00753)+
         (d[["sf36_re_z"]]*-0.19206)+(d[["sf36_pf_z"]]*-0.22069)

      d[["sf36_mcs_z"]] <-
         (d[["sf36_pf_z"]]*-0.22999)+(d[["sf36_rp_z"]]*-0.12329)+
         (d[["sf36_bp_z"]]*-0.09731)+(d[["sf36_gh_z"]]*-0.01571)+
         (d[["sf36_vt_z"]]*0.23534)+(d[["sf36_sf_z"]]*0.26876)+
         (d[["sf36_re_z"]]*0.43407)+(d[["sf36_pf_z"]]*0.48581)

      d[["sf36_pcs"]] <- 50+(d[["sf36_pcs_z"]]*10)
      d[["sf36_mcs"]] <- 50+(d[["sf36_mcs_z"]]*10)


      # Create output
      o <- d[,!(colnames(d) %in% questions)]

   }

   # **************************
   # LONG NAMES ##########
   # **************************
   # if(long.names){
   nmz <- c(`afeq_exp`="AFEQ Experience of being a parent",
            `afeq_fl`="AFEQ Family Life",
            `afeq_cdus`="AFEQ Child Development, Understanding and Social Relationships",
            `afeq_cs`="AFEQ Child Symptoms",
            `afeq_tot`="AFEQ totalscore",

            `cbcl_tot`="CBCL totalscore",
            `cbcl_int`="CBCL internaliserende score",
            `cbcl_ext`="CBCL eksternaliserende score",
            `cbcl_aff`="CBCL affective problems",
            `cbcl_anx`="CBCL anxiety problem scale",
            `cbcl_asd`="CBCL pervasive developmental problem scale",
            `cbcl_adhd`="CBCL att.def/hyperact. problem scale",
            `cbcl_odd`="CBCL oppositional defiant problem scale",
            `cbcl_sleep`="CBCL sleep problem scale",

            `mpca_tot`="MPCA totalscore",
            `mpca_mean`="MPCA meanscore",

            `pedsql4_tot`="PedsQL4 totalscore",
            `peqsql4_psysoc`="PedsQL4 psychosocial score",
            `peqsql4_phys`="PedsQL4 physical score",

            `prfq_pm`="PRFQ Pre-Mentalizing Modes",
            `prfq_cm`="PRFQ Certainty about Mental States",
            `prfq_ic`="PRFQ Interest and Curiosity in Mental States",

            `whoqolbref_oa`="WHOQOL-BREF Overall QoL and general health",
            `whoqolbref_phys`="WHOQOL-BREF Physical health",
            `whoqolbref_psych`="WHOQOL-BREF Psychological",
            `whoqolbref_soc`="WHOQOL-BREF Social relationships",
            `whoqolbref_env`="WHOQOL-BREF Environment",

            `sf36_pf`="Physical functioning",
            `sf36_rp`="Role limitations due to physical health",
            `sf36_bp`="Pain",
            `sf36_gh`="General health",
            `sf36_vt`="Energy/fatigue",
            `sf36_sf`="Social functioning",
            `sf36_re`="Role limitations due to emotional problems",
            `sf36_mh`="Emotional well-being",
            `sf36_pcs`="Physical Component Score",
            `sf36_mcs`="Mental Component Score"


   )

   # }

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


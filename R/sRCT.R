# ==== DOCUMENTATION ====

#' simulated Randomised Clinical Trial (sRCT)
#'
#' `sRCT()` is a function which simulates a randomised clinical trial with a binary outcome and returns a dataframe. This version is validated to be used for analysis of interaction in a factorial design.
#'
#' @name sRCT
#'
#' @usage sRCT(all_sizes = NULL, n_pop = 1000,
#' n_sites = 10, design = c(2,2,2),
#' rrr = c(0.05,0.05,0), outcome_risk = 0.492,
#' interaction = c(`1-2` = 0.05, `1-2-3` = -0.05),
#' site_re = 0.05)
#'
#' @param all_sizes Size of blocks in allocation table. If left empty the three lowest possible block sizes will be randomly assigned.
#' @param n_pop Number of participants included in the trial.
#' @param n_sites Number of sites
#' @param design Number of sites as a list where each element corresponds to an intervention and the number in the element is the number of groups. So for a 2x2 factorial design `c(2,2)` should be used.
#' @param rrr relative risk reduction for each intervention so for the abovementioned 2x2 factorial design with RRR of 0.05 and 0.10 we would use `c(0.05,0.10)`.
#' @param outcome_risk The baseline risk (probability in absolute percentage) of the dichotomous primary outcome.
#' @param interaction Interaction between interventions with a named list. If interaction exists between intervention 1 and 2 we would use `1-2 = 0.05`.
#' @param site_re The size of the random effect of site, default is `0.05`.
#'
#' @return Returns a dataframe with an individual participant data frame.
#'
#' @details The sRCT function is continuously being developed to answer specific questions in simulation studies. sRCT will be updated and tested for each specific question. For each update the function will be validated for the current purpose and all previous purposes. sRCT is not validated for all simulation studies
#'
#' @examples sRCT()
#'
#' @importFrom stats rbinom
#' @importFrom stats rnorm
#' @export
#
# ==== FUNCTION ====

# #ADD PERCENTAGE OF BLOCKS
# all_sizes = NULL;
# n_pop = 2000; #Antal inkluderede
# n_sites = 10; #Antal sites
# design = c(2,2,2); #No. Interventioner / grupper
# rrr = c(0.05,0.05,-.05); #RRR
# interaction = c(`1-2` = 0.05, `1-2-3` = -0.05);
# outcome_risk = 0.49

sRCT <- function(all_sizes = NULL, n_pop = 1000,
                 n_sites = 10, design = c(2,2,2),
                 rrr = c(0.05,0.05,0), outcome_risk = 0.492,
                 interaction = c(`1-2` = 0.05, `1-2-3` = -0.05),
                 site_re = 0.05){

   trial <- NULL

   #Generate site names ----
      site_id <- replicate(n_sites, paste0(paste(sample(LETTERS,5, replace=TRUE),collapse=""),paste(sample(c(0:9),2, replace=TRUE),collapse="")))

   #Generate site probability ----
      site_prop <- rnorm(n_sites,mean=10,sd=5)
      site_prop[site_prop < 0] <- 0
      while(any(round(site_prop,digits=2) == 0)){
         site_prop[round(site_prop,digits=2) == 0 ] <- 0.02
         site_prop <- site_prop/sum(site_prop)
      }

   #Generate blocks ----
      genBlock_tbl <- function(design){
         expand_grid_list <- NULL
         for(i in 1:length(design)){
            expand_grid_list <- c(expand_grid_list, list(0:(design[i]-1)))
         }
         block_tbl <- expand.grid(expand_grid_list)
         return(block_tbl)
      }
      block_tbl <- genBlock_tbl(design)

   #Generate participants ----
      #Allocate participants to trials
      trial$site <- sample(site_id,n_pop,replace=T,prob=site_prop)
      trial <- data.frame(trial)
      for(i in colnames(block_tbl)) trial[[i]] <- NA

      #Find or define allocation sizes
      if(is.null(all_sizes)) {
         all_sizes <- c(nrow(block_tbl),nrow(block_tbl)*2,nrow(block_tbl)*3)
      }

      #Allocate participants to interventions
      for(i in unique(trial$site)){
         tmp_n <- sum((trial$site == i)*1)
         all_size <- NULL
         while(sum(all_size) <= tmp_n){
            all_size <- c(all_size,sample(all_sizes,1))
         }
         alloc_seq <- NULL
         for(j in 1:length(all_size)){
            block <- do.call("rbind",
                             replicate(all_size[j]/nrow(block_tbl),
                                       block_tbl, simplify = FALSE))
            block <- data.frame(block[sample(c(1:nrow(block)),
                                                 nrow(block)),])
            alloc_seq <- rbind(alloc_seq,block)
         }
         trial[trial$site == i,colnames(alloc_seq)] <- alloc_seq[c(1:tmp_n),]

         #Define site specific outcome risk
         trial$rr_site[trial$site == i] <- rnorm(1,mean=log(outcome_risk),sd=site_re)
      }

      #Find RRR for each intervention
      for(i in c(1:(length(rrr)))){
         rrr_temp <- trial[[paste0("Var",i)]]*rrr[i]
         trial[[paste0("rr_intervention",i)]] <- log(1-replace(rrr_temp,is.na(rrr_temp*rrr[i]),0))
      }

      #Find RRR for each interaction TO ADD
      if(length(interaction) > 0){
         for(i in c(1:(length(interaction)))){
            tmp_setting <- names(interaction)[i]
            formel <- NULL
            for(j in unlist(unique(strsplit(tmp_setting,"-")))){
               formel <- paste0(formel,"trial[[\"Var",j,"\"]]*")
            }
            formel <- paste0(formel,"log(1-",interaction[i],")")
            trial[[paste0("rr_interaction",i)]] <- eval(parse(text = formel))

         }
      }

   #Generate outcome ----
      trial$outcome_prob <- exp(rowSums(trial[grepl("rr_",colnames(trial))]))
      trial$outcome <- lapply(trial$outcome_prob, function(x) sample(c(0,1),1,prob = c(1-x,x)))
      trial$outcome <- as.numeric(trial$outcome)

   return(trial)
}

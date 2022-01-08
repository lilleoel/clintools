#sRCT
#Todowith real data <-

# ==== DOCUMENTATION ====

#' simulated Randomised Clinical Trial (sRCT)
#'
#' `sRCT()` is a function which simulates a randomised clinical trial with a binary outome and returns a dataframe. This version is validated to be used for analysis of interaction in a factorial design.
#'
#' @name sRCT
#'
#' @usage sRCT(part_tbl = NULL, all_sizes = NULL,
#' n_pop = 100,n_sites = 1,design = c(2,2,2),
#' rrr = c(0.05,0.05,0), interaction = c(`1<-2` = 0.05, `1<>2` = -0.05),
#' strata_var = c("age","sex"), strata_site = T,
#' strata_risk = c(age=0.3,sex=0.5),
#' outcome_risk = 0.492)
#'
#' @param part_tbl Here a participation data frame should be imported. `[TODO: NOT FUNCTIONAL]`
#' @param all_sizes Size of blocks in allocation table. If left empty the three lowest possible block sizes will be randomly assigned.
#' @param n_pop Number of participants included in the trial.
#' @param n_sites Number of sites
#' @param design Number of sites as a list where each element corresponds to an intervention and the number in the element is the number of groups. So for a 2x2 factorial design `c(2,2)` should be used. `[TODO: THREE GROUPS]`
#' @param rrr relative risk reduction for each intervention so for the abovementioned 2x2 factorial desing with RRR of 0.05 and 0.10 we would use `c(0.05,0.10)`.
#' @param interaction Interaction between interventions with a named list. If intervention 2 increases the RRR of intervention 1 we would use `1<-2 = 0.05`.
#' @param strata_var Variable which would be used for stratification.
#' @param strata_site If randomisation should be stratified by site
#' @param strata_risk The frequency of a dichotomised strata. Named list where the name must correspond to a strata var.
#' @param outcome_risk The baseline risk of the dichotomous primary outcome.
#'
#' @return Returns a dataframe with an individual participant data frame.
#'
#' @details The sRCT function is continuously being developed to answer specific questions in simulation studies. sRCT will be updated and tested for each specific question. For each update the function will be validated for the current purpose and all previous purposes. sRCT is not validated for all simulation studies
#'
#' @examples sRCT()
#'
#' @importFrom stats rbinom
#' @export
#
# ==== FUNCTION ====

# #ADD PERCENTAGE OF BLOCKS
# part_tbl = NULL; all_sizes = NULL;
# n_pop = 2000; #Antal inkluderede
# n_sites = 10; #Antal sites
# design = c(2,2,2); #No. Interventioner / grupper
# rrr = c(0.05,0.05,-.05); #RRR
# interaction = c(`1<-2` = 0.05);
# strata_var = c("age","sex"); #Strata variables
# strata_site = T; #Stratify by site
# strata_risk = c(age=0.3,sex=0.5); #Name and Risk of strata
# outcome_risk = 0.49

sRCT <- function(part_tbl = NULL, all_sizes = NULL,
                 n_pop = 100, #Antal inkluderede
                 n_sites = 1, #Antal sites
                 design = c(2,2,2), #No. Interventioner / grupper
                 rrr = c(0.05,0.05,0), #RRR
                 interaction = c(`1<-2` = 0.05, `1<>2` = -0.05),
                 strata_var = c("age","sex"), #Strata variables
                 strata_site = T, #Stratify by site
                 strata_risk = c(age=0.3,sex=0.5), #Name and Risk of strata
                 outcome_risk = 0.492){
   #Generate sites ----
   if(length(n_sites) == 1){
      site_id <- replicate(n_sites, paste0(paste(sample(LETTERS,5, replace=TRUE),collapse=""),paste(sample(c(0:9),2, replace=TRUE),collapse="")))
   }else{
      site_id <- unique(part_tbl$site)
   }

   #Generate block ----
   genBlock_tbl <- function(design){
      expand_grid_list <- NULL
      for(i in 1:length(design)){
         expand_grid_list <- c(expand_grid_list, list(0:(design[i]-1)))
      }
      block_tbl <- expand.grid(expand_grid_list)
      return(block_tbl)
   }
   block_tbl <- genBlock_tbl(design)

   #Generate participants table ----
   genPart_tbl <- function(n_pop, site_id, strata_var, strata_risk, outcome_risk){
      part_tbl <- NULL
      if(!is.null(site_id)){
         part_tbl$site <- sample(site_id,n_pop,replace=TRUE)
      }
      if(!is.null(strata_var)){
         for(i in 1:length(strata_var)){
            part_tbl[[names(strata_risk[i])]] <- sample(0:1,n_pop,replace=T,prob=c(1-strata_risk[i],strata_risk[i]))
         }
      }
      part_tbl <- data.frame(part_tbl)
      part_tbl$outcome <- sample(0:1,nrow(part_tbl),replace=T,prob=c(outcome_risk,1-outcome_risk))
      return(part_tbl)
   }
   if(is.null(part_tbl)) part_tbl <- genPart_tbl(n_pop,site_id,strata_var,
                                                 strata_risk, outcome_risk)

   #Generate allocation table ----
   genAll_tbl <- function(n_pop, site_id, block_tbl,strata_site, strata_var,
                          all_sizes = NULL){
      if(is.null(all_sizes)) {
         all_sizes <- c(nrow(block_tbl),nrow(block_tbl)*2,nrow(block_tbl)*3)
      }

      #Function to generate allocation for one strata
      genAll_simple <- function(n_pop, block_tbl, all_sizes){
         results <- NULL
         k <- 0
         all_size <- NULL
         while(sum(all_size) <= n_pop){
            all_size <- c(all_size,sample(all_sizes,1))
         }
         for(j in 1:length(all_size)){
            block <- do.call("rbind",
                             replicate(all_size[j]/nrow(block_tbl),
                                       block_tbl, simplify = FALSE))
            block <- data.frame(block_tbl[sample(c(1:nrow(block_tbl)),
                                                 nrow(block_tbl)),])
            results <- rbind(results,block)
         }
         k <- nrow(results)
         colnames(results) <- paste0("Var", 1:ncol(results))
         return(results)
      }

      strata <- NULL
      for(i in strata_var){
         strata[[i]] <- c(0,1)
      }
      if(strata_site){
         strata[["site"]] <- site_id
      }
      strata <- data.frame(expand.grid(strata))
      all_tbl <- NULL
      for(i in 1:nrow(strata)){
         temp <- suppressWarnings(cbind(strata[i,],genAll_simple(n_pop,block_tbl,all_sizes)))
         colnames(temp)[1:ncol(strata)] <- colnames(strata)
         all_tbl <- rbind(all_tbl,temp)
      }
      return(all_tbl)
   }
   all_tbl <- genAll_tbl(n_pop, site_id, block_tbl,strata_site, strata_var,
                         all_sizes)

   #Calculate RRR ----
   for(i in 1:length(rrr)){
      col <- colnames(all_tbl)[grepl("Var",colnames(all_tbl))][i]
      all_tbl[[paste0("rrr",i)]] <- 0
      all_tbl[[paste0("rrr",i)]][all_tbl[[col]] == 1] <- rrr[i]
   }
   #Add interactions
   if(!is.null(interaction)){
      for(i in 1:length(interaction)){
         int_set <- names(interaction)[i]
         int_eff <- interaction[i]
         if(grepl("<-",int_set)){
            all_tbl[[paste0("rrr",substr(int_set,1,1))]][all_tbl[[paste0("Var",substr(int_set,4,4))]] == 1] <- all_tbl[[paste0("rrr",substr(int_set,1,1))]][all_tbl[[paste0("Var",substr(int_set,4,4))]] == 1]+all_tbl[[paste0("rrr",substr(int_set,1,1))]][all_tbl[[paste0("Var",substr(int_set,4,4))]] == 1]*int_eff
         }
         if(grepl("<>",int_set)){
            all_tbl[[paste0("rrr",substr(int_set,1,1))]][all_tbl[[paste0("Var",substr(int_set,4,4))]] == 1 & all_tbl[[paste0("Var",substr(int_set,1,1))]] == 1] <- all_tbl[[paste0("rrr",substr(int_set,1,1))]][all_tbl[[paste0("Var",substr(int_set,4,4))]] == 1 & all_tbl[[paste0("Var",substr(int_set,1,1))]] == 1]+all_tbl[[paste0("rrr",substr(int_set,1,1))]][all_tbl[[paste0("Var",substr(int_set,4,4))]] == 1 & all_tbl[[paste0("Var",substr(int_set,1,1))]] == 1]*int_eff
            all_tbl[[paste0("rrr",substr(int_set,4,4))]][all_tbl[[paste0("Var",substr(int_set,4,4))]] == 1 & all_tbl[[paste0("Var",substr(int_set,1,1))]] == 1] <- all_tbl[[paste0("rrr",substr(int_set,4,4))]][all_tbl[[paste0("Var",substr(int_set,4,4))]] == 1 & all_tbl[[paste0("Var",substr(int_set,1,1))]] == 1]+all_tbl[[paste0("rrr",substr(int_set,4,4))]][all_tbl[[paste0("Var",substr(int_set,4,4))]] == 1 & all_tbl[[paste0("Var",substr(int_set,1,1))]] == 1]*int_eff
         }
      }
   }
   all_tbl$rrr <- rowSums(all_tbl[,grepl("rrr",colnames(all_tbl))])


   #Include participants ----
   '%!in%' <- function(x,y)!('%in%'(x,y))
   trial <- part_tbl[sample(1:nrow(part_tbl),n_pop,replace=T),]
   colz <- colnames(trial)[colnames(trial) != "outcome"]
   temp <- NULL
   for(i in unique(interaction(trial[,colz]))){
      n_all <- sum((i == interaction(trial[,colz]))*1)
      test <- cbind(trial[interaction(trial[,colz]) == i,],all_tbl[interaction(all_tbl[,colz]) %in% i,][1:n_all,which(colnames(all_tbl) %!in% colz)])
      temp <- rbind(temp,test)
   }
   trial <- temp

   re_rrr <- trial$rrr[trial$rrr > 0 & trial$outcome == 1]
   re_rrr <- 1-re_rrr
   trial$outcome[trial$rrr > 0 & trial$outcome == 1] <- rbinom(length(re_rrr), size = 1, prob=re_rrr)

   re_rrr <- trial$rrr[trial$rrr < 0 & trial$outcome == 0]
   re_rrr <- abs(re_rrr)
   trial$outcome[trial$rrr < 0 & trial$outcome == 0] <- rbinom(length(re_rrr), size = 1, prob=re_rrr)

   return(trial)
}

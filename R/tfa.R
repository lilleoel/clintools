# ==== TFA ====



#1. RAW RECORDING
#DURING CYCLIC MEAN FUNCTION
Z.cyclic_mean <- function(df, variables){

   variables <- variables[variables != "hr"]

   #find peaks (use - to identify minimum)
   Z.find_peaks <- function (x, freq){
      m <- freq/2
      shape <- diff(sign(diff(x, na.pad = FALSE)))
      pks <- sapply(which(shape < 0), FUN = function(i){
         z <- i - m + 1
         z <- ifelse(z > 0, z, 1)
         w <- i + m + 1
         w <- ifelse(w < length(x), w, length(x))
         if(all(x[c(z : i, (i + 2) : w)] <= x[i + 1])) return(i + 1) else return(numeric(0))
      })
      pks <- unlist(pks)
      pks
   }

   #standardize Hz
   df <- Z.fast(df,freq,50)
   freq <- 50

   for(i in c(variables)){
      temp <- df
      temp$peaks <- 0
      temp$peaks[Z.find_peaks(-temp[[i]],freq)] <- temp[[i]][Z.find_peaks(-temp[[i]],freq)]
      temp$cycle <- cumsum(c(0,as.numeric(diff(temp$peaks))!=0))
      temp$cycle[temp$cycle %% 2 == 1] <- temp$cycle[temp$cycle %% 2 == 1] -1
      temp$cycle <- temp$cycle/2
      temp_mean <- aggregate(temp[[i]],by=list(temp$cycle),mean)
      colnames(temp_mean) <- c("cycle",paste0(i,"_mean"))
      temp_max <- aggregate(temp$t,by=list(temp$cycle),max)
      colnames(temp_max) <- c("cycle","t")
      temp_max <- temp_max[temp_max$t[c(2:nrow(temp_max))]-temp_max$t[c(1:(nrow(temp_max)-1))] > 0.25,]
      temp_results <- merge(temp_max,temp_mean,by="cycle")
      temp <- merge(temp,temp_results[,c("t",paste0(i,"_mean"))],by="t",all.x=T)
      df <- merge(df,temp[,c("t",paste0(i,"_mean"))],by="t")
      df[[paste0(i,"_mean")]] <- approx(df[[paste0(i,"_mean")]],
                                        xout = seq_along(df[[paste0(i,"_mean")]]))$y
   }
   return(df)
}
df <- Z.cyclic_mean(df,variables)
#INTERPOLATION
#Short periods of strong artefact (up to three beats) should be removed and replaced by linear interpolation.

#2. BEAT TO BEAT ----
setwd("C:/Oel/Artikler/PhD - Mxa - 2. package (clinmon)/TFA/CARNet_software")
df <- read.csv2("tfa_sample_data.txt", sep="\t")
df <- df[,c(1,2,3)]
colnames(df) <- c("t","abp_mean","mcav_mean")
df[] <- sapply(df, as.numeric)
library(ggplot2)
ggplot(data=df) + geom_line(aes(x=t,y=abp_mean))

ABP <- df$abp_mean
CBFV <- df$mcav_mean
freq <- 10
vlf = c(0.02,0.07); lf = c(0.07,0.2); hf = c(0.2,0.5);
detrend = 1; spectral_smoothing = 3;
coherence2_thresholds = cbind(c(3:15),c(0.51,0.40,0.34,0.29,0.25,0.22,0.20,0.18,0.17,0.15,0.14,0.13,0.12))
apply_coherence2_threshold = 1
remove_negative_phase = 1
remove_negative_phase_f_cutoff = 0.1
normalize_ABP = 0
normalize_CBFV = 0
window_type = 'hanning' #alternative BOXCAR
window_length = 102.4 #in s
overlap = 59.99
overlap_adjust = 1

tmep <- TFA(ABP,CBFV,freq=10,output="raw")



   library(ggplot2);
   g1 <- ggplot(data=tmep) + geom_line(aes(freq,gain)) + xlim(0,0.5) +
      ylab("Gain") + theme(axis.title.x = element_blank(),axis.text.x = element_blank())
   g2 <- ggplot(data=tmep) + geom_line(aes(f,phase)) + xlim(0,0.5) +
      ylab("Phase") + theme(axis.title.x = element_blank(),axis.text.x = element_blank())
   g3 <- ggplot(data=tmep) + geom_line(aes(f,coherence)) + xlim(0,0.5) +
      ylab("Coherence")
   gridExtra::grid.arrange(g1,g2,g3,ncol=1)

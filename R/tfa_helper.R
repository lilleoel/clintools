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
fs <- 10

#TFA_CAR 1-157 - WORKING ----
   params <- NULL
   params$vlf = c(0.02,0.07)
   params$lf = c(0.07,0.2)
   params$hf = c(0.2,0.5)
   params$detrend = 0
   params$spectral_smoothing = 3 # NORMALT 3
   params$coherence2_thresholds = cbind(c(3:15),c(0.51,0.40,0.34,0.29,0.25,0.22,0.20,0.18,0.17,0.15,0.14,0.13,0.12))
   params$apply_coherence2_threshold = 1
   params$remove_negative_phase = 1
   params$remove_negative_phase_f_cutoff = 0.1
   params$normalize_ABP = 0
   params$normalize_CBFV = 0
   params$window_type = 'hanning' #alternative BOXCAR
   params$window_length = 102.4 #in s
   params$overlap = 59.99
   params$overlap_adjust = 1
   params$plot = 1
   params$plot_f_range = c(0,0.5)
   params$plot_title=''

   #PLOT
   subplot1=c(.15,0.65,0.75,0.22)
   subplot2=cbind(subplot1,c(0,-0.25,0,0))
   subplot3=cbind(subplot2,c(0,-0.25,0,0))

   #STANDARDISE
   tfa_out <- NULL
   tfa_out$Mean_abp = mean(ABP);
   tfa_out$Std_abp=sd(ABP);

   if(params$detrend == 1){
      #Få det lavet uden en pakke
      ABP=pracma::detrend(ABP)
   }else{
      ABP=ABP-mean(ABP)
   }
   if(params$normalize_ABP==1){
      ABP=(ABP/tfa_out$Mean_abp)*100;
   }

   tfa_out$Mean_cbfv=mean(CBFV)
   tfa_out$Std_cbfv=sd(CBFV)

   if(params$detrend == 1){
      #Få det lavet uden en pakke
      CBFV=pracma::detrend(CBFV)
   }else{
      CBFV=CBFV-mean(CBFV)
   }
   if(params$normalize_ABP==1){
      CBFV=(CBFV/tfa_out$Mean_cbfv)*100;
   }

   #HANNING / BOXCAR
   window_length=round(params$window_length*fs)

   #Hanning function:
   hanning_car <- function(M){
      w <- (1-cos(2*pi*(c(0:(M-1))/M)))/2
      return(w)
   }

   #Boxcar
   boxcar  <- function(n)  {
      if (length(n) > 1 || n != floor(n) || n <= 0)
         stop("n must be an integer > 0")
      rep.int(1, n)
   }


   if(params$window_type == "hanning"){
      wind=hanning_car(window_length)
   }
   if(params$window_type == "boxcar"){
      wind=boxcar(window_length)
   }

   if(params$overlap_adjust==1){
      L=floor((length(ABP)-window_length)/(window_length*(1-params$overlap/100)))+1;
      if(L>1){
         shift=floor((length(ABP)-window_length)/(L-1));
         overlap=(window_length-shift)/window_length*100;
         tfa_out$overlap=overlap
      }
   }else{
         overlap=params$overlap;
   }

   overlap=overlap/100;
   M_smooth=params$spectral_smoothing;
   N_fft=window_length;

#TFA_CAR 158 - TFA1 ---
   x <- ABP
   y <- CBFV

   if(length(wind)==1){
      M=wind
      wind=boxcar(window_length)
   }else{
      M=length(wind);
   }
   Nfft=0;
   if(Nfft==0){
      Nfft=M;
   }

#TFA_CAR 158 - TFA1 49 - WELCH1 - WORKING ----
   window <- wind

   if(length(window)==1){
      M=window;
      window=boxcar(M)
   }else{
      M=length(window);
   }
   Nfft=M
   shift=round((1-overlap)*M);
   x=x
   y=y
   window=window
   N=length(x);

   X=fft(x[1:M]*window)
   Y=fft(y[1:M]*window)
   L=1;
   if(shift>0){
      i_start=1+shift;
      while(i_start+M-1 <= N){
         X=cbind(X,fft(x[i_start:(i_start+M-1)]*window,Nfft))
         Y=cbind(Y,fft(y[i_start:(i_start+M-1)]*window,Nfft))
         i_start=i_start+shift;
         L=L+1
      }
   }
   f=c(0:(Nfft-1))/Nfft*fs

   #ELEMTWISE MULTIPLICATION IN R
   #With 1 conj and one regular
   elementwise_multi <- function(X,Y){
      df.x.real <- Re(X)
      df.x.imag <- Im(X)
      df.x.imag <- cbind(df.x.imag[,1],df.x.imag[,c(2:ncol(df.x.imag))]*-1)
      df.y.real <- Re(Y)
      df.y.imag <- Im(Y)
      df.y.imag <- cbind(df.y.imag[,1],df.y.imag[,c(2:ncol(df.y.imag))]*-1)

      df.x <- NULL
      df.y <- NULL
      for(i in c(1:ncol(X))){
         df.x <- cbind(df.x,complex(real=df.x.real[,i],imaginary=df.x.imag[,i]))
         df.y <- cbind(df.y,complex(real=df.y.real[,i],imaginary=df.y.imag[,i]))
      }

      results <- df.y
      for(i in c(1:nrow(df.x))){
         for(j in c(1:ncol(df.x))){
            results[i,j] <- df.x[i,j]*df.y[i,j]
         }
      }

      return(results)

   }

   if(L==1){
      C.Pxx=(X*Conj(X))/L/sum(window^2)/fs
      C.Pyy=(Y*Conj(Y))/L/sum(window^2)/fs
      C.Pxy=(Conj(X)*Y)/L/sum(window^2)/fs
      C.coh=C.Pxy/((abs((C.Pxx*C.Pyy)))^0.5)
   }else{
      C.Pxx=as.numeric(rowSums(elementwise_multi(X,Conj(X)))/L/sum(window^2)/fs)
      C.Pyy=as.numeric(rowSums(elementwise_multi(Y,Conj(Y)))/L/sum(window^2)/fs)
      C.Pxy=rowSums(elementwise_multi(Conj(X),Y))/L/sum(window^2)/fs
      C.coh=C.Pxy/((abs((as.numeric(C.Pxx)*C.Pyy)))^0.5)
   }



#TFA_CAR 158 - TFA1 58 ----

   Pxx=C.Pxx
   Pyy=C.Pyy
   Pxy=C.Pxy

   if(M_smooth>1){
      h=rep(1,floor((M_smooth+1)/2))
      h=h/sum(h);
      Pxx1=Pxx;
      Pxx1[1]=Pxx[2]
      Pyy1=Pyy;
      Pyy1[1]=Pyy[2]
      Pxy1=Pxy;
      Pxy1[1]=Pxy[2]

      #PROBLEM MED FILTFILT
      Pxx1=signal::filtfilt(h,1,Pxx1);
      Pyy1=signal::filtfilt(h,1,Pyy1);

      filter.complex=function(x){complex(real=signal::filtfilt(h,1,Re(x)), imag=signal::filtfilt(h,1,Im(x)))}
      Pxy1=filter.complex(Pxy1);

      Pxx1[1]=Pxx[1]
      Pxx=Pxx1
      Pyy1[1]=Pyy[1]
      Pyy=Pyy1
      Pxy1[1]=Pxy[1]
      Pxy=Pxy1
   }
   H=Pxy/Pxx
   C=Pxy/(abs(Pxx*Pyy)^0.5)

#TFA_CAR 165-190 FILTER PROBLEM ----
   tfa_out$H=H;
   tfa_out$C=C;
   tfa_out$f=f;
   tfa_out$Pxx=Pxx;
   tfa_out$Pyy=Pyy;
   tfa_out$Pxy=Pxy;
   tfa_out$No_windows=L;
   no_windows <- tfa_out$No_windows

   i=which(params$coherence2_thresholds[,1] %in% no_windows)
   if(!is.numeric(i)){
      disp('Warning:no coherence threshold defined for the number of windows obtained - all frequencies will be included')
      coherence2_threshold=0;
   }else{
      coherence2_threshold=params$coherence2_thresholds[i,2];
   }

   G=H;

   if(params$apply_coherence2_threshold == 1){
      i=which(abs(C)^2 < coherence2_threshold)
      H[i]=NA;
   }

#TFA_CAR 191- ----

   #HER VED ANGLE DER ER ENDNU ET PROBLEM
   P=atan2(Im(H), Re(H))

   #HER VED ANGLE DER ER ENDNU ET PROBLEM
   if(params$remove_negative_phase){
      n=which(f<params$remove_negative_phase_f_cutoff)
      k=which(P[n]<0)
      if(length(k) != 0){
         P[n[k]]=NA
      }
   }

   i=which(f>=params$vlf[1] & f<params$vlf[2])

   tfa_out$Gain_vlf=mean(abs(H[i]),na.rm=T) #FEJL
   tfa_out$Phase_vlf=mean(P[i],na.rm=T)/(2*pi)*360; #FEJL
   tfa_out$Coh2_vlf=mean(abs(C[i])^2, na.rm=T) #FEJL
   tfa_out$P_abp_vlf=2*sum(Pxx[i])*f[2]; #KORREKT
   tfa_out$P_cbfv_vlf=2*sum(Pyy[i])*f[2]; #KORREKT

   i=which(f>=params$lf[1] & f<params$lf[2])
   tfa_out$Gain_lf=mean(abs(H[i]),na.rm=T);
   tfa_out$Phase_lf=mean(P[i],na.rm=T)/(2*pi)*360;
   tfa_out$Coh2_lf=mean(abs(C[i])^2,na.rm=T);
   tfa_out$P_abp_lf=2*sum(Pxx[i])*f[2];
   tfa_out$P_cbfv_lf=2*sum(Pyy[i])*f[2];

   i=which(f>=params$hf[1] & f<params$hf[2]);
   tfa_out$Gain_hf=mean(abs(H[i]),na.rm=T);
   tfa_out$Phase_hf=mean(P[i],na.rm=T)/(2*pi)*360;
   tfa_out$Coh2_hf=mean(abs(C[i])^2,na.rm=T);
   tfa_out$P_abp_hf=2*sum(Pxx[i])*f[2];
   tfa_out$P_cbfv_hf=2*sum(Pyy[i])*f[2];

   if(params$normalize_CBFV){
      tfa_out$Gain_vlf_norm=tfa_out$Gain_vlf;
      tfa_out$Gain_lf_norm=tfa_out$Gain_lf;
      tfa_out$Gain_hf_norm=tfa_out$Gain_hf;
      tfa_out$Gain_vlf_not_norm=tfa_out$Gain_vlf*tfa_out$Mean_cbfv/100;
      tfa_out$Gain_lf_not_norm=tfa_out$Gain_lf*tfa_out$Mean_cbfv/100;
      tfa_out$Gain_hf_not_norm=tfa_out$Gain_hf*tfa_out$Mean_cbfv/100;
   }else{
      tfa_out$Gain_vlf_not_norm=tfa_out$Gain_vlf;
      tfa_out$Gain_lf_not_norm=tfa_out$Gain_lf;
      tfa_out$Gain_hf_not_norm=tfa_out$Gain_hf;
      tfa_out$Gain_vlf_norm=tfa_out$Gain_vlf/tfa_out$Mean_cbfv*100;
      tfa_out$Gain_lf_norm=tfa_out$Gain_lf/tfa_out$Mean_cbfv*100;
      tfa_out$Gain_hf_norm=tfa_out$Gain_hf/tfa_out$Mean_cbfv*100;
   }

   if(params$plot){
      library(ggplot2);
      g1 <- ggplot() + geom_line(aes(f,abs(G))) + xlim(0,0.5) +
         ylab("Gain") + theme(axis.title.x = element_blank(),axis.text.x = element_blank())
      g2 <- ggplot() + geom_line(aes(f,atan2(Im(G), Re(G))/(2*pi)*360)) + xlim(0,0.5) +
         ylab("Phase") + theme(axis.title.x = element_blank(),axis.text.x = element_blank())
      g3 <- ggplot() + geom_line(aes(f,abs(C)^2)) + xlim(0,0.5) +
         ylab("Coherence")
      gridExtra::grid.arrange(g1,g2,g3,ncol=1)
   }



   results <- round(rbind(
      cbind(tfa_out$P_abp_vlf,tfa_out$P_abp_lf,tfa_out$P_abp_hf),
      cbind(tfa_out$P_cbfv_vlf,tfa_out$P_cbfv_lf,tfa_out$P_cbfv_hf),
      cbind(tfa_out$Coh2_vlf,tfa_out$Coh2_lf,tfa_out$Coh2_hf),
      cbind(tfa_out$Gain_vlf_not_norm,tfa_out$Gain_lf_not_norm,tfa_out$Gain_hf_not_norm),
      cbind(tfa_out$Gain_vlf_norm,tfa_out$Gain_lf_norm,tfa_out$Gain_hf_norm),
      cbind(tfa_out$Phase_vlf,tfa_out$Phase_lf,tfa_out$Phase_hf)
      ),digits=2)

   colnames(results) <- c("VLF","LF","HF")
   rownames(results) <- c("BP power","CBFV power","Coherence", "Gain (not norm)", "Gain (norm)","Phase")




























#TFA ----

plot.frequency.spectrum <- function(X.k, xlimits=c(0,length(X.k))) {
   plot.data  <- cbind(0:(length(X.k)-1), Mod(X.k))

   # TODO: why this scaling is necessary?
   plot.data[2:length(X.k),2] <- 2*plot.data[2:length(X.k),2]

   plot(plot.data, t="h", lwd=2, main="",
        xlab="Frequency (Hz)", ylab="Strength",
        xlim=xlimits, ylim=c(0,max(Mod(plot.data[,2]))))
}

# Plot the i-th harmonic
# Xk: the frequencies computed by the FFt
#  i: which harmonic
# ts: the sampling time points
# acq.freq: the acquisition rate
plot.harmonic <- function(Xk, i, ts, acq.freq, color="red") {
   Xk.h <- rep(0,length(Xk))
   Xk.h[i+1] <- Xk[i+1] # i-th harmonic
   harmonic.trajectory <- get.trajectory(Xk.h, ts, acq.freq=acq.freq)
   points(ts, harmonic.trajectory, type="l", col=color)
}

X.k <- fft(df$abp_mean)                   # find all harmonics with fft()
plot.frequency.spectrum(X.k, xlimits=c(0,0.45))



del<-1/100 # sampling interval
x.spec <- spectrum(df[,c(2,3)],log="no",span=10,plot=FALSE)
spx <- x.spec$freq/del
spy <- x.spec$spec[,2]
# coh <- COHERENCE
plot(spy~spx,xlab="frequency",ylab="spectral density",type="l",xlim=c(0,0.45))
temp <- as.data.frame(cbind(c(spx),c(spy)))
mean(temp$V2[temp$V1 >= 0.02 & temp$V1 <= 0.07])
mean(temp$V2[temp$V1 >= 0.07 & temp$V1 <= 0.20])
mean(temp$V2[temp$V1 > 0.20 & temp$V1 < 0.50])

spx <- x.spec$freq
spy <- x.spec$spec[,1]
spy <- x.spec$spec[,2]

# spec <- power
plot(spy~spx,xlab="frequency",ylab="spectral density",type="l",xlim=c(0,0.45))


#TEST PART
setwd("C:/Oel/Artikler/PhD - Mxa - 2. package (clinmon)/TFA/CARNet_software")
df <- read.csv("tfa_sample_data.txt", sep="\t")
df <- df[,c(1:3)]
colnames(df) <- c("t", "nabp", "tcd")
freq <- 10

df <- data1000
df <- df[,c(1:3)]
freq <- 1000
df$peak_n <- 0
df$peak_n[Z.find_peaks(-df$nabp, m = freq)] <- 1


ggplot(df, aes(x=t,y=tcd_mean)) + geom_line()


#FFT
plot(IRISSeismic::crossSpectrum(df[,c("t","mcav")],[,c("freq","phase")],type="l")

spy <- x.spec$phase
# spec <- phase
plot(spy~spx,xlab="frequency",ylab="spectral density",type="l",xlim=c(0,0.45))

spy <- x.spec$spec[,1]/x.spec$spec[,2]
# spec <- phase
plot(spy~spx,xlab="frequency",ylab="spectral density",type="l",xlim=c(0,0.45))




df$peak_n <- NA
df$peak_n[find_peaks(-df$nabp)] <- df$peak_n[find_peaks(-df$nabp)]
df$cycle <- cumsum(c(0,as.numeric(diff(df$peak_n))!=0))
df$cycle[df$cycle %% 2 == 1] <- df$cycle[df$cycle %% 2 == 1] -1
df$cycle <- df$cycle/2
temp <- aggregate(df$tcd,by=list(df$cycle),mean)
colnames(temp) <- c("cycle","cyclic_mean2")
df <- merge(df,temp,by="cycle")


del<-0.1 # sampling interval
x.spec <- spectrum(df$cyclic_mean,log="no",span=10,plot=FALSE)
spx <- x.spec$freq/del
spy <- 2*x.spec$spec
plot(spy~spx,xlab="frequency",ylab="spectral density",type="l",xlim=c(0,0.40))

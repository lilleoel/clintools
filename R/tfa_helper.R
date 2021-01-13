#Hanning function:
Z.hanning_car <- function(M){
   w <- (1-cos(2*pi*(c(0:(M-1))/M)))/2
   return(w)
}

#Boxcar
Z.boxcar  <- function(n)  {
   if (length(n) > 1 || n != floor(n) || n <= 0)
      stop("n must be an integer > 0")
   rep.int(1, n)
}

#DETREND

Z.detrend <- function(x, tt = 'linear', bp = c()) {
   if (!is.numeric(x) && !is.complex(x))
      stop("'x' must be a numeric or complex vector or matrix.")
   trendType <- pmatch(tt, c('constant', 'linear'), nomatch = 0)

   if (is.vector(x))
      x <- as.matrix(x)
   n <- nrow(x)
   if (length(bp) > 0 && !all(bp %in% 1:n))
      stop("Breakpoints 'bp' must elements of 1:length(x).")

   if (trendType == 1) {  # 'constant'
      if (!is.null(bp))
         warning("Breakpoints not used for 'constant' trend type.")
      y <- x - matrix(1, n, 1) %*% apply(x, 2, mean)

   } else if (trendType == 2) {  # 'linear'
      bp <- sort(unique(c(0, c(bp), n-1)))
      lb <- length(bp) - 1

      a <- cbind(matrix(0, n, lb), matrix(1, n, 1))
      for (kb in 1:lb) {
         m <- n - bp[kb]
         a[(1:m) + bp[kb], kb] <- as.matrix(1:m)/m
      }
      y <- x - a %*% qr.solve(a, x)

   } else {
      stop("Trend type 'tt' must be 'constant' or 'linear'.")
   }

   return(y)
}

#ELEMTWISE MULTIPLICATION IN R
#With 1 conj and one regular
Z.elementwise_multi <- function(X,Y){
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

#FILTERFILTER COMPLEX NUMBERS
Z.filter.complex=function(x){complex(real=signal::filtfilt(h,1,Re(x)), imag=signal::filtfilt(h,1,Im(x)))}

#TFA FUNCTION
TFA <- function(ABP, CBFV, freq, output = "table",
   vlf = c(0.02,0.07), lf = c(0.07,0.2), hf = c(0.2,0.5),
   detrend = 1, spectral_smoothing = 3,
   coherence2_threshold = cbind(c(3:15),c(0.51,0.40,0.34,0.29,0.25,0.22,0.20,0.18,0.17,0.15,0.14,0.13,0.12)),
   apply_coherence2_threshold = 1,
   remove_negative_phase = 1,
   remove_negative_phase_f_cutoff = 0.1,
   normalize_ABP = 0,
   normalize_CBFV = 0,
   window_type = 'hanning', #alternative BOXCAR
   window_length = 102.4, #in s
   overlap = 59.99,
   overlap_adjust = 1

){

   #Adjust ABP and CBFV ----
   output_var <- NULL
   output_var$abp_mean <- mean(ABP)
   output_var$abp_sd <- sd(ABP)
   output_var$cbfv_mean <- mean(CBFV)
   output_var$cbfv_sd <- sd(CBFV)

   if(detrend == 1){
      ABP <- Z.detrend(ABP)
      CBFV <- Z.detrend(CBFV)
   }else{
      ABP <- ABP-mean(ABP, na.rm=T)
      CBFV <- CBFV-mean(CBFV)
   }
   if(normalize_ABP == 1) ABP <- (ABP/output_var$abp_mean)*100
   if(normalize_CBFV == 1) CBFV <- (CBFV/output_var$cbfv_mean)*100

   #HANNING / BOXCAR ----
   window_l <- round(window_length*freq)
   if(window_type == "hanning") window <- Z.hanning_car(window_l)
   if(window_type == "boxcar") window <- Z.boxcar(window_l)

   #Overlapping ----
   if(overlap_adjust == 1){
      L <- floor((length(ABP)-window_l)/(window_l*(1-overlap/100)))+1
      if(L>1){
         shift <- floor((length(ABP)-window_l)/(L-1))
         overlap <- (window_l-shift)/window_l*100;
         output_var$overlap <- overlap
      }
   }else{
         overlap=overlap;
   }

   overlap <- overlap/100;

   #FFT ----
   M_smooth <- spectral_smoothing

   if(length(window) == 1){
      M <- window
      window <- boxcar(window_l)
   }else{
      M <- length(window)
   }

   shift <- round((1-overlap)*M)
   N <- length(ABP)

   X <- fft(ABP[1:M]*window)
   Y <- fft(CBFV[1:M]*window)
   L <- 1
   if(shift > 0){
      i_start <- 1+shift;
      while(i_start+M-1 <= N){
         X <- cbind(X,fft(ABP[i_start:(i_start+M-1)]*window,M))
         Y <- cbind(Y,fft(CBFV[i_start:(i_start+M-1)]*window,M))
         i_start <- i_start+shift;
         L <- L+1
      }
   }
   f <- c(0:(M-1))/M*freq

   #Phase/Gain/Coherence ----
   if(L == 1){
      Pxx <- as.numeric(X*Conj(X)/L/sum(window^2)/freq)
      Pyy <- as.numeric(Y*Conj(Y)/L/sum(window^2)/freq)
      Pxy <- Conj(X)*Y/L/sum(window^2)/freq
      coh <- Pxy/(abs(Pxx*Pyy))^0.5
   }else{
      Pxx <- as.numeric(rowSums(Z.elementwise_multi(X,Conj(X)))/L/sum(window^2)/freq)
      Pyy <- as.numeric(rowSums(Z.elementwise_multi(Y,Conj(Y)))/L/sum(window^2)/freq)
      Pxy <- rowSums(Z.elementwise_multi(Conj(X),Y))/L/sum(window^2)/freq
      coh <- Pxy/(abs(Pxx*Pyy))^0.5
   }

   if(M_smooth > 1){
      h <- rep(1,floor((M_smooth+1)/2))
      h <- h/sum(h)
      Pxx1 <- Pxx
      Pxx1[1] <- Pxx[2]
      Pyy1 <- Pyy
      Pyy1[1] <- Pyy[2]
      Pxy1 <- Pxy
      Pxy1[1] <- Pxy[2]

      Pxx1 <- signal::filtfilt(h,1,Pxx1)
      Pyy1 <- signal::filtfilt(h,1,Pyy1)
      Pxy1 <- Z.filter.complex(Pxy1)

      Pxx1[1] <- Pxx[1]
      Pxx <- Pxx1
      Pyy1[1] <- Pyy[1]
      Pyy <- Pyy1
      Pxy1[1] <- Pxy[1]
      Pxy <- Pxy1
   }
   H <- Pxy/Pxx
   C <- Pxy/(abs(Pxx*Pyy)^0.5)

   #Output and quality control ----
   output_var$H <- H
   output_var$C <- C
   output_var$f <- f
   output_var$Pxx <- Pxx
   output_var$Pyy <- Pyy
   output_var$Pxy <- Pxy
   output_var$no_windows <- L

   i=which(coherence2_thresholds[,1] %in% L)
   if(!is.numeric(i)){
      warning('No coherence threshold defined for the number of windows obtained - all frequencies will be included')
      coherence2_threshold=0;
   }else{
      coherence2_threshold <- coherence2_thresholds[i,2];
   }

   if(apply_coherence2_threshold == 1){
      i <- which(abs(C)^2 < coherence2_threshold)
      H[i] <- NA
   }

   P <- atan2(Im(H), Re(H))

   if(params$remove_negative_phase){
      n=which(f<params$remove_negative_phase_f_cutoff)
      k=which(P[n]<0)
      if(length(k) != 0){
         P[n[k]]=NA
      }
   }

   i <- which(f >= vlf[1] & f < vlf[2])
   output_var$vlf_gain <- mean(abs(H[i]),na.rm=T)
   output_var$vlf_phase <- mean(P[i],na.rm=T)/(2*pi)*360
   output_var$vlf_coh2 <- mean(abs(C[i])^2, na.rm=T)
   output_var$vlf_p_abp <- 2*sum(Pxx[i])*f[2]
   output_var$vlf_p_cbfv <- 2*sum(Pyy[i])*f[2]

   i <- which(f >= params$lf[1] & f < params$lf[2])
   output_var$lf_gain <- mean(abs(H[i]),na.rm=T)
   output_var$lf_phase <- mean(P[i],na.rm=T)/(2*pi)*360
   output_var$lf_coh2 <- mean(abs(C[i])^2, na.rm=T)
   output_var$lf_p_abp <- 2*sum(Pxx[i])*f[2]
   output_var$lf_p_cbfv <- 2*sum(Pyy[i])*f[2]

   i <- which(f >= params$hf[1] & f < params$hf[2])
   output_var$hf_gain <- mean(abs(H[i]),na.rm=T)
   output_var$hf_phase <- mean(P[i],na.rm=T)/(2*pi)*360
   output_var$hf_coh2 <- mean(abs(C[i])^2, na.rm=T)
   output_var$hf_p_abp <- 2*sum(Pxx[i])*f[2]
   output_var$hf_p_cbfv <- 2*sum(Pyy[i])*f[2]

   if(normalize_CBFV != 0){
      output_var$vlf_gain_norm <- output_var$vlf_gain
      output_var$lf_gain_norm <- output_var$lf_gain
      output_var$hf_gain_norm <- output_var$hf_gain
      output_var$vlf_gain_not_norm <- output_var$vlf_gain*output_var$cbfv_mean/100
      output_var$lf_gain_not_norm <- output_var$lf_gain*output_var$cbfv_mean/100
      output_var$hf_gain_not_norm <- output_var$hf_gain*output_var$cbfv_mean/100
   }else{
      output_var$vlf_gain_not_norm <- output_var$vlf_gain
      output_var$lf_gain_not_norm <- output_var$lf_gain
      output_var$hf_gain_not_norm <- output_var$hf_gain
      output_var$vlf_gain_norm <- output_var$vlf_gain/output_var$cbfv_mean*100
      output_var$lf_gain_norm <- output_var$lf_gain/output_var$cbfv_mean*100
      output_var$hf_gain_norm <- output_var$hf_gain/output_var$cbfv_mean*100
   }


   results <- round(rbind(
      cbind(output_var$vlf_p_abp,output_var$lf_p_abp,output_var$hf_p_abp),
      cbind(output_var$vlf_p_cbfv,output_var$lf_p_cbfv,output_var$hf_p_cbfv),
      cbind(output_var$vlf_coh2,output_var$lf_coh2,output_var$hf_coh2),
      cbind(output_var$vlf_gain_not_norm,output_var$lf_gain_not_norm,output_var$hf_gain_not_norm),
      cbind(output_var$vlf_gain_norm,output_var$lf_gain_norm,output_var$hf_gain_norm),
      cbind(output_var$vlf_phase,output_var$lf_phase,output_var$hf_phase)
   ),digits=2)

   colnames(results) <- c("VLF","LF","HF")
   rownames(results) <- c("BP power","CBFV power","Coherence", "Gain (not norm)", "Gain (norm)","Phase")
   results <- as.data.frame(results)

   #PLOT output ----
   plot <- cbind(f,abs(output_var$H),atan2(Im(output_var$H), Re(output_var$H))/(2*pi)*360,abs(C)^2)
   colnames(plot) <- c("freq","gain","phase","coherence")
   plot <- as.data.frame(plot)

   if(output == "raw") return(output_var)
   if(output == "table") return(results)
   if(output == "plot") return(plot)
}





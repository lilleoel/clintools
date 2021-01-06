Z.cyclic_mean <- function(df, freq){

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
   df <- Z.optimize(df,freq,50)
   freq <- 50

   for(i in c(2:ncol(df))){
      temp <- df
      temp$peaks <- 0
      temp$peaks[Z.find_peaks(-temp[[i]],freq)] <- temp$nabp[Z.find_peaks(-temp[[i]],freq)]
      temp$cycle <- cumsum(c(0,as.numeric(diff(temp$peaks))!=0))
      temp$cycle[temp$cycle %% 2 == 1] <- temp$cycle[temp$cycle %% 2 == 1] -1
      temp$cycle <- temp$cycle/2
      temp_mean <- aggregate(temp[[i]],by=list(temp$cycle),mean)
      colnames(temp_mean) <- c("cycle",paste0(colnames(df)[i],"_mean"))
      temp_max <- aggregate(temp$t,by=list(temp$cycle),max)
      colnames(temp_max) <- c("cycle","t")
      temp_results <- merge(temp_max,temp_mean,by="cycle")
      temp <- merge(temp,temp_results[,c("t",paste0(colnames(df)[i],"_mean"))],by="t",all.x=T)
      df <- merge(df,temp[,c("t",paste0(colnames(df)[i],"_mean"))],by="t")
      df[[paste0(colnames(df)[i],"_mean")]] <- approx(df[[paste0(colnames(df)[i],"_mean")]],
                                                      xout = seq_along(df[[paste0(colnames(df)[i],"_mean")]]))$y
   }




}

library(ggplot2)
ggplot(data=temp) + geom_line(aes(x=t,y=nabp), alpha=0.5) +
   geom_point(aes(x=t,y=peaks), color="red")

ggplot(data=df) + geom_line(aes(x=t,y=nabp), alpha=0.5) + geom_line(aes(x=t,y=interpol), color="blue") +
   geom_point(aes(x=t,y=nabp_mean), color="red")


n <- nrow(mydf)
with(mydf, data.frame(approx(date, value, xout = seq(date[1], date[n], "day"))))



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

del<-1/50 # sampling interval
x.spec <- spectrum(df[complete.cases(df),c("nabp_mean","tcd_mean")],log="no",span=10,plot=FALSE, method="pgram")
spx <- x.spec$freq/del
spy <- x.spec$coh
# coh <- COHERENCE
plot(spy~spx,xlab="frequency",ylab="spectral density",type="l",xlim=c(0,0.45))

temp <- as.data.frame(cbind(c(spx),c(spy)))
mean(temp$V2[temp$V1 >= 0.02 & temp$V1 <= 0.07])
mean(temp$V2[temp$V1 >= 0.07 & temp$V1 <= 0.20])
mean(temp$V2[temp$V1 > 0.20 & temp$V1 < 0.50])


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
















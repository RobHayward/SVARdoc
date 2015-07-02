# ACF3 for ACF and ACF4 for PACF
# acf3-----------------
acf3=function(series,max.lag=NULL){
  num=length(series)
  if (is.null(max.lag)) max.lag=ceiling(10+sqrt(num))
  if (max.lag > (num-1)) stop("Number of lags exceeds number of observations")
  ACF=acf(series, max.lag, plot=FALSE)$acf[-1]
  PACF=pacf(series, max.lag, plot=FALSE)$acf
  LAG=1:max.lag/frequency(series)
  minA=min(ACF)
  maxA=max(ACF)
  minP=min(PACF)
  maxP=max(PACF)
  U=2/sqrt(num)
  L=-U
  minu=min(minA,minP,L)-.01
  manu=max(maxA,maxP,U)+0.1
  old.par <- par(no.readonly = TRUE)
  #par(mfrow=c(2,1), mar = c(3,3,2,0.8),
  #    oma = c(1,1.2,1,1), mgp = c(1.5,0.6,0))
  plot(LAG, ACF, type="h",ylim=c(minu,manu), 
       main=paste("Series: ",deparse(substitute(series))))
  abline(h=c(0,L,U), lty=c(1,2,2), col=c(1,4,4))
  #plot(LAG, PACF, type="h",ylim=c(minu,manu))
  #abline(h=c(0,L,U), lty=c(1,2,2), col=c(1,4,4))
  # on.exit(par(old.par))  
  #ACF<-round(ACF,2); PACF<-round(PACF,2)    
  # return(cbind(ACF, PACF)) 
}
pdf("acf.pdf", paper= "a4", width = 9, height = 11, title = "acf")
par(mfcol=c(3,2), oma = c(0,0,0,0))
acf3(rv1CNB)
acf3(rv1CNE)
acf3(rv1COT)
acf3(rv1RTWI)
acf3(rv1SPREAD2)
acf3(rv1S1)
dev.off()
# acf4---------------------------------
pcf3=function(series,max.lag=NULL){
  num=length(series)
  if (is.null(max.lag)) max.lag=ceiling(10+sqrt(num))
  if (max.lag > (num-1)) stop("Number of lags exceeds number of observations")
  ACF=acf(series, max.lag, plot=FALSE)$acf[-1]
  PACF=pacf(series, max.lag, plot=FALSE)$acf
  LAG=1:max.lag/frequency(series)
  minA=min(ACF)
  maxA=max(ACF)
  minP=min(PACF)
  maxP=max(PACF)
  U=2/sqrt(num)
  L=-U
  minu=min(minA,minP,L)-.01
  manu=max(maxA,maxP,U)+0.1
  old.par <- par(no.readonly = TRUE)
  #par(mfrow=c(2,1), mar = c(3,3,2,0.8),
  #    oma = c(1,1.2,1,1), mgp = c(1.5,0.6,0))
  # plot(LAG, ACF, type="h",ylim=c(minu,manu), 
  #      main=paste("Series: ",deparse(substitute(series))))
  # abline(h=c(0,L,U), lty=c(1,2,2), col=c(1,4,4))
  plot(LAG, PACF, type="h",ylim=c(minu,manu))
  abline(h=c(0,L,U), lty=c(1,2,2), col=c(1,4,4))
   # on.exit(par(old.par))  
  #ACF<-round(ACF,2); PACF<-round(PACF,2)    
  # return(cbind(ACF, PACF)) 
}
pdf("pcf.pdf", paper= "a4r", width = 9, title = "pcf")
par(mfcol=c(2,3), oma = c(0,0,1,0))
pcf3(rv1CNB)
pcf3(rv1CNE)
pcf3(rv1COT)
pcf3(rv1RTWI)
pcf3(rv1SPREAD2)
pcf3(rv1S1)
dev.off()
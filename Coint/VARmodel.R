# This is tt
# ds selects the model 
head(da)
ds <- da[,c(3:7,9,11)]
# The following will add the updated bond variable. 
ds <- da[,c(13, 4:7, 9, 11)]
head(ds)
info<-VARselect(ds,lag.max=8,type='b')
info
#if you want turn list into dataframe and xtable for la tex
#info<-as.data.frame(info)
#xtable(info2)
#Deal with current account########################################################
#the three ways to deal with the lack of stationarity in the current account 
#The first ignores the problem, 
#The second will look at the change in the current account (though this is not 
#stationary itself.
#The third will remove the current account from the system.
#Each of these can be tried with and without the dummies.
# Model selection var1 and va2---------------------
# Two models var1 is original, var2 has spread 2 and S2. 
Var1<-VAR(ds,p=4, type='both',season=NULL, exog=dum)
logLik(Var1)
summary(Var1)
roots(Var1)
# Residual preparation#############################################
#Plot residuals
#First make residuals into data from the var and 
#the turn them into time series for plotting and testing. 
#It may be possible to go back from here to think about additional dummies. 
#Indeed, it seems as if there is a need for a dummy at the start of the series
#for the spread equation. 
resid1<-data.frame(resid(Var1))
head(resid1)
rv1CNB<-ts(resid1$CNBa, start=c(1986.1), frequency=4)
plot(rv1CNB, main="Residuals from CNB equation", ylab="Error")
hist(rv1CNB, main="Residuals from CNB equation and normal plot", ylab="Error", 
	xlab='CNB',prob=TRUE)
plot(function(x){dnorm(x,m=mean(rv1CNB),sd=sd(rv1CNB))},-40,30,add=TRUE, 
	col='red')
rv1CNE<-ts(resid1$CNE, start=c(1986.1), frequency=4)
plot(rv1CNE, main="Residuals from CNE equation", ylab="Error")
hist(rv1CNE, main="Residuals from CNE equation and normal plot", ylab="Error", 
	xlab='CNE',prob=TRUE)
plot(function(x){dnorm(x,m=mean(rv1CNE),sd=sd(rv1CNE))},-15,20,add=TRUE, 
	col='red')
rv1CNFDI<-ts(resid1$CNFDI, start=c(1986.1), frequency=4)
plot(rv1CNFDI, main="Residuals from CNFDI equation", ylab="Error")
hist(rv1CNFDI, main="Residuals from CFDI equation and normal plot", ylab="Error", 
	xlab= 'CNE',prob=TRUE)
plot(function(x){dnorm(x,m=mean(rv1CNFDI),sd=sd(rv1CNFDI))},-25,30,add=TRUE, 
	col='red')
rv1COT<-ts(resid1$COT, start=c(1986.1), frequency=4)
plot(rv1COT, main="Residuals from COT equation", ylab="Error")
hist(rv1COT, main="Residuals from COT equation and normal plot", ylab="Error", 
	xlab='COT',prob=TRUE)
plot(function(x){dnorm(x,m=mean(rv1COT),sd=sd(rv1COT))},-15,20,add=TRUE, 
	col='red')
rv1RTWI<-ts(resid1$RTWI, start=c(1986.1), frequency=4)
plot(rv1RTWI, main="Residuals from RTWI equation", ylab="Error")
hist(rv1RTWI, main="Residuals from RTWI equation and normal plot", ylab="Error", 
	xlab='CNE',prob=TRUE)
plot(function(x){dnorm(x,m=mean(rv1RTWI),sd=sd(rv1RTWI))},-5,5,add=TRUE, 
	col='red')
rv1SPREAD2<-ts(resid1$SPREAD2, start=c(1986.1), frequency=4)
plot(rv1SPREAD2, main="Residuals from SPREAD2 equation", ylab="Error")
hist(rv1SPREAD2, main="Residuals from SPREAD2 equation and normal plot", ylab="Error", 
	xlab='CNE',prob=TRUE)
plot(function(x){dnorm(x,m=mean(rv1SPREAD2),sd=sd(rv1SPREAD2))},-5,10,add=TRUE, 
	col='red')
rv1S1<-ts(resid1$S1, start=c(1986.1), frequency=4)
plot(rv1S1, main="Residuals from S1 equation", ylab="Error")
hist(rv1S1, main="Residuals from S1 equation and normal plot", ylab="Error", 
	xlab='S1',prob=TRUE)
plot(function(x){dnorm(x,m=mean(rv1S1),sd=sd(rv1S1))},-0.2,0.2, 
	col='red', main="Residuals from S1 equation and normal plot", 
	ylab="Error", xlab='S1')
hist(rv1S1, prob=TRUE, add=TRUE)
#The next step will be to plot the residual plots together. 
# Plot serial correlation and histo#############################################
#Plot time series of all the error terms
par(mfrow=c(3,2))
rv1CNB<-ts(resid1$CNB, start=c(1986.1), frequency=4)
plot(rv1CNB, main="Residuals from CNB equation", ylab="Error")
rv1CNE<-ts(resid1$CNE, start=c(1986.1), frequency=4)
plot(rv1CNE, main="Residuals from CNE equation", ylab="Error")
rv1CNFDI<-ts(resid1$CNFDI, start=c(1986.1), frequency=4)
# remove one to leave 6. 
#plot(rv1CNFDI, main="Residuals from CNFDI equation", ylab="Error")
#rv1COT<-ts(resid1$COT, start=c(1986.1), frequency=4)
plot(rv1COT, main="Residuals from COT equation", ylab="Error")
rv1RTWI<-ts(resid1$RTWI, start=c(1986.1), frequency=4)
plot(rv1RTWI, main="Residuals from RTWI equation", ylab="Error")
rv1SPREAD1<-ts(resid1$SPREAD1, start=c(1986.1), frequency=4)
plot(rv1SPREAD1, main="Residuals from SPREAD1 equation", ylab="Error")
rv1S1<-ts(resid1$S1, start=c(1986.1), frequency=4)
plot(rv1S1, main="Residuals from S1 equation", ylab="Error")
#  Plot histogram and normal plot of the residuals-------------
par(mfrow=c(3,2))
hist(rv1CNB, main="Residuals from CNB equation and normal plot", ylab="Error", 
	xlab='CNB',prob=TRUE)
plot(function(x){dnorm(x,m=mean(rv1CNB),sd=sd(rv1CNB))},-40,30,add=TRUE, 
	col='red')
hist(rv1CNE, main="Residuals from CNE equation and normal plot", ylab="Error", 
	xlab='CNE',prob=TRUE)
plot(function(x){dnorm(x,m=mean(rv1CNE),sd=sd(rv1CNE))},-15,20,add=TRUE, 
	col='red')
hist(rv1CNFDI, main="Residuals from CFDI equation and normal plot", ylab="Error", 
	xlab='CNE',prob=TRUE)
plot(function(x){dnorm(x,m=mean(rv1CNFDI),sd=sd(rv1CNFDI))},-25,30,add=TRUE, 
	col='red')
hist(rv1COT, main="Residuals from COT equation and normal plot", ylab="Error", 
	xlab='COT',prob=TRUE)
plot(function(x){dnorm(x,m=mean(rv1COT),sd=sd(rv1COT))},-15,20,add=TRUE, 
	col='red')
hist(rv1RTWI, main="Residuals from RTWI equation and normal plot", ylab="Error", 
	xlab='CNE',prob=TRUE)
plot(function(x){dnorm(x,m=mean(rv1RTWI),sd=sd(rv1RTWI))},-5,5,add=TRUE, 
	col='red')
hist(rv1SPREAD1, main="Residuals from SPREAD1 equation and normal plot", ylab="Error", 
	xlab='CNE',prob=TRUE)
plot(function(x){dnorm(x,m=mean(rv1SPREAD1),sd=sd(rv1SPREAD1))},-5,10,add=TRUE, 
	col='red')
hist(rv1S1, main="Residuals from S1 equation and normal plot", ylab="Error", 
	xlab='S1',prob=TRUE)
plot(function(x){dnorm(x,m=mean(rv1S1),sd=sd(rv1S1))},-0.2,0.2, 
	col='red', main="Residuals from S1 equation and normal plot", 
	ylab="Error", xlab='S1')
hist(rv1S1, prob=TRUE, add=TRUE)
###arf2 ####################################################################
#This is the acf2 function from Robert Shunway and D.S. Stoffer. 
#This has been modified to include maxA and maxP (copied).
#Next step is to get title to work better
acf2=function(series,max.lag=NULL){
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
  par(mfrow=c(2,1), mar = c(3,3,2,0.8),
    oma = c(1,1.2,1,1), mgp = c(1.5,0.6,0))
  plot(LAG, ACF, type="h",ylim=c(minu,manu), 
    main=paste("Series: ",deparse(substitute(series))))
    abline(h=c(0,L,U), lty=c(1,2,2), col=c(1,4,4))
  plot(LAG, PACF, type="h",ylim=c(minu,manu))
    abline(h=c(0,L,U), lty=c(1,2,2), col=c(1,4,4))
  on.exit(par(old.par))  
  ACF<-round(ACF,2); PACF<-round(PACF,2)    
  return(cbind(ACF, PACF)) 
  }
#Now add the residual series.   
acf2(rv1CNB)
acf2(rv1CNE)
acf2(rv1CNFDI)
acf2(rv1COT)
acf2(rv1SPREAD2)
acf2(rv1RTWI)
acf2(rv1S1)
########Serial test#############################################################
#test the null of no serial correlation.  Test lags and types
#BG is Breusch-Godfrey LM test
#ES is the Edgerton-Shukur F test
Var1.ser<-serial.test(Var1,lags.pt=8,type="BG")
Var1.ser
#test null of no arch---------------------
var1.arch<-arch.test(Var1,lags.multi=4, multivariate.only=FALSE)
var1.arch
#test normality---------------
var1.norm<-normality.test(Var1,multivariate.only=FALSE)
var1.norm
plot(var1.arch)

#Test Granger Causality--------------

#change the variable to get the grager causality
var1.cause<-causality(Var1, cause='RTWI')
var1.cause$Granger
var1.cause$Instant
# Impulse response functions----------------------
#the variables are changed to show the effect of a shock on each 
#impulse to the responding exchange rate

fevd.var6<-fevd(Var6)
plot(fevd.var6)
fevd.var6$CNB


stability(Var6)

#This is the end of the basic VAR analysis. 
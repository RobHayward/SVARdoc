# If data is not prepared
# source("Prepare.R")
head(da)
# THis will select the variables that are used. 
da <- da[,c(3:7,9,11)]
info<-VARselect(da,lag.max=8,type='t')
info
#if you want turn list into dataframe and xtable for la tex
#info<-as.data.frame(info)
#xtable(info2)
#Deal with current account
#the three ways to deal with the lack of stationarity in the current account 
#The first ignores the problem, 
#The second will look at the change in the current account (though this is not 
#stationary itself.
#The third will remove the current account from the system.
#Each of these can be tried with and without the dummies.
# Model selection var1 and va2
# Two models var1 is original, var2 has spread 2 and S2. 
Var1<-VAR(da,p=4, type='both',season=NULL, exog=dum)
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
#The next step will be to plot the residual plots together. 
# Plot serial history-----------------
#Plot time series of all the error terms
pdf("residts.pdf", paper= "a4", width = 9, height = 11, title = "residts")
par(mfcol=c(3,2), oma = c(0,0,0,0))
rv1CNB<-ts(resid1$CNB, start=c(1986.1), frequency=4)
plot(rv1CNB, main="Residuals from CNB equation", ylab="Error")
abline(h = 0, col = 'red')
rv1CNE<-ts(resid1$CNE, start=c(1986.1), frequency=4)
plot(rv1CNE, main="Residuals from CNE equation", ylab="Error")
abline(h = 0, col = 'red')
rv1CNFDI<-ts(resid1$CNFDI, start=c(1986.1), frequency=4)
# remove one to leave 6. 
#plot(rv1CNFDI, main="Residuals from CNFDI equation", ylab="Error")
rv1COT<-ts(resid1$COT, start=c(1986.1), frequency=4)
plot(rv1COT, main="Residuals from COT equation", ylab="Error")
abline(h = 0, col = 'red')
rv1RTWI<-ts(resid1$RTWI, start=c(1986.1), frequency=4)
plot(rv1RTWI, main="Residuals from RTWI equation", ylab="Error")
rv1SPREAD2<-ts(resid1$SPREAD2, start=c(1986.1), frequency=4)
abline(h = 0, col = 'red')
plot(rv1SPREAD2, main="Residuals from SPREAD2 equation", ylab="Error")
rv1S1<-ts(resid1$S1, start=c(1986.1), frequency=4)
abline(h = 0, col = 'red')
plot(rv1S1, main="Residuals from S1 equation", ylab="Error")
abline(h = 0, col = 'red')
dev.off()
#  Plot histogram and normal plot of the residuals-------------
pdf("Hresid.pdf", paper= "a4", width = 9, height = 11, title = "Hresid")
par(mfcol=c(3,2), oma = c(0,0,0,0))
hist(rv1CNB, main="CNB Residuals and normal plot", ylab="Error", 
	xlab='CNB',prob=TRUE)
plot(function(x){dnorm(x,m=mean(rv1CNB),sd=sd(rv1CNB))},-40,30,add=TRUE, 
	col='red')
hist(rv1CNE, main="CNE Residuals and normal plot", ylab="Error", 
	xlab='CNE',prob=TRUE)
plot(function(x){dnorm(x,m=mean(rv1CNE),sd=sd(rv1CNE))},-15,20,add=TRUE, 
	col='red')
hist(rv1CNFDI, main="CNFDI Residuals and normal plot", ylab="Error", 
  xlab='CNFDI',prob=TRUE)
plot(function(x){dnorm(x,m=mean(rv1CNFDI),sd=sd(rv1CNFDI))},-25,30,add=TRUE, 
	col='red')
hist(rv1COT, main="COT Residuals and normal plot", ylab="Error", 
	xlab='COT',prob=TRUE)
plot(function(x){dnorm(x,m=mean(rv1COT),sd=sd(rv1COT))},-15,20,add=TRUE, 
	col='red')
hist(rv1RTWI, main="RTWI Residuals and normal plot", ylab="Error", 
	xlab='RTWI',prob=TRUE)
plot(function(x){dnorm(x,m=mean(rv1RTWI),sd=sd(rv1RTWI))},-5,5,add=TRUE, 
	col='red')
hist(rv1SPREAD2, main="SPREAD2 Residuals and normal plot", ylab="Error", 
	xlab='SPREAD2',prob=TRUE)
plot(function(x){dnorm(x,m=mean(rv1SPREAD2),sd=sd(rv1SPREAD2))},-5,10,add=TRUE, 
	col='red')
#hist(rv1S1, main="Residuals from S1 equation and normal plot", ylab="Error", 
#	xlab='S1',prob=TRUE)
# plot(function(x){dnorm(x,m=mean(rv1S1),sd=sd(rv1S1))},-2,2, 
#	col='red', main = "S1 Residuals and normal plot", ylab = "Error") 
# hist(rv1S1, prob=TRUE, add=TRUE)
dev.off()
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
par(mfrow=c(3,2))
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
Var1.ser<-serial.test(Var1,lags.pt=8,type="PT.adjusted")
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
# Plot cumsum of coefficients---------------
pdf("stab.pdf", paper= "a4", width = 9, height = 11, title = "Stability test")
par(mfcol=c(3,2), oma = c(0,0,0,0))
# find functions
# names(stability(Var1)$stability$S1)
STCNB <- (stability(Var1)$stability$CNB)
STCNE <- (stability(Var1)$stability$CNE)
STCOT <- (stability(Var1)$stability$COT)
STRTWI <- (stability(Var1)$stability$RTWI)
STSPREAD2 <- (stability(Var1)$stability$SPREAD2)
STS1 <- (stability(Var1)$stability$S1)
bound.STCNB <- boundary(STCNB, alph = 0.05)
bound.STCNE <- boundary(STCNE, alph = 0.05)
bound.STCOT <- boundary(STCOT, alph = 0.05)
bound.STRTWI <- boundary(STRTWI, alph = 0.05)
bound.STSPREAD2 <- boundary(STSPREAD2, alph = 0.05)
bound.STS1 <- boundary(STS1, alph = 0.05)
plot(STCNB, main = "CNB", ylab = "", boundary = FALSE)
lines(bound.STCNB, col = 'red')
lines(-bound.STCNB, col = 'red')
plot(STCNE, main = "CNE", ylab = "", boundary = FALSE)
lines(bound.STCNE, col = 'red')
lines(-bound.STCNE, col = 'red')
plot(STCOT, main = "COT", ylab = "", boundary = FALSE)
lines(bound.STCOT, col = 'red')
lines(-bound.STCOT, col = 'red')
plot(STRTWI, main = "RTWI", ylab = "", boundary = FALSE)
lines(bound.STRTWI, col = 'red')
lines(-bound.STRTWI, col = 'red')
plot(STSPREAD2, main = "SPREAD2", ylab = "", boundary = FALSE)
lines(bound.STSPREAD2, col = 'red')
lines(-bound.STSPREAD2, col = 'red')
plot(STS1, main = "S1", ylab = "", boundary = FALSE)
lines(bound.STS1, col = 'red')
lines(-bound.STS1, col = 'red')
dev.off()
#This is the end of the basic VAR analysis. 
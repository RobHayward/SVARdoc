rm(list=ls(all=TRUE))
library(dse)
library(vars)
library(car)
d <- read.csv("Data/VARdata.csv", header=TRUE, sep=",")
head(d)
# Create variables per GDP for capital flows-----------------------
# Divide by 10 to deal with BN GDP and MLN capital flow data
#d will be the raw data before capital flows
da <- d
da$CCA<-(d$CCA/d$NGDP2)/10
da$DCCA<-(d$DCCA/d$NGDP2)/10
da$CNB <- (d$CNB - d$COT)/d$NGDP2/10
da$CNE<-(d$CNE/d$NGDP2)/10
da$CNFDI<-(d$CNFDI/d$NGDP2)/10
da$COT<-(d$COT/d$NGDP2)/10
# delete unneeded variables---------------------------------------
#delete the first 52 rows with missing values and the debris
da<-da[-c(1:52),]
da<-da[,-c(1:2,9,10,12,13)]
### to shuffle data for the Chlosky decomposition
# da2<-da[,sample(1:12, size = 12, replace = FALSE)]
# head(da2)
# adjust sentiment data-------------------------
da$S1 <- da$S1*(-10)
da$S2 <- da$S2*(-10)
head(da)
# Normalise--------------------------------------------
#normalise da by taking mean and dividing by standard error
#Do we need to normalise them all?  RTWI? 
#normalised data will be dan in stead of da
#head(da)
dan<-scale(da)
head(dan)
#  Dummy Variables###########################################################
# D1
# Q386 to Q188 (55 to 61 in d) and 3 to 9 in da is an interest rate shock for 
#spread 1.  It does not happen with spread 2. 
# is this needed ? d[120:121,]
da$D1=0
da$D1[c(3:9)]=1
# D2
#create a dummy for the shock of the 1994 interest rate increase. 
#2Q94 to 2Q95.  This is 86:90 in the original (d) and 34:38 in the (da) 
#34 plus 52 is 86; 38 plus 52 is 90
da$D2=0
da$D2[c(34:38)]=1
# D3
#q32007 is 139 q42008 is 144.  The dummy is designed to account for the sharp 
#flow in funds (particularly bonds and money market) in this period. 
#87 plus 52 is 139; 92 plus 52 is 144.
da$D3=0
da$D3[c(87:92)]=1
#dummies must be turned into matrix to use with VAR. 
# dum<-cbind(da$D2, da$D3)
dum<-cbind(da$D1,da$D2, da$D3)
#Any other dummies?  Maybe look at the residuals to see if anything is required.
#One possibility would be the dot.com burst.  Check equity and FDI flow.
colnames(dum)<- c("D1", "D2", "D3")
## If data is not prepared
# source("Prepare.R")
head(dan)
# THis will select the variables that are used. 
da <- dan[,c(3, 4, 5, 6, 7,9,11)]
info<-VARselect(da,lag.max=8,type='t')
info
Var1<-VAR(da,p=4, type='both',season=NULL, exog=dum)
logLik(Var1)
summary(Var1)
names(Var1)
roots(Var1)
# Residual preparation#############################################
#Plot residuals
#First make residuals into data from the var and 
#the turn them into time series for plotting and testing. 
#It may be possible to go back from here to think about additional dummies. 
#Indeed, it seems as if there is a need for a dummy at the start of the series
#for the spread equation. 
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
Var1.ser<-serial.test(Var1,lags.pt=8,type="PT.adjusted")
Var1.ser
Box.test(resid1[,7], lag = 4, type = 'Ljung-Box', fitdf = 4)
#test null of no arch---------------------
var1.arch<-arch.test(Var1,lags.multi=4, multivariate.only=FALSE)
var1.arch
#test normality---------------
var1.norm<-normality.test(Var1,multivariate.only=FALSE)
var1.norm
plot(var1.arch)


IR <- irf(Var1, impulse = 'CNB', response = 'RTWI', n.ahead = 2)
plot(IR)


head(IR)


Amat=diag(7)
Amat[1,1]<-1
Amat[2,2]<-1
Amat[3,3]<-1  
Amat[4,4]<-1
Amat[5,5]<-1
Amat[6,6]<-1
Amat[7,7]<-1
Amat[1,2]<-NA #bond vs equity 
Amat[1,3]<-0  #bond vs fdi
Amat[1,4]<-NA #bond vs cot 
Amat[1,5]<-0 #bond vs fx
Amat[1,6]<-NA #bond vs spread
Amat[1,7]<-0 #bond vs sentiment
Amat[2,1]<-NA #equity bonds
Amat[2,3]<-NA #equity fdi (estimated cos similar influences)
Amat[2,4]<-0 #equity cot (dubious if inflow has an effect)
Amat[2,5]<-NA #equity fx
Amat[2,6]<-0 #equity spread (can be justified)?
Amat[2,7]<-NA #equity sentiment
Amat[3,1]<-0 #fdi bond
Amat[3,2]<-NA  #fdi equity
Amat[3,4]<-0 #fdi cot
Amat[3,5]<-NA #fdi fx
Amat[3,6]<-0 #fdi spread
Amat[3,7]<-0 #fdi sentiment
Amat[4,1]<-0 #cot bond
Amat[4,2]<-0 # cot equity
Amat[4,3]<-0 # cot fdi
Amat[4,5]<-NA # cot vs fx
Amat[4,6]<-0 # cot vs spread
Amat[4,7]<-NA # cot vs s1
Amat[5,1]<-0 # fx and bond
Amat[5,2]<-NA # fx vs equity
Amat[5,3]<-NA # fx and FDI
Amat[5,4]<-NA # fx and no
Amat[5,6]<-NA # fx vs spread
Amat[5,7]<-NA # fx vs s1
Amat[6,1]<-NA #spread and bonds
Amat[6,2]<-0  #spread and equities
Amat[6,3]<-0 # spread and fdi
Amat[6,4]<-0 # spread and cot
Amat[6,5]<-NA #spread and fx
Amat[6,7]<-0 #spread and sentiment
Amat[7,1]<-0 #S1 vs bond
Amat[7,2]<-NA #S1 vs equity
Amat[7,3]<-0 #S1 vs fdi
Amat[7,4]<-NA #sentiment vs no
Amat[7,5]<-NA # sentiment vs fx
Amat[7,6]<-0 # sentiment vs spread
Amat

# Estimate the SVAR
Svar1<-SVAR(Var1,estmethod='direct',Amat=Amat,hessian=TRUE)
summary(Svar1)
irfCNB.Svar1<-irf(Svar1, cumulative=T,impulse="CNB",response="RTWI", 
                  boot=TRUE, runs=100, n.ahead=4)
Svar1$A
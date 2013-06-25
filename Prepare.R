rm(list=ls(all=TRUE))
library(xtable)
library(dse)
library(vars)
d <- read.csv("./VARdata.csv", header=TRUE, sep=",")
head(d)
# Create variables per GDP for capital flows---------------------
#d will be the raw data before capital flows
da <- d
da$CCA<-(d$CCA/d$NGDP2)
da$DCCA<-(d$DCCA/d$NGDP2)
da$CNB<-(d$CNB/d$NGDP2)
da$CNBa <- (d$CNB - d$COT)/d$NGDP2
da$CNE<-(d$CNE/d$NGDP2)
da$CNFDI<-(d$CNFDI/d$NGDP2)
da$COT<-(d$COT/d$NGDP2)
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
#create and save time series for plot--------------  
dt<-ts(da,start=c(1986.1),frequency=4)
# Normalise--------------------------------------------
#normalise da by taking mean and dividing by standard error
#Do we need to normalise them all?  RTWI? 
#normalised data will be dan insteady of da
#head(da)
#dan<-scale(da)
#head(dan)
#  Dummy Variables###########################################################
# D1
# Q386 to Q188 (55 to 61 in d) and 3 to 9 in da is an interest rate shock for 
#spread 1.  It does not happen with spread 2. 
d[120:121,]
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
ds <- da[,c(3:7,9,11)]
head(ds)
# The following will add the updated bond variable. 
ds <- da[,c(13, 4:7, 9, 11)]
head(ds)
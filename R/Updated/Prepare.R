rm(list=ls(all=TRUE))
#library(xtable)
library(dse)
library(vars)
# this appears to be missing. 
#library(car)
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
head(da)
da<-da[-c(1:52),]
da<-da[,-c(1:2,9,10,12,13)]
### to shuffle data for the Chlosky decomposition
# da2<-da[,sample(1:12, size = 12, replace = FALSE)]
# head(da2)
# adjust sentiment data-------------------------
da$S1 <- da$S1*(-10)
da$S2 <- da$S2*(-10)
da <- da[ ,c(3, 4, 5, 6, 7, 8, 11)]
colnames(da) <- c('Bond', 'Equity', 'FDI', 'CB', 'ER', 'Irs', 'Spec')
head(da)



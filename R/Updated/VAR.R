# If data is not prepared
# source("Prepare.R" and "dummy.R")
head(dan)
head(da)
# THis will select the variables that are used. 
#da <- da[,c(3:7,9,11)]
info<-VARselect(da,lag.max=8,type='t')
info
Var1<-VAR(dan,p=4, type='both',season=NULL, exog=dum)
Var2<-VAR(da,p=4, type='both',season=NULL, exog=dum)
sample <- sample(c(1:7), size = 7, replace = FALSE)
head(sample)
Var3<-VAR(da[,sample],p=4, type='both',season=NULL, exog=dum)
head(da)

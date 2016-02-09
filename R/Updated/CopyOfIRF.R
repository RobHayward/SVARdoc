# Create impulse response with random order of variables---------------------
set.seed(3)
head(da)
sample <- sample(c(1:7), size = 7, replace = FALSE)
head(sample)
Var2<-VAR(da[,sample],p=4, type='both',season=NULL, exog=dum)
# adjust spread data-------------------------
set.seed(123)
pdf("IRF2.pdf", paper= "a4r", width = 9, title = "IRF2")
par(mfcol=c(2,3), oma = c(0,0,1,0))
# System 2 CNB---------------------------------------
responseList <- c("CNB", "CNE", "CNFDI", "COT", "SPREAS", "S1")
irfList <- list(length(responseList))
par(mfcol=c(2,3), oma = c(0,0,1,0))
for(i in responseList){
irf[[i]] <- irf(Var2, n.ahead=8,impulse=i, response='RTWI',ortho=TRUE, 
                   cumulative=TRUE,boot=TRUE, runs=100)
a<-irf[[i]]$irf
b<-irf[[i]]$Upper
c<-irf[[i]]$Lower
aa<-data.frame(c(a,b,c))
plot(aa[,1], xlim=c(1,8), ylim=c(-6, 6), type='l',
     main='CNB shock: System 2',
     ylab='RTWI', xlab='Quarters')
lines(aa[,2], type='l', col='red', lty=2)
lines(aa[,3],type='l', col='red',lty=2)
abline(h=0, col='red',lty=6)
}

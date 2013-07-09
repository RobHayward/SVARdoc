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
irf.rtwicnb <- irf(Var2, n.ahead=8,impulse='CNB', response='RTWI',ortho=TRUE, 
                   cumulative=TRUE,boot=TRUE, runs=100)
a<-irf.rtwicnb$irf
b<-irf.rtwicnb$Upper
c<-irf.rtwicnb$Lower
aa<-data.frame(c(a,b,c))
plot(aa[,1], xlim=c(1,8), ylim=c(-6, 6), type='l',
     main='CNB shock: System 2',
     ylab='RTWI', xlab='Quarters')
lines(aa[,2], type='l', col='red', lty=2)
lines(aa[,3],type='l', col='red',lty=2)
abline(h=0, col='red',lty=6)
# System 2 CNE------------
irf.rtwicne <- irf(Var2, n.ahead=8,impulse='CNE', response='RTWI',ortho=TRUE, 
                   cumulative=TRUE,boot=TRUE, runs=100)
a<-irf.rtwicne$irf
b<-irf.rtwicne$Upper
c<-irf.rtwicne$Lower
aa<-data.frame(c(a,b,c))
plot(aa[,1], xlim=c(1,8), ylim=c(-4, 8), type='l',
     main='CNE shock: System 2',
     ylab='RTWI', xlab='Quarters')
lines(aa[,2], type='l', col='red', lty=2)
lines(aa[,3],type='l', col='red',lty=2)
abline(h=0, col='red',lty=6)
# System 2 CNFDI --------------------------------
irf.rtwicnfdi <- irf(Var2, n.ahead=8,impulse='CNFDI', response='RTWI',ortho=TRUE, 
                     cumulative=TRUE,boot=TRUE, runs=100)
a<-irf.rtwicnfdi$irf
b<-irf.rtwicnfdi$Upper
c<-irf.rtwicnfdi$Lower
aa<-data.frame(c(a,b,c))
plot(aa[,1], xlim=c(1,8), ylim=c(-4, 6), type='l',
     main='CNFDI shock: System 1',
     ylab='RTWI', xlab='Quarters')
lines(aa[,2], type='l', col='red', lty=2)
lines(aa[,3],type='l', col='red',lty=2)
abline(h=0, col='red',lty=6)
# System 2 COT--------------------------------------
irf.rtwicot <- irf(Var2, n.ahead=8,impulse='COT', response='RTWI',ortho=TRUE, 
                   cumulative=TRUE,boot=TRUE, runs=100)
a<-irf.rtwicot$irf
b<-irf.rtwicot$Upper
c<-irf.rtwicot$Lower
aa<-data.frame(c(a,b,c))
plot(aa[,1], xlim=c(1,8), ylim=c(-4, 4), type='l',
     main='Cot shock: System 2',
     ylab='RTWI', xlab='Quarters')
lines(aa[,2], type='l', col='red', lty=2)
lines(aa[,3],type='l', col='red',lty=2)
abline(h=0, col='red',lty=6)
# System 2 Spread----------------------------
irf.rtwispread <- irf(Var2, n.ahead=8,impulse='SPREAD2', response='RTWI',ortho=TRUE, 
                      cumulative=TRUE,boot=TRUE, runs=100)
a<-irf.rtwispread$irf
b<-irf.rtwispread$Upper
c<-irf.rtwispread$Lower
aa<-data.frame(c(a,b,c))
plot(aa[,1], xlim=c(1,8), ylim=c(-2, 6), type='l',
     main='Spread shock System 2',
     ylab='RTWI', xlab='Quarters')
lines(aa[,2], type='l', col='red', lty=2)
lines(aa[,3],type='l', col='red',lty=2)
abline(h=0, col='red',lty=6)
# system 2 Sentiment ------------------------------------------------
irf.rtwisent <- irf(Var2, n.ahead=8,impulse='S1', response='RTWI',ortho=TRUE, 
                    cumulative=TRUE,boot=TRUE, runs=100)
a<-irf.rtwisent$irf
b<-irf.rtwisent$Upper
c<-irf.rtwisent$Lower
aa<-data.frame(c(a,b,c))
plot(aa[,1], xlim=c(1,8), ylim=c(-2, 6), type='l',
     main='Speculative shock: System 2',
     ylab='RTWI', xlab='Quarters')
lines(aa[,2], type='l', col='red', lty=2)
lines(aa[,3],type='l', col='red',lty=2)
abline(h=0, col='red',lty=6)
dev.off()

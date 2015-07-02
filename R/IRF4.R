# Change the focus of IRF---------------------
set.seed(3)
# This looks at the effect of a shock to S1 on the other variables
pdf("IRF4.pdf", paper= "a4r", width = 9, title = "IRF2")
par(mfcol=c(2,3), oma = c(0,0,1,0))
# S1 on CNB---------------------------------------
irf.rtwicnb <- irf(Var1, n.ahead=8,impulse='S1', response='CNB',ortho=TRUE, 
                   cumulative=TRUE,boot=TRUE, runs=100)
a<-irf.rtwicnb$irf
b<-irf.rtwicnb$Upper
c<-irf.rtwicnb$Lower
aa<-data.frame(c(a,b,c))
plot(aa[,1], xlim=c(1,8), ylim=c(-1, 2), type='l',
     main='S1 shock: CNB',
     ylab='RTWI', xlab='Quarters')
lines(aa[,2], type='l', col='red', lty=2)
lines(aa[,3],type='l', col='red',lty=2)
abline(h=0, col='red',lty=6)
# S1 CNE------------
irf.rtwicne <- irf(Var1, n.ahead=8,impulse='S1', response='CNE',ortho=TRUE, 
                   cumulative=TRUE,boot=TRUE, runs=100)
a<-irf.rtwicne$irf
b<-irf.rtwicne$Upper
c<-irf.rtwicne$Lower
aa<-data.frame(c(a,b,c))
plot(aa[,1], xlim=c(1,8), ylim=c(-1, 1), type='l',
     main='S1 shock: CNE',
     ylab='RTWI', xlab='Quarters')
lines(aa[,2], type='l', col='red', lty=2)
lines(aa[,3],type='l', col='red',lty=2)
abline(h=0, col='red',lty=6)
# S1 CNFDI --------------------------------
irf.rtwicnfdi <- irf(Var1, n.ahead=8,impulse='S1', response='CNFDI',ortho=TRUE, 
                     cumulative=TRUE,boot=TRUE, runs=100)
a<-irf.rtwicnfdi$irf
b<-irf.rtwicnfdi$Upper
c<-irf.rtwicnfdi$Lower
aa<-data.frame(c(a,b,c))
plot(aa[,1], xlim=c(1,8), ylim=c(-1, 2), type='l',
     main='S1: CNFDI',
     ylab='RTWI', xlab='Quarters')
lines(aa[,2], type='l', col='red', lty=2)
lines(aa[,3],type='l', col='red',lty=2)
abline(h=0, col='red',lty=6)
# S1 COT--------------------------------------
irf.rtwicot <- irf(Var1, n.ahead=8,impulse='S1', response='COT',ortho=TRUE, 
                   cumulative=TRUE,boot=TRUE, runs=100)
a<-irf.rtwicot$irf
b<-irf.rtwicot$Upper
c<-irf.rtwicot$Lower
aa<-data.frame(c(a,b,c))
plot(aa[,1], xlim=c(1,8), ylim=c(-1, 1), type='l',
     main='S1: COT',
     ylab='RTWI', xlab='Quarters')
lines(aa[,2], type='l', col='red', lty=2)
lines(aa[,3],type='l', col='red',lty=2)
abline(h=0, col='red',lty=6)
# S1 SPREAD2----------------------------
irf.rtwispread <- irf(Var1, n.ahead=8,impulse='S1', response='SPREAD2',ortho=TRUE, 
                      cumulative=TRUE,boot=TRUE, runs=100)
a<-irf.rtwispread$irf
b<-irf.rtwispread$Upper
c<-irf.rtwispread$Lower
aa<-data.frame(c(a,b,c))
plot(aa[,1], xlim=c(1,8), ylim=c(-1, 1), type='l',
     main='S1 : SPREAD',
     ylab='RTWI', xlab='Quarters')
lines(aa[,2], type='l', col='red', lty=2)
lines(aa[,3],type='l', col='red',lty=2)
abline(h=0, col='red',lty=6)
# system 2 Sentiment ------------------------------------------------
irf.rtwisent <- irf(Var1, n.ahead=8,impulse='S1', response='RTWI', ortho = TRUE,
                    cumulative = TRUE, boot = TRUE, runs = 100)
a<-irf.rtwisent$irf
b<-irf.rtwisent$Upper
c<-irf.rtwisent$Lower
aa<-data.frame(c(a,b,c))
plot(aa[,1], xlim=c(1,8), ylim=c(-2, 4), type='l',
     main='S1 : RTWI',
     ylab='RTWI', xlab='Quarters')
lines(aa[,2], type='l', col='red', lty=2)
lines(aa[,3],type='l', col='red',lty=2)
abline(h=0, col='red',lty=6)
dev.off()

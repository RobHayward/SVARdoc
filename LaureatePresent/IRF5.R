# Create irf for SVAR (made in SVAR.r)
pdf("IRF3.pdf", paper= "a4r", width = 9, title = "IRF2")
par(mfcol=c(2,3), oma = c(0,0,1,0))
# System 3 CNB---------------------------------------
irfCNB.Svar1<-irf(Svar1, cumulative=T,impulse="CNB",response="RTWI", 
                  boot=TRUE, runs=100, n.ahead=7)
a<-irfCNB.Svar1$irf
b<-irfCNB.Svar1$Upper
c<-irfCNB.Svar1$Lower
aa<-data.frame(c(a,b,c))
plot(aa[,1], xlim=c(1,8), ylim=c(-6, 3), type='l',
     main='CNB shock: System 3',
     ylab='RTWI', xlab='Quarters')
lines(aa[,2], type='l', col='red', lty=2)
lines(aa[,3],type='l', col='red',lty=2)
abline(h=0, col='red',lty=6)
# System 3 CNE------------
irfCNE.Svar1<-irf(Svar1, cumulative=T,impulse="CNE",response="RTWI", 
                  boot=TRUE, runs=100, n.ahead=7)
a<-irfCNE.Svar1$irf
b<-irfCNE.Svar1$Upper
c<-irfCNE.Svar1$Lower
aa<-data.frame(c(a,b,c))
plot(aa[,1], xlim=c(1,8), ylim=c(-8, 8), type='l',
     main='CNE shock: System 3',
     ylab='RTWI', xlab='Quarters')
lines(aa[,2], type='l', col='red', lty=2)
lines(aa[,3],type='l', col='red',lty=2)
abline(h=0, col='red',lty=6)
# System 3 CNFDI --------------------------------
irfCNFDI.Svar1<-irf(Svar1, cumulative=T,impulse="CNFDI",response="RTWI", 
                  boot=TRUE, runs=100, n.ahead=7)
a<-irfCNFDI.Svar1$irf
b<-irfCNFDI.Svar1$Upper
c<-irfCNFDI.Svar1$Lower
aa<-data.frame(c(a,b,c))
plot(aa[,1], xlim=c(1,8), ylim=c(-4, 6), type='l',
     main='CNFDI shock: System 3',
     ylab='RTWI', xlab='Quarters')
lines(aa[,2], type='l', col='red', lty=2)
lines(aa[,3],type='l', col='red',lty=2)
abline(h=0, col='red',lty=6)
# System 3 COT--------------------------------------
irfCOT.Svar1<-irf(Svar1, cumulative=T,impulse="COT",response="RTWI", 
                   boot=TRUE, runs=100, n.ahead=7)
a<-irfCOT.Svar1$irf
b<-irfCOT.Svar1$Upper
c<-irfCOT.Svar1$Lower
aa<-data.frame(c(a,b,c))
plot(aa[,1], xlim=c(1,8), ylim=c(-4, 10), type='l',
     main='Cot shock: System 3',
     ylab='RTWI', xlab='Quarters')
lines(aa[,2], type='l', col='red', lty=2)
lines(aa[,3],type='l', col='red',lty=2)
abline(h=0, col='red',lty=6)
# System 3 Spread----------------------------
irfSPREAD.Svar1<-irf(Svar1, cumulative=T,impulse="SPREAD2",response="RTWI", 
                     boot=TRUE, runs=100, n.ahead=7)
a<-irfSPREAD.Svar1$irf
b<-irfSPREAD.Svar1$Upper
c<-irfSPREAD.Svar1$Lower
aa<-data.frame(c(a,b,c))
plot(aa[,1], xlim=c(1,8), ylim=c(-6, 14), type='l',
     main='Spread shock System 3',
     ylab='RTWI', xlab='Quarters')
lines(aa[,2], type='l', col='red', lty=2)
lines(aa[,3],type='l', col='red',lty=2)
abline(h=0, col='red',lty=6)
# system 3 Sentiment ------------------------------------------------
irfS1.Svar1<-irf(Svar1, cumulative=T,impulse="S1",response="RTWI", 
                 boot=TRUE, runs=100, n.ahead=7)
a<-irfS1.Svar1$irf
b<-irfS1.Svar1$Upper
c<-irfS1.Svar1$Lower
aa<-data.frame(c(a,b,c))
plot(aa[,1], xlim=c(1,8), ylim=c(-4, 10), type='l',
     main='Speculative shock: System 3',
     ylab='RTWI', xlab='Quarters')
lines(aa[,2], type='l', col='red', lty=2)
lines(aa[,3],type='l', col='red',lty=2)
abline(h=0, col='red',lty=6)
dev.off()

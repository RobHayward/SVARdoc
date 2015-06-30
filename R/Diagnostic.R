require(xtable)
head(da)
# change i for column number
i <- 1
da <- da[,c(3:7,9,11)]
M <- data.frame(NA)
info<-VARselect(da,lag.max=8,type='t')
Var1<-VAR(da,p=4, type='both',season=NULL, exog=dum)
resid1<-data.frame(resid(Var1))
Var1.ser<-serial.test(Var1,lags.pt=8,type="ES")
Var1.arch<-arch.test(Var1,lags.multi=4, multivariate.only=FALSE)
Var1.norm<-normality.test(Var1,multivariate.only=FALSE)
M[1,i] <- info$selection[1]
M[2,i] <- info$selection[2]
M[3,i] <- Var1$type
M[4,i] <- logLik(Var1)
M[5,i] <- AIC(Var1)
M[6,i] <- BIC(Var1)
M[7,i] <- ifelse(any(roots(Var1) > 1) == FALSE, "Stable", "Unstable")
M[8,i] <- Var1.ser$serial[1]
M[9,i] <- Var1.ser$serial[3]
M[10,i] <- Var1.arch$arch.mul$statistic
M[11,i] <- Var1.arch$arch.mul$p.value
M[12,i] <- Var1.norm$jb.mul$Skewness$statistic
M[13,i] <- Var1.norm$jb.mul$Skewness$p.value
M[14,i] <- Var1.norm$jb.mul$Kurtosis$statistic
M[15,i] <- Var1.norm$jb.mul$Kurtosis$p.value

rownames(M) <- c("Lags (AIC)", "Lags (BIC)", "Type", "LogLikelihood", "AIC", 
              "BIC", "Roots", "Serial Correlation", "P1", "Arch-test", 
               "P2", "Skewness", "P3", "Kurtosis", "P4")
M
M <- xtable(M, caption = "Diagnostic tests", label = "tabref:diag", digits = 2)
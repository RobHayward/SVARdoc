require(xtable)
# Only run the next line once
M <- data.frame(NA)
head(da)
# normalised.  
head(dan)
# change i for column number
i <- 14
#Chose the reqired variables
da_temp <- da[,c(7, 8, 12)]
info<-VARselect(da_temp,lag.max=4,type='t')
# change lags (p), type (const or trend or both or none), exog = change dum. 
var_temp<-VAR(da_temp,p=4, type= 'both', season=NULL, exog = dum)
resid1<-data.frame(resid(var_temp))
var_temp.ser<-serial.test(var_temp,lags.pt=8,type="ES")
var_temp.arch<-arch.test(var_temp,lags.multi=4, multivariate.only=FALSE)
var_temp.norm<-normality.test(var_temp,multivariate.only=FALSE)
IRF <- irf(var_temp, response = "RTWI")
M[1,i] <- info$selection[1]
M[2,i] <- info$selection[2]
M[3,i] <- var_temp$type
M[4,i] <- round(logLik(var_temp), 2)
M[5,i] <- round(var_temp$p, 2)
M[6,i] <- round(AIC(var_temp), 2)
M[7,i] <- round(BIC(var_temp), 2)
M[8,i] <- ifelse(any(roots(var_temp) > 1) == FALSE, "Stable", "Unstable")
M[9,i] <- round(var_temp.ser$serial[[1]], 2)
M[10,i] <- round(var_temp.ser$serial[[3]], )
M[11,i] <- round(var_temp.arch$arch.mul$statistic, )
M[12,i] <- round(var_temp.arch$arch.mul$p.value, )
M[13,i] <- round(var_temp.norm$jb.mul$Skewness$statistic, )
M[14,i] <- round(var_temp.norm$jb.mul$Skewness$p.value, )
M[15,i] <- round(var_temp.norm$jb.mul$Kurtosis$statistic, )
M[16,i] <- round(var_temp.norm$jb.mul$Kurtosis$p.value, )
M[17,i] <- paste(colnames(dum), sep = "", collapse = "\\")
M[18,i] <- paste(colnames(var_temp$y), sep = "", collapse = "\\")

# This will create the plots.  Take a loook and then comple the rest of the table
plot(IRF)

M[19, i] <- 'NA'
M[20, i] <- 'NA'
M[21, i] <- 'NA'
M[22, i] <- 'NA'
M[23, i] <- 'vpos'
M[24, i] <- 'vpos'

rownames(M) <- c("Lags (AIC)", "Lags (BIC)", "Type", "LogLikelihood", "P", "AIC", 
              "BIC", "Roots", "Serial Correlation", "P1", "Arch-test", 
               "P2", "Skewness", "P3", "Kurtosis", "P4", "Dummies", "Endogenous", 
              'NB', 'NE', 'NFDI', 'CB', 'i', 'S')
colnames(M) <- seq(1, 14, 1)

M
Mx <- xtable(M, caption = "Diagnostic tests", label = "tabref:diag", 
              digits = 2) 
Mx

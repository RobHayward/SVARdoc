require(xtable)
# Only run the next line once
M <- data.frame(NA)
head(da)
# normalised.  
head(dan)
# change i for column number
i <- 1
#Chose the reqired variables
da_temp <- da[,c(3:7,9,11)]

info<-VARselect(da_temp,lag.max=4,type='t')
# change lags (p), tuye (constant or trend), exog = change dum. 
var_temp<-VAR(da_temp,p=4, type='both',season=NULL)
resid1<-data.frame(resid(var_temp))
var_temp.ser<-serial.test(var_temp,lags.pt=8,type="ES")
var_temp.arch<-arch.test(var_temp,lags.multi=4, multivariate.only=FALSE)
var_temp.norm<-normality.test(var_temp,multivariate.only=FALSE)
IRF <- irf(var_temp, response = "RTWI")
M[1,i] <- info$selection[1]
M[2,i] <- info$selection[2]
M[3,i] <- var_temp$type
M[4,i] <- logLik(var_temp)
M[5,i] <- var_temp$p
M[6,i] <- AIC(var_temp)
M[7,i] <- BIC(var_temp)
M[8,i] <- ifelse(any(roots(var_temp) > 1) == FALSE, "Stable", "Unstable")
M[9,i] <- var_temp.ser$serial[1]
M[10,i] <- var_temp.ser$serial[3]
M[11,i] <- var_temp.arch$arch.mul$statistic
M[12,i] <- var_temp.arch$arch.mul$p.value
M[13,i] <- var_temp.norm$jb.mul$Skewness$statistic
M[14,i] <- var_temp.norm$jb.mul$Skewness$p.value
M[15,i] <- var_temp.norm$jb.mul$Kurtosis$statistic
M[16,i] <- var_temp.norm$jb.mul$Kurtosis$p.value
M[17,i] <- paste(colnames(dum), sep = "", collapse = "/")
M[18,i] <- paste(colnames(var_temp$y), sep = "", collapse = "-")

# This will create the plots.  Take a loook and then comple the rest of the table
plot(IRF)

M[19, i] <- 'NB does not do much'
M[20, 1] <- 'NE is a small positive'
M[21, i] <- 'NFI is certainly positve'
M[22, i] <- 'CB starts negative and goes neutral'
M[23, i] <- 'i firmly positive'
M[24, i] <- 's strond positive'

rownames(M) <- c("Lags (AIC)", "Lags (BIC)", "Type", "LogLikelihood", "P", "AIC", 
              "BIC", "Roots", "Serial Correlation", "P1", "Arch-test", 
               "P2", "Skewness", "P3", "Kurtosis", "P4", "Dummies", "Endogenous", 
              'NB', 'NE', 'NFDI', 'CB', 'i', 'S')
M
M <- xtable(M, caption = "Diagnostic tests", label = "tabref:diag", digits = 2)


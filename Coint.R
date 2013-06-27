# This file will carry out the tests of stationarity and cointegration.
# Assuming "Prepare data.R" has been run. 
require(vars)
require(urca)
require(xtable)
# This is tt
# ds selects the model 
head(da)
ds <- da[,c(3:7,9,11)]
# The following will add the updated bond variable. 
ds <- da[,c(13, 4:7, 9, 11)]
head(ds)
# Test Unit Root--------------------------
require(xtable)
M <- matrix(0, nrow = 4, ncol = 7)
par(mfrow = c(3,2))
for(i in 1:7){
  title <- paste("Plot of ",colnames(ds)[i], sep = "")
  plot(ds[,i], type = 'l', main = title)
  a <- ur.df(ds[,i], type = "trend", selectlags = "AIC")
  a@cval
  M[1,i] <- a@teststat[1,1] 
  M[2,i] <- a@cval[1,2]
  M[3,i] <- a@teststat[1,3]
  M[4,i] <- a@cval[3,2]
}
colnames(M) <- c("CNB", "CNE", "CNFDI", "COT", "RTWI", "SPREAD2", "S1")
rownames(M) <- c("tau", "CV(tau)", "phi2", "CV(phi)")
#print(xtable(M), title = "Critical Values for DF Test, digits = 4")
M
# apply(da, MARGIN = 2, FUN = ur.df, type = "trend", selectlags = c("AIC"))
plot(ds[,5], type = 'l')


# Creat new dataframe for differenced----------------------------
# After finding that CNB, RTI and SPREAD1 appear to have unit roots, 
# will be differenced to make them stationary. 
DCNB <- diff(ds$CNBa)
DRTWI <- diff(ds$RTWI)
DSPREAD2 <- diff(ds$SPREAD2)
#Remove first row
ds1 <- ds[2:97,]
ds1 <- cbind(DRTWI, DSPREAD2, DCNB, ds1)
#remove levels of CNB, RTWI and SPREAD2
head(ds1)
ds1 <- ds1[,c(1, 2, 3, 5, 6, 7, 10)]
head(ds1)
head(ds1)
jo1 <- ca.jo(ds1, type = 'trace', ecdet = 'none', spec = 'transitory')
summary(jo1)
ec1 <- cajorls(jo1, r = 4)
slotNames(ec1)
plotres(jo1)

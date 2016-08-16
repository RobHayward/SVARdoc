# this is the explanation for page 26 of the Pfaff text
# simulate VAR(2) data
library(dsel)
library(vars)
# setting the lag polynominal A(L)
Apoly <- array(c(1, -0.5, 0.3, 0, 0.2, 0.1, 0, -0.2, 0.7, 1, 0.5, -0.3),
               c(3, 2, 2))
# Setting the covariance n the identity-matrix
B <- diag(2)
# SETTING THE CONSTANT TO 5 AND 10
TRD <- c(5, 10)
# generatin the var model
var2 <- ARMA(A = Apoly, B = B, TREND = TRD)
## Simulating 50 observations
varsim <- simulate(var2, sampleT = 500, 
                   noise = list(w= matrix(rnorm(1000), 
                                         nrow = 500, 
                                         ncol = 2)), rng = list(seed = 123456))
#Obtaining the generated series
vardat <- matrix(varsim$output, nrow = 500, ncol = 2)
colnames(vardat) <- c('y1', 'y2')
#plptting the series
plot.ts(vardat, main = "", xlab = "")
infocrit <- VARselect(vardat, lag.max = 3, type = 'const')
infocrit
varsimest <- VAR(vardat, p = 2, type = 'const')
varsimest
var.causality <- causality(varsimest, cause = 'y2')
var.causality
irf.v1 <- irf(varsimest, impulse = 'y1', response = 'y2', n.ahead = 10, 
           ortho = FALSE, cumulative = FALSE, boot = FALSE, 
           seed = 12345)
plot(irf.v1)
print(irf.v1)
irf.v2 <- irf(varsimest, impulse = 'y2', response = 'y1', n.ahead = 10, 
              ortho = TRUE, cumulative = TRUE, boot = TRUE, 
              seed = 12345)
plot(irf.v2)
print(irf.v2)

~----------------------------------------------------------------
# this is the SVAR test on page 45 pf the Pfaff text. 
Apoly <- array(c(1.0, -0.5, 0.3, 0.8, 
                 0.2, 0.1, -0.7, -0.2, 
                 0.7, 1.0, 0.5, -0.3), 
               c(3, 2, 2))
B <- diag(2)
svarA <- ARMA(A = Apoly, B = B)
svarsim <- simulate(svarA, sampleT = 500, rng = list(seed = c(123456)))
svardata <- matrix(svarsim$output, nrow = 500, ncol = 2)
colnames = c('y1', 'y2')
varest <- VAR(svardata, p = 2, type = 'none') 
Amat <- diag(2)
Amat[2, 1] <- NA
Amat[1, 2] <- NA
args(SVAR)
svar.A <- SVAR(varest, estmethod = 'direct', Amat = Amat, hessian = TRUE)
svar.A$Sigma.U
IR <- irf(svar.A)
# I am not sure how we get from the values in Sigma.U and Svar to the IRF.  What is the calculation? 
plot(IR)
print(IR)
IR[[1]]$y1
# gives the IRF for y1. 
# Here I will use the IRF step by step 
x <- svar.A
#This is the Phi model
Acoef(varest) # are the estimated coefficients of teh reduced form. 

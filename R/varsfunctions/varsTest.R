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

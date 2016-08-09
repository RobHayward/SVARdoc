rm(list = ls())
#Create the errors 
library(vars)
library(MASS)
set.seed(123)
# create the multivariate random variables
Mu <- c(0, 0)
Mu
Sigma <- matrix(c(1, 0.8, 0.8, 1), 2, 2)
Sigma
# mvrnorm will create a multivariate random normal distribution from MASS package
bv <- mvrnorm(100, mu = Mu, Sigma = Sigma)
head(bv)
y <- rep(NA, 100)
z <- rep(NA, 100)
y[1] <- 0
z[1] <- 0
for(i in 2:length(y)){
  y[i] = 0.7 * y[i-1] + 0.2 * z[i-1] + bv[i, 1]
  z[i] = 0.2 * y[i-1] + 0.7 * z[i-1] + bv[i, 2]
}  
plot(y, type = 'l', col = 'red')
lines(z, col = 'blue')
vec <- cbind(y, z)
head(vec)
var1 <- VAR(vec, p = 1, type = 'none')
summary(var1)
var1.resid <- residuals(var1)
serial.test(var1)
arch.test(var1)
normality.test(var1)
IR <- irf(var1, boot = TRUE)
print(IR)
str(IR)
# This is an exercise in creating a print function and then applying it 
# to a number of variables. This will eventually be used in the paper. 

Amat <- diag(2)
Amat[2,1] <- NA
eq <- SVAR(var1, Amat = Amat)
IR2 <- irf(eq)
print(IR2)

print(eq)


par(mfrow = c(2, 1))
names(IR$irf)
myData <- c('y', 'z')
myPlot <- function(myData){
  plot(seq(0,10, 1),   IR2$irf[[myData]][,1], type = 'l', col = 'black', ylim = c(0, 1), 
       lty = 1,   lwd = 3, ylab = "Response", main = paste("Response of y 
       to innovation in ", myData, sep = ""), xlab = "Quarters") 
  lines(seq(0,10, 1), IR$Upper[[myData]][,1], col = 'blue', lty = 2)
  lines(seq(0, 10,1), IR$Lower[[myData]][,1], col = 'blue', lty = 2)
}
lapply(X = myData, FUN = myPlot)
# If there is a negative relationship
for(i in 2:length(y)){
  y[i] = 0.7 * y[i-1] - 0.2 * z[i-1] + bv[i, 1]
  z[i] = -0.2 * y[i-1] + 0.7 * z[i-1] + bv[i, 2]
} 
plot(y, type = 'l', col = 'red')
lines(z, col = 'blue')
vec <- cbind(y, z)
head(vec)
var1 <- VAR(vec, p = 1, type = 'none')
summary(var1)
var1.resid <- residuals(var1)    

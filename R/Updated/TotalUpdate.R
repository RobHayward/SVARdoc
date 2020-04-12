#The Var update
source('R/Updated/Prepare.R')
rm(d)
source('R/Updated/dummy.R')
source('R/Updated/normalise.R')
source('R/Updated/VAR.R')
rm(info)
source('R/Updated/SVAR.R')
source('R/Updated/SVARest.R')
summary(Svar1)
pdf("./Figures3/NewIRFb.pdf", paper= "a4r", width = 9, title = "IRF1")
# Need to make consistent with others so made in irf3.R
par(mfcol=c(2,3), oma = c(0,0,1,0))
myPlot <- function(imp){
plot(unlist(imp[[1]], seq(0, 9, 1)), type = 'l', ylim = c(-2, 4), 
     main = paste("One unit ", names(imp[[1]]), " innovation", sep = ""), 
     xlab = "Quarters", ylab = "RTWI", lwd = 2)
lines(unlist(imp[[2]]), type = 'l', lty = 2)
lines(unlist(imp[[3]]), type = 'l', lty = 2)  
abline(h = 0)
}
capital_flow <- c("Bond", "Equity", "ER", "CB", "Irs", "Spec")
imp <- list(length = length(capital_flow))
for(i in capital_flow){
  imp[[i]] <- irf(Svar3, cumulative = T, impulse = i, response = "ER", boot = "TRUE", 
                runs = 100, n.ahead = 3)
}
#par(mfrow = c(2, 3))
for(i in capital_flow){
  myPlot(imp[[i]])
}
dev.off()

# Plot charts.  Change the name of the chart.  Make the input imp  
pdf("./Figures3/IRF1.pdf", paper= "a4r", width = 9, title = "IRF1")
# Need to make consistent with others so made in irf3.R
par(mfcol=c(2,3), oma = c(0,0,1,0))

#=====================Plot the IRF============================================
myPlot <- function(imp){
plot(unlist(imp[[1]], seq(0, 9, 1)), type = 'l', ylim = c(-6, 10), 
     main = paste("One unit ", names(imp[[1]]), " innovation", sep = ""), 
     xlab = "Quarters", ylab = "RTWI", lwd = 2)
lines(unlist(imp[[2]]), type = 'l', lty = 2)
lines(unlist(imp[[3]]), type = 'l', lty = 2)  
abline(h = 0)
}
capital_flow <- c("b", "e", "fdi", "cb", "i", "s")
imp <- list(length = length(capital_flow))
for(i in capital_flow){
  imp[[i]] <- irf(Svar1, cumulative = T, impulse = i, response = "er", boot = "TRUE", 
                runs = 100, n.ahead = 8)
}
#par(mfrow = c(2, 3))
for(i in capital_flow){
  myPlot(imp[[i]])
}

dev.off()

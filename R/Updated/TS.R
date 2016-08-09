# Create tims series plot of capital flow variables. 
# Run next line if not alredady done. 
#source('R/Prepar.R')
#create and save time series for plot--------------  
dt<-ts(da,start=c(1986.1),frequency=4)
#pdf("Figures/ts2.pdf", paper= "a4", width = 10, height = 10, title = "ts")
par(mfcol=c(3,2), oma = c(0,0,0,0))
plot(dt[,c(2:7, 9, 11)], main = "Cumulative capital flow and exchange rate")
dev.off()



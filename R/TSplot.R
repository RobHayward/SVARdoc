da<-da[,-c(1,2,9,10,12,13,15,16,18)]
head(da)
#create and save time series for plot.  
dt<-ts(da,start=c(1986.1),frequency=4)
##############################################################################
#This is the prepared table for la tex
#table1<-xtable(dt[50:58,])
#print(table1)
plot(dt[,-2], main="Cumulative Capital Flow, Current Account and Exchange Rate")
# Indivdual charts
plot(dt[,2], ylab="Official Treasuries to GDP", main="Change in Current Account")
plot(dt[,4], ylab="Official Treasuries to GDP", main="US Net Equity Flow")
plot(dt[,5], ylab="Official Treasuries to GDP", main="US Net FDI Flow")
plot(dt[,6], ylab="Official Treasuries to GDP", main="Official Treasuries to GDP")

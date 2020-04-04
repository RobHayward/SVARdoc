# Normalise bariables 
# Run next line if not alredady done. 
#source('R/Prepar.R')
# Normalise--------------------------------------------
#normalise da by taking mean and dividing by standard error
#Do we need to normalise them all?  RTWI? 
#normalised data will be dan in stead of da
#head(da)
dan<-scale(da)
head(dan)

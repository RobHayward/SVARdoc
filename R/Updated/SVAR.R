#SVAR sel up
# there will be three models: the central SVAR model is model 1
# the othogonal model is model 2
# the othogonal random model is model 3
head(da)
#head(dan)
#Var1<-VAR(da,p=4, type='both',season=NULL, exog=dum)
#Var3 <- VAR(dan[, c(3:7, 9. 11)], p = 8, type = 'both', season = NULL, exog = dum)
#Setting up the A (or B) Matrix#####################################
#Setting up the A matrix. 
#change the restrictions according to the theory to be tested. 
Amat=diag(7)
Amat[1,1]<-1
Amat[2,2]<-1
Amat[3,3]<-1  
Amat[4,4]<-1
Amat[5,5]<-1
Amat[6,6]<-1
Amat[7,7]<-1
Amat[1,2]<-NA #bond vs equity 
Amat[1,3]<-0  #bond vs fdi
Amat[1,4]<-NA #bond vs cot 
Amat[1,5]<-0 #bond vs fx
Amat[1,6]<-NA #bond vs spread
Amat[1,7]<-0 #bond vs sentiment
Amat[2,1]<-NA #equity bonds
Amat[2,3]<-NA #equity fdi (estimated cos similar influences)
Amat[2,4]<-0 #equity cot (dubious if inflow has an effect)
Amat[2,5]<-NA #equity fx
Amat[2,6]<-0 #equity spread (can be justified)?
Amat[2,7]<-NA #equity sentiment
Amat[3,1]<-0 #fdi bond
Amat[3,2]<-NA  #fdi equity
Amat[3,4]<-0 #fdi cot
Amat[3,5]<-NA #fdi fx
Amat[3,6]<-0 #fdi spread
Amat[3,7]<-0 #fdi sentiment
Amat[4,1]<-0 #cot bond
Amat[4,2]<-0 # cot equity
Amat[4,3]<-0 # cot fdi
Amat[4,5]<-NA # cot vs fx
Amat[4,6]<-0 # cot vs spread
Amat[4,7]<-NA # cot vs s1
Amat[5,1]<-0 # fx and bond
Amat[5,2]<-NA # fx vs equity
Amat[5,3]<-NA # fx and FDI
Amat[5,4]<-NA # fx and no
Amat[5,6]<-NA # fx vs spread
Amat[5,7]<-NA # fx vs s1
Amat[6,1]<-NA #spread and bonds
Amat[6,2]<-0  #spread and equities
Amat[6,3]<-0 # spread and fdi
Amat[6,4]<-0 # spread and cot
Amat[6,5]<-NA #spread and fx
Amat[6,7]<-0 #spread and sentiment
Amat[7,1]<-0 #S1 vs bond
Amat[7,2]<-NA #S1 vs equity
Amat[7,3]<-0 #S1 vs fdi
Amat[7,4]<-NA #sentiment vs no
Amat[7,5]<-NA # sentiment vs fx
Amat[7,6]<-0 # sentiment vs spread
Amat

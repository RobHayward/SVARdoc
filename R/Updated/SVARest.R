# Estimate the SVAR
# There are three models:  1-normalised' 2-non-normalised, cholotsky decomposition
Svar1<-SVAR(Var1,estmethod='direct',Amat=Amat,hessian=TRUE)
Svar1$A
#summary(Svar1)
#Svar2<-SVAR(Var2,estmethod='direct',Amat=Amat,hessian=TRUE)
#Svar2$A
#Svar3<-SVAR(Var3,estmethod='direct',Amat=Amat,hessian=TRUE)
#Svar3$A
# This is the A matrix
#if you want turn list into dataframe and xtable for la tex
#xtable(Svar1$A, digits = 3)
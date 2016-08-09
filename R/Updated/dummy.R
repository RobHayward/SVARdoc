# Create dummay variables----------------------------------------
#  Dummy Variables###########################################################
# D1
# Q386 to Q188 (55 to 61 in d) and 3 to 9 in da is an interest rate shock for 
#spread 1.  It does not happen with spread 2. 
# is this needed ? d[120:121,]
da$D1=0
da$D1[c(3:9)]=1
# D2
#create a dummy for the shock of the 1994 interest rate increase. 
#2Q94 to 2Q95.  This is 86:90 in the original (d) and 34:38 in the (da) 
#34 plus 52 is 86; 38 plus 52 is 90
da$D2=0
da$D2[c(34:38)]=1
# D3
#q32007 is 139 q42008 is 144.  The dummy is designed to account for the sharp 
#flow in funds (particularly bonds and money market) in this period. 
#87 plus 52 is 139; 92 plus 52 is 144.
da$D3=0
da$D3[c(87:92)]=1
#dummies must be turned into matrix to use with VAR. 
# dum<-cbind(da$D2, da$D3)
dum<-cbind(da$D1,da$D2, da$D3)
#Any other dummies?  Maybe look at the residuals to see if anything is required.
#One possibility would be the dot.com burst.  Check equity and FDI flow.
colnames(dum)<- c("D1", "D2", "D3")
# maybe update to include new dummy.  Hau and Rey have breaks at  
# 1994. Assuming first quarter, these would be row 34 onwards for 1994
# that would be 66 for 2002.  This could be tried. 
# 
da$D4 = 0
da$D4[c(34:length(da$D4))] = 1
da$D5 = 0
da$D5[c(66:length(da$D5))] = 1
D4 <- da$D4
D5 <- da$D5
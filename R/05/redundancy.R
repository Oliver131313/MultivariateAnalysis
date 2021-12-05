#package CCA must be downloaded
U<-Distr[,1:7]
V<-Distr[,8:15]
p<-7
q<-8
Cresults<-cc(U,V)
Crho<-Cresults$cor
#
Xstruct<-Cresults$scores$corr.X.xscores #R(X_i,U_j) in a p×p matrix
Xstruct2<-Xstruct^2
Ystruct<-Cresults$scores$corr.Y.yscores
Ystruct2<-Ystruct^2
#
XExplained<-vector("numeric",p)
YExplained<-vector("numeric",p)
for(i in 1:p){  
  XExplained[i]<-sum(Xstruct2[,i]) 
  YExplained[i]<-sum(Ystruct2[,i]) 
}
cat("Relative Explained Variance of the Left Set by particular Us:", XExplained/p, '\n')
cat("Relative Explained Variance of the Right Set by particular  Vs:", YExplained/q, '\n')
cat("Total Explayned for the Left Set by all Us:", sum(XExplained/p)*100,"%", '\n')
cat("Total Explayned for the Right Set by all Vs:", sum(YExplained/q)*100,"%", '\n')
cat("Total Redundancy for the Left Set:",sum(XExplained/p*Crho^2)*100,"%", '\n')
cat("Total Redundancy for the Right Set:", sum(YExplained/q*Crho^2)*100,"%", '\n')
#
k=3
Xfoo<-XExplained/p*Crho^2
Yfoo<-YExplained/q*Crho^2
cat("Redundancy for the Left Set for the first", k, "roots:",sum(Xfoo[1:k])*100,"%", '\n')
cat("Redundancy for the Right Set for the first" , k, "roots:", sum(Yfoo[1:k])*100,"%", '\n')








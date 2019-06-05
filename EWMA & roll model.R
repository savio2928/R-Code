IBM<-read.csv("IBM-2014_2017.csv", header = T)
head(IBM)
n=22
beta=2/(n+1)
B=seq(1:n)
B=beta^B
#creating a new row to redict values for "9/27/2017" 
lastRow=data.frame("10/24/2017",NA,NA,NA,NA,NA,NA)
names(lastRow)=names(IBM)
IBM=rbind(IBM,lastRow)
tail(IBM)

#Calculating log return
IBM$logReturn=c(0,diff(log(IBM$Adj.Close[1:(nrow(IBM)-1)])),NA) 

#SD of Adj. close price
IBM$Var=c(rep(NA,n),rollapply(IBM$logReturn[2:(nrow(IBM))], width = n, FUN = sd))

#Exponentially weighted moving average (EWMA)
x<- function(a){
  y=sum(a*B)/sum(B)
  return(y)
}
IBM$EWMA=c(rep(NA,n),rollapply(IBM$Var[1:(nrow(IBM)-1)], width = n, FUN = x))
IBM


# Roll Model 
IBM$diff=c(NA,(diff(IBM$Adj.Close[1:(nrow(IBM)-1)])),NA)
IBM$Gamma_0=c(rep(NA,n),rollapply(IBM$diff[1:(nrow(IBM)-1)], width = n, FUN = var))
AC<- function(a){
  y=cov(a[1:length(a)-1], a[2:length(a)])
  return(y)
}
IBM$Gamma_1=c(rep(NA,n),rollapply(IBM$diff[1:(nrow(IBM)-1)], width = n, FUN = AC))

#cost 
IBM$c=sqrt(abs(IBM$Gamma_1))
#fundamental volatility 
IBM$RollModel=IBM$Gamma_0+2*IBM$Gamma_1 
IBM
library(ggplot2)
ggplot(IBM,aes(x=EWMA,y=RollModel)) +geom_point(size=2)
#The spots seems to be scattered hence there seems to be no correlation between the values.

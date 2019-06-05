GE<-read.csv("GE_2007-2017.csv", header = T)
head(GE)
n=22
beta=2/(n+1)
B=seq(1:n)
B=beta^B
#creating a new row to redict values for "9/27/2017" 
lastRow=data.frame("9/27/2017",NA,NA,NA,NA,NA,NA)
names(lastRow)=names(GE)
GE=rbind(GE,lastRow)
tail(GE)
#Calculating log return
GE$logReturn=c(0,diff(log(GE$Adj.Close[1:(nrow(GE)-1)])),NA) 

library(zoo)
library(xts)
#SD of Adj. close price
GE$Var=c(rep(NA,n),rollapply(GE$logReturn[2:(nrow(GE))], width = n, FUN = sd))

#Random Walk forecast:
GE$RWF=c(NA,GE$Var[1:(nrow(GE)-1)])
GE
library(quantmod)
#Simple moving average (SMA)
GE$SMA=c(SMA(GE$Var[1:(nrow(GE)-1)],n),NA)

#Exponential smoothing average (EMA)
GE$EMA=c(NA,GE$Var[1:(nrow(GE)-1)]*(1-beta)+GE$SMA[1:(nrow(GE)-1)]*(beta))
GE

#Exponentially weighted moving average (EWMA)
x<- function(a){
  y=sum(a*B)/sum(B)
  return(y)
}
GE$EWMA=c(rep(NA,n),rollapply(GE$Var[1:(nrow(GE)-1)], width = n, FUN = x))
GE
#GARCH(1,1)
library(fGarch)

m2=garchFit(formula=~garch(1,1),data=GE$logReturn[1:(nrow(GE)-1)],trace=F,cond.dist="std")
summary(m2)
predict(m2,5)
R=GE$logReturn[1:(nrow(GE)-1)]
m2 <- garchFit(formula = ~ garch(1,1),data = R,trace = F)
h.t <- m2@h.t
eps <- m2@residuals
B <- coef(m2)

'''my.predict <- function(k) {
  h.predict <- B[2] + h.t[length(R)]*B[4] + (eps[length(R)]^2)*B[3]  
  i <- 2
  while(i <= k) {
    h.predict[i] <- B[2] +  h.predict [i-1]*(B[4]+B[3])
    i <- i + 1
  }
  return(h.predict)
}
my.predict(5)
'''

vol=(sd(GE$logReturn[1:(nrow(GE)-1)],))^2
((B[3]+B[4])^1)*(vol-B[2]/(1-B[3]-B[4])+B[2]/(1-B[3]-B[4]))
((B[3]+B[4])^2)*(vol-B[2]/(1-B[3]-B[4])+B[2]/(1-B[3]-B[4]))
((B[3]+B[4])^3)*(vol-B[2]/(1-B[3]-B[4])+B[2]/(1-B[3]-B[4]))
((B[3]+B[4])^4)*(vol-B[2]/(1-B[3]-B[4])+B[2]/(1-B[3]-B[4]))
((B[3]+B[4])^5)*(vol-B[2]/(1-B[3]-B[4])+B[2]/(1-B[3]-B[4]))





# Roll Model
returns=na.omit(GE$logReturn)
Gamma_0=var(returns)
Gamma_1=cov(returns[1:length(returns)-1], returns[2:length(returns)])
#cost
c=sqrt(-Gamma_1)
c
#fundamental volatility
Sigma=Gamma_0+2*Gamma_1
Sigma

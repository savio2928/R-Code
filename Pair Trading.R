library('urca')  


xom <- read.csv("XOM.csv", header=T);
cvx <- read.csv("CVX.csv", header=T);
summary(xom)
summary(cvx)
a=NULL
a <- data.frame(xom.ret=diff(log(xom$Close)))
a$cvx.ret <- diff(log(cvx$Close))
a$date=xom[2:nrow(xom),1]
a=a[,c(3,1,2)]
summary(a$xom.ret)
summary(a$cvx.ret)
cor(a$xom.ret, a$cvx.ret)
coRes=ca.jo(data.frame(a$xom.ret, a$cvx.ret), type="trace", K=2) 
summary(coRes)  

# linear model z = ret.cvx - alpha*ret.xom + b
lm.pair <- lm(a$cvx.ret~a$xom.ret)
summary(lm.pair)
c=as.numeric(lm.pair$coeff[1])
alpha=as.numeric(lm.pair$coeff[2])
alpha

adf.test <- ur.df(lm.pair$res, type = "none", lags = 2) 
summary(adf.test)
adf.test@teststat
adf.test@cval

# delta = 2*SD
delta.pair <- 2*sd(lm.pair$res)

a$Signal = 0
counter=0

i=1
# The rule to enter into the trade and to exit is coded below
for (i in 1:nrow(a)){
  if ( (a$cvx.ret[i]-alpha*a$xom.ret[i])<(c-delta.pair) && counter < 1) {
    a$Signal[i] = "Buy"
    counter=1
  } else if ( ((a$cvx.ret[i]-alpha*a$xom.ret[i])>(c+delta.pair)) && counter == 1) {
    a$Signal[i] = "Sell"
    counter=-1
  }
  
}

b=a[a$Signal!=0,]


data <- read.table(file="Final_FII_oi.CSV", header=TRUE, sep=",")
data=data[,-1]

data.t <- read.table(file="Final_Total_oi.CSV", header=TRUE, sep=",")
data.t=data.t[,-1]
i=7
colnames(data[7])

data$b=(data[,i])/(data.t[,i])

data$NIfty.lag=c(data$NiftyPrice[2:nrow(data)],0)
cor(data$NIfty.lag,data$b)

par( mfrow = c( 2, 1 ) )
plot(x=data$Date,y=data$b,type="l")
plot(x=data$Date,y=data$NIfty.lag,type="l")
par( mfrow = c( 1, 1 ) )

data$returns=c(diff(log(data$NiftyPrice)),0)


data$return.profit=0
for (i in 2:nrow(data)){
  if(data$b[i]<data$b[i-1]){data$return.profit[i]=data$returns[i]}
  else{data$return.profit[i]=-data$returns[i]}
  
}

sum(data$return.profit)


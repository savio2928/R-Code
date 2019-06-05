data <- read.table(file="Final_FII_oi.CSV", header=TRUE, sep=",")
data=data[,-1]

data$b=(data$Option.Index.Call.Long+data$Option.Index.Put.Short)/(data$Option.Index.Put.Long +data$Option.Index.Call.Short)

c=mean(data$b)
c.up=c*1.3
c.down=c*0.7


ggplot(data=data, aes(x=Date, y=b,group = 1)) + geom_line(color = "Black")+
geom_line(y=c) +
geom_line(y=c.up,col="red") +
geom_line(y=c.down,col="red")

data$Signal = 0
data$E_Price = 0
data$PL = 0
data$Position = 0
data$returns = 0
n = nrow(data)
ntotal = n
counter=0
check="A"
# The rule to enter into the trade and to exit is coded below)
for (i in 2:nrow(data)){
  if ((data$b[i-1] <= c.up) && (data$b[i] > c.up) && (counter > -1) && check != "Buy") {
    data$Signal[i] = "Sell"
    data$E_Price[i] = data$NiftyPrice[i]
    data$Position[i] = "1"
    check ="Sell"
    counter=-1
  } else if ((data$b[i-1] >= c.down) && (data$b[i] < c.down) && (counter < 1)&& check != "Sell" ) {
    data$Signal[i] = "Buy"
    data$E_Price[i] = data$NiftyPrice[i]
    counter=1
    data$Position[i] = "1"
    check="Buy"
  }
  else if ((data$b[i-1] >= c) && (data$b[i] < c) && (counter == 1)) {
    data$Signal[i] = "Sell"
    data$E_Price[i] = data$NiftyPrice[i]
    counter=0
    check="A"
    data$Position[i] = "2"
  }
  else if ((data$b[i-1] <= c) && (data$b[i] > c) && (counter == -1)) {
    data$Signal[i] = "Buy"
    data$E_Price[i] = data$NiftyPrice[i]
    check="A"
    counter=0
    data$Position[i] = "2"
  }
}

for (i in 1:nrow(data)){
  if ((data$Signal[i] == 0 ) && (data$E_Price[i] == 0)){ 
    data$Signal[i] = NA
    data$E_Price[i] = ""
  }
  
}
# This will keep only those rows of the data frame which entered or exited a trade
data = data[!is.na(data$Signal),]
head(data)

n = nrow(data)
ntotal = n


condition1 = data$Signal== "Buy"
condition2 = data$Signal == "Sell"

for (i in 2:nrow(data)){
  if (data$Signal[i]=="Buy" && data$Position[i]==2) {
    data$PL[i] = as.numeric(data$E_Price[i-1]) - as.numeric(data$E_Price[i])
    data$returns[i] = (as.numeric(data$E_Price[i-1]) - as.numeric(data$E_Price[i]))/as.numeric(data$E_Price[i])
  } else if (data$Signal[i]=="Sell" && data$Position[i]==2 ) {
    data$PL[i] = as.numeric(data$E_Price[i]) - as.numeric(data$E_Price[i-1])
    data$returns[i] = (as.numeric(data$E_Price[i]) - as.numeric(data$E_Price[i-1]))/as.numeric(data$E_Price[i])
    
  }
  
}

# This will compute the total number of trades entered
no_of_trades = ntotal - 1
Positive_trades = length(data$PL[data$PL>0 & data$Position=="2"])
Negative_trades = length(data$PL[data$PL<=0 & data$Position=="2"])
no_of_trades = Positive_trades +Negative_trades

# This will compute the total Profit/loss made on all trades
k=as.numeric(data$PL)
s = sum(k)
s

summ=c(no_of_trades,Positive_trades,Negative_trades,s)
summ=matrix(summ,nrow = 4)
colnames(summ)=c("Contrarian Strategy")
rownames(summ)=c("Number of Trades","Positive Trades","Negative Trades","Total Profit")
summ

data$date_diff <- c(NA,diff(as.Date(as.character(data$Date))))
sum(data$date_diff[data$Position=="2"])/365
sum(data$returns[data$Position=="2"])


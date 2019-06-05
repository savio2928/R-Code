
m=c(7,9,11)
summ=NULL
for (k in m)
{
sp500 <- read.table(file="sp500hst-1.CSV", header=TRUE, sep=",")
sp500$Date = as.Date(as.character(sp500$Date),format="%Y%m%d")

BAC=sp500[sp500$Ticker=="BAC",]
head(BAC)
range(BAC$Date)
EMASHORT=12
EMALONG=26
SIGNALLINE=k
BAC$Signal = 0
BAC$E_Price = 0
BAC$PL = 0
BAC$sharp=0
BAC$ret=0
BAC$EMA12=EMA(BAC$Close,n=EMASHORT)
BAC$EMA26=EMA(BAC$Close,n=EMALONG)
BAC$MACD=BAC$EMA12-BAC$EMA26
BAC$Signal_line=EMA(BAC$MACD,n=SIGNALLINE)

n = nrow(BAC)
ntotal = n
nstart = EMALONG+ SIGNALLINE

# The rule to enter into the trade and to exit is coded below
for (i in nstart:ntotal){
  if ((BAC$MACD[i-1] <= BAC$Signal_line[i-1]) && (BAC$MACD[i] > BAC$Signal_line[i])) {
    BAC$Signal[i] = "Buy"
    BAC$E_Price[i] = BAC$Close[i]
  } else if ((BAC$MACD[i-1] >= BAC$Signal_line[i-1]) && (BAC$MACD[i] < BAC$Signal_line[i])) {
    BAC$Signal[i] = "Sell"
    BAC$E_Price[i] = BAC$Close[i]
  }
  
}


# This will remove all the zeros from the Signal and the E_Price column
for (i in 1:ntotal){
  if ((BAC$Signal[i] == 0 ) && (BAC$E_Price[i] == 0)){ 
    BAC$Signal[i] = NA
    BAC$E_Price[i] = ""
  }
  
}
# This will keep only those rows of the data frame which entered or exited a trade
BAC = BAC[!is.na(BAC$Signal),]
 head(BAC)
# This will compute the PL for each trade entered using the revised data frame
n = nrow(BAC)
ntotal = n
print(n)

as.numeric(BAC$E_Price)
BAC$rf=0.1506
BAC$rf[BAC$Date<'2009-12-31']=0.2646
condition1 = BAC$Signal== "Buy"
condition2 = BAC $Signal == "Sell"
nrow(BAC)
for (i in 2:ntotal){
  if (condition1[i]) {
    BAC$PL[i] = as.numeric(BAC$E_Price[i-1]) - as.numeric(BAC$E_Price[i])
    BAC$ret[i]= (as.numeric(BAC$E_Price[i-1]) - as.numeric(BAC$E_Price[i]))/(as.numeric(BAC$E_Price[i]))
    BAC$sharp[i]=BAC$ret[i]-BAC$rf[i]
  } else if (condition2[i]) {
    BAC$PL[i] = as.numeric(BAC$E_Price[i]) - as.numeric(BAC$E_Price[i-1])
    BAC$ret[i]= (as.numeric(BAC$E_Price[i]) - as.numeric(BAC$E_Price[i-1]))/(as.numeric(BAC$E_Price[i-1]))
    BAC$sharp[i]=BAC$ret[i]-BAC$rf[i]
  }
  
}

# This will compute the total number of trades entered
no_of_trades = ntotal - 1
Positive_trades = length(BAC$PL[BAC$PL>0])
Negative_trades = no_of_trades - Positive_trades

# This will compute the total Profit/loss made on all trades
k=as.numeric(BAC$PL)
s = sum(k)
ret=mean(BAC$ret[2:nrow(BAC)])
var.ret=var(BAC$ret[2:nrow(BAC)])
s

initial.invest=100000
final.invest=initial.invest+initial.invest*ret
profit=final.invest-initial.invest
R.Sharpe=mean(BAC$sharp[2:nrow(BAC)])/sqrt(var(BAC$sharp[2:nrow(BAC)]))
summ=c(summ,no_of_trades,Positive_trades,Negative_trades,s,ret,var.ret,initial.invest,final.invest,profit,R.Sharpe)

}

summ=matrix(summ,nrow = 10)
colnames(summ)=c("M=7","M=9","M=11")
rownames(summ)=c("Number of Trades","Positive Trades","Negative Trades","Total Profit", "Average Return", "Variance of return","Initial Investment", "Final Investment","Profit","sharp Ratio")

summ








library(quantmod)
#library(xlsx)
library(zoo)

a1 = Sys.time()

# choose the number of days for SMA and LMA
SMAdays = 50
LMAdays = 200



getSymbols("^NSEI",from='2007-01-01', to='2015-06-24')


head(NSEI)

data=NSEI[,4]



# Computes Short term moving average (SMA) and long-term moving average(LMA) using 
# the SMA function
data$SMA=SMA(data$NSEI.Close,SMAdays)
data$LMA=SMA(data$NSEI.Close,LMAdays)
data$Signal = 0
data$E_Price = 0
data$PL = 0

# To count number of rows in the data set.
n = nrow(data)
ntotal = n
nstart = LMAdays + 1

# The rule to enter into the trade and to exit is coded below
for (i in nstart:ntotal){
  if ((data$SMA[i-1] <= data$LMA[i-1]) && (data$SMA[i] > data$LMA[i])) {
    data$Signal[i] = "Buy"
    data$E_Price[i] = data$NSEI.Close[i]
  } else if ((data$SMA[i-1] >= data$LMA[i-1]) && (data$SMA[i] < data$LMA[i])) {
    data$Signal[i] = "Sell"
    data$E_Price[i] = data$NSEI.Close[i]
  }
  
}


# This will remove all the zeros from the Signal and the E_Price column
for (i in 1:ntotal){
  if ((data$Signal[i] == 0 ) && (data$E_Price[i] == 0)){ 
    data$Signal[i] = NA
    data$E_Price[i] = ""
  }
  
}

# This will keep only those rows of the data frame which entered or exited a trade
data = data[!is.na(data$Signal),]
head(data)

# This will compute the PL for each trade entered using the revised data frame
n = nrow(data)
ntotal = n
print(n)

as.numeric(data$E_Price)


condition1 = data$Signal== "Buy"
condition2 = data$Signal == "Sell"

for (i in 2:ntotal){
  if (condition1[i]) {
    data$PL[i] = as.numeric(data$E_Price[i-1]) - as.numeric(data$E_Price[i])
  } else if (condition2[i]) {
    data$PL[i] = as.numeric(data$E_Price[i]) - as.numeric(data$E_Price[i-1])
  }
  
}

# This will compute the total number of trades entered
no_of_trades = ntotal - 1
Positive_trades = length(data$PL[data$PL>0])
Negative_trades = no_of_trades - Positive_trades

# This will compute the total Profit/loss made on all trades
k=as.numeric(data$PL)

#k=data$PL

s = sum(k)

s



# Create a dataframe for trade summary
Header = c("No.of Trades","Positive trades","Negative trades","Total PL")
Number = c(no_of_trades,Positive_trades,Negative_trades,s)
tsummary = data.frame(Header,Number)


data_fram<-as.data.frame(data)

# Copy the trades and the trade summary to an excel file 
write.zoo(data_fram,"D:/MAC.csv",index.name="Date",sep=",")

# Compute the time taken to run the code.
a2 = Sys.time()
a = a2 - a1
print(a)


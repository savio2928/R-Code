library(quantmod)

tickers <-c("AAPL",
            "GOOGL")
#1.Using the quant mod package, download the Apple data from 01-01-2011 to 01-01-2013.
startDate = '2011-01-01'
endDate = '2013-01-01'

getSymbols(tickers, from=startDate, to=endDate)

#2.	For the data downloaded above, check what class type is the downloaded data and convert it into a data frame and name it Apple_dataframe.
class(AAPL)
Apple_dataframe=data.frame(AAPL)

class(Apple_dataframe)
Apple_dataframe=na.omit(Apple_dataframe)

#3.	Create a new column called Cum_Price and the values in this column should be the product of closing price and the volume.
Apple_dataframe$AAPL.Cum_Price=Apple_dataframe$AAPL.Close*Apple_dataframe$AAPL.Volume


# 4.	Create two new columns called LMA and SMA and calculate exponential moving average choosing moving average window of 60 and 20 respectively.
ESMAdays = 20
ELMAdays = 60
Apple_dataframe$SMA=EMA(Apple_dataframe$AAPL.Close,ESMAdays)
Apple_dataframe$LMA=EMA(Apple_dataframe$AAPL.Close,ELMAdays)

#5.	Create two columns called LMA2 and SMA2 and calculate moving average using roll apply function.
SMAdays = 20
LMAdays = 60
Apple_dataframe$SMA2=SMA(Apple_dataframe$AAPL.Close,SMAdays)
Apple_dataframe$LMA2=SMA(Apple_dataframe$AAPL.Close,LMAdays)

#6.	Download google stock data from 01-01-2011 to 01-01-2013 and perform a linear regression between apple and google. Tabulate the result.
Data <- merge(AAPL,GOOGL)
Data <- na.omit(Data) 
regression<-lm(Data$GOOGL.Close~Data$AAPL.Close)
print(summary(regression))

#7.	Calculate moving average crossover strategy for apple with SMA and LMA pair of 20-60, 30-90, 50-200. Choose the duration from 01-01-2007 to 01-01-2014. Document the returns, positive trade and negative trades. Which pair provides maximum returns?
startDate = '2007-01-01'
endDate = '2014-01-01'

getSymbols("AAPL", from=startDate, to=endDate)


SMAdays = c(20,30,50)
LMAdays = c(60,90,200)
# data fram tmp to store return, Positive and negative trade details
tmp<-data.frame(SMA=SMAdays, LMA=LMAdays)
tmp$Return<-0
tmp$PositiveTrades<-0
tmp$NegativeTrades<-0


J=length(SMAdays)
# Loop to run the program for the given number of MA's
for(r in 1:J)

{
# Computes Short term moving average (SMA) and long-term moving average(LMA) using 
# the SMA function
  
  data=Cl(AAPL)
  data=na.omit(data)

data$SMA=SMA(data$AAPL.Close,tmp[r,1])
data$LMA=SMA(data$AAPL.Close,tmp[r,2])
data$Signal = 0
data$E_Price = 0
data$PL = 0

# To count number of rows in the data set.
n = nrow(data)
ntotal = n
nstart = tmp[r,2] + 1

# The rule to enter into the trade and to exit is coded below
for (i in nstart:ntotal){
  if ((data$SMA[i-1] <= data$LMA[i-1]) && (data$SMA[i] > data$LMA[i])) {
    data$Signal[i] = "Buy"
    data$E_Price[i] = data$AAPL.Close[i]
  } else if ((data$SMA[i-1] >= data$LMA[i-1]) && (data$SMA[i] < data$LMA[i])) {
    data$Signal[i] = "Sell"
    data$E_Price[i] = data$AAPL.Close[i]
  }
  
}


# This will remove all the zeros from the Signal and the E_Price column
for (i in 1:ntotal){
  if ((data$Signal[i] == 0 ) && (data$E_Price[i] == 0)){ 
    data$Signal[i] = NA
    data$E_Price[i] = ""
  }
  
}
#data$Signal <- ifelse(((data$Signal== 0 ) && (data$E_Price== 0)),NA)

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
    data$PL[i] = (as.numeric(data$E_Price[i-1]) - as.numeric(data$E_Price[i]))*100/as.numeric(data$E_Price[i])
  } else if (condition2[i]) {
    data$PL[i] = (as.numeric(data$E_Price[i]) - as.numeric(data$E_Price[i-1]))*100/as.numeric(data$E_Price[i-1])
  }
  
}

# This will compute the total number of trades entered
no_of_trades = ntotal - 1
tmp[r,4] = length(data$PL[data$PL>0])
tmp[r,5] = no_of_trades - tmp[r,4]

# This will compute the total return made on all trades
k=as.numeric(data$PL)

#k=data$PL

tmp[r,3] = sum(k)

}
tmp
#Displays the pair with Maximum Returns:
tmp[which.max(tmp$Return),]
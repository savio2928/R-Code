library(moments) #skew function
library(gdata)# Importing xls file
library(WriteXLS)# Exporting xls file

#Importing Data
SP = read.xls("Data-1.xls",sheet=1,perl = "C:\\Strawberry\\perl\\bin\\perl.exe")
NHMNL = read.xls("Data-1.xls",sheet=2,skip = 1,perl = "C:\\Strawberry\\perl\\bin\\perl.exe")
Above200 = read.xls("StocksAbove200D.xls",sheet=1,perl = "C:\\Strawberry\\perl\\bin\\perl.exe")
NHMNL = NHMNL[,c(1,2)]
SP = SP[,c(1,5)]
Above200 = Above200[,c(1,2)]

#Constants used for reusability
Range=100               # range for PercentRank Calculation
DaysLower = 5           # Day change Lower
DaysHigher = 10        # Day change Upper
MaxExitDay=100         # longest holding period 
SPTarget=10           # S&P criteria
IndicatorTarget=95     #NHMNL and above 200 day criteria

###Function to calculate PERCENTRANK
perc.rank <- function(xo , x)  (length(x[x < xo]))/(length(x)-1)*100

###Function to Combine S&P and Another Indicator and Calculate PERCENTRANK
CombineCalculate = function (SP500,Indicator){
  
#Formating Data
SP500$Date = as.Date(SP500$Date)
Indicator$Date = as.Date(Indicator$Date)
Combine=merge(SP500,Indicator)

#Creating a single file
colnames(Combine) = c("Date","SP500Close","IndicatorClose")
Combine$Date = as.Date(Combine$Date)

#Check For complete records
Combine=Combine[complete.cases(Combine),]
                
#Calculating 5 and 10 Day Change

Combine$SP500DayChangeLower = as.numeric(c(rep(NA,(DaysLower-1)),Combine$SP500Close[DaysLower:(nrow(Combine))]-Combine$SP500Close[1:(nrow(Combine)-(DaysLower-1))]))
Combine$IndicatorDayChangeLower = as.numeric(c(rep(NA,(DaysLower-1)),Combine$IndicatorClose[DaysLower:(nrow(Combine))]-Combine$IndicatorClose[1:(nrow(Combine)-(DaysLower-1))]))
Combine$SP500DayChangeHigher = as.numeric(c(rep(NA,(DaysHigher-1)),Combine$SP500Close[DaysHigher:(nrow(Combine))]-Combine$SP500Close[1:(nrow(Combine)-(DaysHigher-1))]))
Combine$IndicatorDayChangeHigher = as.numeric(c(rep(NA,(DaysHigher-1)),Combine$IndicatorClose[DaysHigher:(nrow(Combine))]-Combine$IndicatorClose[1:(nrow(Combine)-(DaysHigher-1))]))


#percentage Rank Calculation
Combine$SP500RankDayChangeLower=as.numeric(c(""))
Combine$SP500RankDayChangeHigher=as.numeric(c(""))
Combine$IndicatorRankDayChangeLower=as.numeric(c(""))
Combine$IndicatorRankDayChangeHigher=as.numeric(c(""))

for (i in (Range+DaysLower):(Range+DaysHigher-1)){
  Combine$SP500RankDayChangeLower[i]=perc.rank(Combine$SP500DayChangeLower[i],Combine$SP500DayChangeLower[(i-Range):i])
  Combine$IndicatorRankDayChangeLower[i]=perc.rank(Combine$IndicatorDayChangeLower[i],Combine$IndicatorDayChangeLower[(i-Range):i])
}
for (i in (Range+DaysHigher):(nrow(Combine))){
  Combine$SP500RankDayChangeLower[i]=perc.rank(Combine$SP500DayChangeLower[i],Combine$SP500DayChangeLower[(i-Range):i])
  Combine$IndicatorRankDayChangeLower[i]=perc.rank(Combine$IndicatorDayChangeLower[i],Combine$IndicatorDayChangeLower[(i-Range):i])
  Combine$SP500RankDayChangeHigher[i]=perc.rank(Combine$SP500DayChangeHigher[i],Combine$SP500DayChangeHigher[(i-Range):i])
  Combine$IndicatorRankDayChangeHigher[i]=perc.rank(Combine$IndicatorDayChangeHigher[i],Combine$IndicatorDayChangeHigher[(i-Range):i])
}

return(Combine)
}

###Function to check for Trades
TradeCalculation = function (Combine1){

#Creating a Data Frame to store all Trades
trades = data.frame(Entry.Date=as.Date(character()), Entry.Price=double(), Exit.Date=as.Date(character()), Exit.Price=double(),Percentage.Return=double(),Holding.Period=integer(),stringsAsFactors=FALSE)

# Creating a list of Vectors and Calling each vector by the Number of days to exit
Vec <- vector(mode="list", length=MaxExitDay)
names(Vec)=paste(1:MaxExitDay," Day Exit", sep="")

#A counter to keep track of the purchase(if greater than 0 then a trade is in progress) 
counter=c(rep(0,MaxExitDay))
LastCondition=0
k=1 #to increment trade

#Combine1$Check=""    # To check if trades exectue correctly
for (i in (Range+DaysHigher):(nrow(Combine1))){
  if (((Combine1$SP500RankDayChangeLower[i]< SPTarget) || (Combine1$SP500RankDayChangeHigher[i] < SPTarget)) && ((Combine1$IndicatorRankDayChangeLower[i] > IndicatorTarget) || (Combine1$IndicatorRankDayChangeHigher[i] > IndicatorTarget))){
    #Combine1$Check[i]=i  # To check if trades exectue correctly
    counter=counter-(i-LastCondition)
    
    for (j in 1:MaxExitDay){
      if((counter[j]<=0) && (((nrow(Combine1))-1)>j)){
      trades[k,]=list(Combine1$Date[i+1],Combine1$SP500Close[i+1],Combine1$Date[i+j+1],Combine1$SP500Close[i+j+1],((Combine1$SP500Close[i+j+1]-Combine1$SP500Close[i+1])/Combine1$SP500Close[i+1])*100,j)
      Vec[[j]]=c(Vec[[j]],trades[k,5])
      k=k+1
      counter[j]=j
      }
    }
     LastCondition=i 
  }
}
Vec[[MaxExitDay+1]]=trades
return (Vec)
}


### Summary Calculation
GetSummary = function (Vec){
  Summary = data.frame(Mean=double(), StandardDeviation=double(), NumberOfOccurance=integer(),TStat=double(), Skew=double(),stringsAsFactors=FALSE)
for(j in 1:MaxExitDay){
  Summary[j,]=list(mean(Vec[[j]]) , sd(Vec[[j]]) , length(Vec[[j]]) , as.numeric(t.test(Vec[[j]])[1]) , skewness(Vec[[j]]))
}
rownames(Summary)=paste(1:MaxExitDay," Day Exit", sep="")
return (Summary)
}

GetStatistics = function (Vec1){
  Summary = data.frame(Positive.Trades=integer(), Negative.Trades=integer(), Hit.Ratio=double(),Avg.Positive.Trades=double(), Avg.Negative.Trades=double(),Avg.Profit.Over.Avg.Loss=double(),stringsAsFactors=FALSE)
  for(j in 1:MaxExitDay){
    Summary[j,]=list(sum(Vec1[[j]]>0) , sum(Vec1[[j]]<0) , sum(Vec1[[j]]>0)/sum(Vec1[[j]]<0) , mean(Vec1[[j]][(Vec1[[j]]>0)])  , mean(Vec1[[j]][(Vec1[[j]]<0)]) ,   abs((mean(Vec1[[j]][(Vec1[[j]]>0)]))/(mean(Vec1[[j]][(Vec1[[j]]<0)]))))
  }
  rownames(Summary)=paste(1:MaxExitDay," Day Exit", sep="")
  
  return (Summary)
}
  

######## SP500 and NHMNL calculations
SP_NHMNL=CombineCalculate(SP,NHMNL)
SP_NHMNL_Vector= TradeCalculation(SP_NHMNL)
SP_NHMNL_trades=data.frame(SP_NHMNL_Vector[MaxExitDay+1])
SP_NHMNL_Vector[MaxExitDay+1]=NULL
SP_NHMNL_Summary= GetSummary(SP_NHMNL_Vector)

#barplot(SP_NHMNL_Summary$Mean, main="Mean", horiz=FALSE,names.arg=paste(1:MaxExitDay," Day", sep=""),las=2)
barplot(SP_NHMNL_Summary$Mean, main="Mean using SP500 & NHMNL Data", horiz=FALSE,names.arg=paste(1:MaxExitDay," Day", sep=""))

#writing file in xls
WriteXLS(SP_NHMNL_trades, "NewHighMinusNewLowIndicators.xls")

#t-statistic of the percentage returns calculated 
t.test(SP_NHMNL_trades$Percentage.Return)
#since the p value is less than significance level(lets say 0.05) we can reject the NULL hypothesis that the mean percentage return of all trades is equal to 0. So the true mean may not equal 0. So the mean of 1.62 is not that significantly small.


######## SP500 and Percentage stocks above 200D MA calculations
SP_Above200=CombineCalculate(SP,Above200)
SP_Above200_Vector= TradeCalculation(SP_Above200)
SP_Above200_trades=data.frame(SP_Above200_Vector[MaxExitDay+1])
SP_Above200_Vector[MaxExitDay+1]=NULL
SP_Above200_Summary= GetSummary(SP_Above200_Vector)

#barplot(SP_Above200_Summary$Mean, main="Mean", horiz=FALSE,names.arg=paste(1:MaxExitDay," Day", sep=""),las=2)
barplot(SP_Above200_Summary$Mean, main="Mean using SP500 & Percentage of Stocks above 200D MA Data", horiz=FALSE,names.arg=paste(1:MaxExitDay," Day", sep=""))
#writing file in xls
WriteXLS(SP_Above200_trades, "PercentageofStocksabove200dmovingaverage.xls")

#t-statistic of the percentage returns calculated 
t.test(SP_Above200_trades$Percentage.Return)
#since the p value is less than significance level(lets say 0.05) we can reject the NULL hypothesis that the mean percentage return of all trades is equal to 0. So the true mean may not equal 0. So the mean of 2.95 is not that significantly small.



#Additional Statistic for analysis
SP_NHMNL_Statistics=GetStatistics(SP_NHMNL_Vector)
SP_Above200_Statistics=GetStatistics(SP_Above200_Vector)

#Clearly the Hit ratio and mean is better when the "Above 200D MA" is used as an indicator. 
#There are less trade signals generates when "Above 200D MA" is used as an indicator, but when the holding period is chosen correctly there are trades that have never gone negative. 





library(quantmod)
getSymbols("AAPL") #Change the ticker here


#Seasonality through box plot

monthly_data <- to.monthly(AAPL) # Change the ticker here
monthlyReturns <- ClCl(monthly_data)
monthlyReturns[1] <- 0.0
monthIndex <- as.double(format(index(monthlyReturns),'%m'))
tmpRet<-as.double(monthlyReturns)
tmpMon<-as.numeric(monthIndex)
tmp<-data.frame(Return=tmpRet, Month=tmpMon)
tmp$Month = factor(tmp$Month, labels = c("Jan", "Feb",
                                         "Mar", "Apr", "May", "Jun", "Jul",
                                         "Aug", "Sep", "Oct", "Nov", "Dec"))
boxplot(Return~Month, data=tmp)
abline(h=0, col="blue")

#Since the returns were negative in January and positive in March
# We will buy the stock in January and sell in March in the same year

Jan_data=monthly_data[.indexmon(monthly_data)==0]

March_data=monthly_data[.indexmon(monthly_data)==2]

a=March_data$AAPL.Close # Change the value here

b=Jan_data$AAPL.Close # Change the value here

a=data.frame(a)


b=data.frame(b)

c=(a/b-1)*100 # Buy price is January and sell price is March

colnames(c) <- "Seasonality returns in %"

print(c)
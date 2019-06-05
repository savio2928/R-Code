a1=Sys.time()

sym=c("ACC.NS","ADANIPORTS.NS",
      "AMBUJACEM.NS",
      "ASIANPAINT.NS",
      "AUROPHARMA.NS",
      "AXISBANK.NS",
      "BANKBARODA.NS",
      "BHEL.NS",
      "BPCL.NS",
      "BHARTIARTL.NS",
      "INFRATEL.NS",
      "BOSCHLTD.NS",
      "CIPLA.NS",
      "COALINDIA.NS",
      "DRREDDY.NS",
      "EICHERMOT.NS",
      "GAIL.NS",
      "GRASIM.NS",
      "HCLTECH.NS",
      "HDFCBANK.NS",
      "HEROMOTOCO.NS",
      "HINDALCO.NS",
      "HINDUNILVR.NS",
      "HDFC.NS",
      "ITC.NS",
      "ICICIBANK.NS",
      "IDEA.NS",
      "INDUSINDBK.NS",
      "INFY.NS",
      "KOTAKBANK.NS",
      "LT.NS",
      "LUPIN.NS",
      "M&M.NS",
      "MARUTI.NS",
      "NTPC.NS",
      "ONGC.NS",
      "POWERGRID.NS",
      "RELIANCE.NS",
      "SBIN.NS",
      "SUNPHARMA.NS",
      "TCS.NS",
      "TATAMTRDVR.NS",
      "TATAMOTORS.NS",
      "TATAPOWER.NS",
      "TATASTEEL.NS",
      "TECHM.NS",
      "ULTRACEMCO.NS",
      "WIPRO.NS",
      "YESBANK.NS",
      "ZEEL.NS")
# Couldn't download Bajaj auto

frame <- new.env()
startDate = as.Date("2013-01-01")
endDate = as.Date("2015-12-31")

library(quantmod)

getSymbols(c("^NSEI",sym), from = startDate, to = endDate, env=frame)



Retu <- eapply(frame, function(s) ROC(Cl(s), type="discrete"))
RetuDF <- as.data.frame(do.call(merge, Retu))

RetuDF=na.omit(RetuDF)



head(RetuDF)

tail(RetuDF)

RetuDF2=100*RetuDF


head(RetuDF2)

tail(RetuDF2)

vvv <- lapply(RetuDF2, FUN=sd)

vvv 

vvv=as.data.frame(vvv)

apply(vvv,1,min)



mns <- colMeans(vvv, na.rm=TRUE)

vvv <- sqrt(252)*vvv[,order(mns)] # Annualized volatility


rownames(vvv)="Annualized Volatility"

Data_table=data.frame(t(vvv))

Data_table

#tl=head(c(gsub(".Close","",(row.names(Data_table)))),6)

#tradelist=tl[2:6]

tradelist = c("HDFCBANK.NS","POWERGRID.NS","TCS.NS","GRASIM.NS","M&M.NS")

frame2 <- new.env()
startDate = as.Date("2016-01-01") 
endDate = as.Date("2016-12-31")

getSymbols(c(tradelist), from = startDate, to = endDate, env=frame2)

Price <- eapply(frame2, function(s) (Cl(s)))
PriceDF <- as.data.frame(do.call(merge, Price))

PriceDF=na.omit(PriceDF)


head(PriceDF,4)
tail(PriceDF,4)

buyprice=head(PriceDF,1)
sellprice=tail(PriceDF,1)


sellprice$GRASIM.NS.Close=5*sellprice$GRASIM.NS.Close # Stock split of 1:5

strategy_returns=100*(sellprice/buyprice-1)


strategy_returns # In %


a2=Sys.time()

ttt=a2-a1

ttt
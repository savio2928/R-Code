library(quantmod)


startDate = '1995-01-01'
endDate = '2014-04-30'
MA_DAYS = 200
getSymbols('JPM', from=startDate, to=endDate)

closePrices = Cl(JPM)
A=Op(YESBANK.NS)
B=Hi(YESBANK.NS)
closePrices=na.omit(closePrices)
closePrices = as.numeric(closePrices)
N_DAYS = length(closePrices)
MA = SMA( closePrices, MA_DAYS )
signal = "inCash"
buyPrice = 0.0
sellPrice = 0.0
maWealth = 1.0
for(d in (MA_DAYS+1):N_DAYS)
{
  #buy if Stockprice > MA & if not bought yet
  if((closePrices[d] > MA[d]) && (signal == "inCash"))
  {
    buyPrice = closePrices[d]
    signal = "inStock"
  }
  #sell if (Stockprice < MA OR endDate reached)
  # & there is something to sell
  if(((closePrices[d] < MA[d]) || (d == N_DAYS)) && (signal == "inStock"))
  {
    sellPrice = closePrices[d]
    signal = "inCash"
    maWealth = maWealth * (sellPrice / buyPrice)
  }
  
}

plot(closePrices)

bhWealth = closePrices[N_DAYS] / closePrices[(MA_DAYS+1)]
weatlhDiffs = bhWealth - maWealth

bhWealth

maWealth

weatlhDiffs 

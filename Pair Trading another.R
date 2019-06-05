library('quantmod')
library(XML)

PEP <- read.table(file="PEP.CSV", header=TRUE, sep=",")
KO <- read.table(file="KO.CSV", header=TRUE, sep=",")
DPS <- read.table(file="DPS.CSV", header=TRUE, sep=",")

combine2Stocks = 
  function(a, b, stockNames = c(deparse(substitute(a)), 
                                deparse(substitute(b))))
  {
    rr = intersect(a$Date, b$Date)
    a.sub=a[which(a$Date %in% rr),]
    b.sub=b[which(b$Date %in% rr),]
    structure(data.frame(as.Date(a.sub$Date), 
                         a.sub$Adj.Close, 
                         b.sub$Adj.Close),
              names = c("Date", stockNames))
  }

plotRatio =
function(r, k = 1, date = seq(along = r), ...)
{
  plot(date, r, type = "l", ...)
  abline(h = c(mean(r), 
               mean(r) + k * sd(r), 
               mean(r) - k * sd(r)), 
         col = c("darkgreen", rep("red", 2*length(k))), 
         lty = "dashed")
}

findNextPosition =
function(ratio, startDay = 1, k = 1, 
          m = mean(ratio), s = sd(ratio))
{
  up = m + k *s
  down = m - k *s

  if(startDay > 1)
     ratio = ratio[ - (1:(startDay-1)) ]
    
  isExtreme = ratio >= up | ratio <= down
  
  if(!any(isExtreme))
      return(integer())

  start = which(isExtreme)[1]
  backToNormal = if(ratio[start] > up)
                     ratio[ - (1:start) ] <= m
                 else
                     ratio[ - (1:start) ] >= m

  end = if(any(backToNormal))
           which(backToNormal)[1] + start
        else
           length(ratio)
  
  c(start, end) + startDay - 1 
}

getPositions =
function(ratio, k = 1, m = mean(ratio), s = sd(ratio))
{
   when = list()
   cur = 1

   while(cur < length(ratio)) {
      tmp = findNextPosition(ratio, cur, k, m, s)
      if(length(tmp) == 0)  # done
         break
      when[[length(when) + 1]] = tmp
      if(is.na(tmp[2]) || tmp[2] == length(ratio))
         break
      cur = tmp[2]
    }

   when
}

showPosition = 
function(days, ratio, radius = 70)
{
  if(is.list(days))
     days = unlist(days)

  symbols(days, ratio[days], 
          circles = rep(radius, length(days)), 
          fg = c("darkgreen", "red"),
          add = TRUE, inches = FALSE)
}

positionProfit =
  #  r = overlap$att/overlap$verizon
  #  k = 1.7
  #  pos = getPositions(r, k)
  #  positionProfit(pos[[1]], overlap$att, overlap$verizon)
function(pos, stockPriceA, stockPriceB, 
         ratioMean = mean(stockPriceA/stockPriceB), 
         p = .001, byStock = FALSE)
{
  if(is.list(pos)) {
    ans = sapply(pos, positionProfit, 
                  stockPriceA, stockPriceB, ratioMean, p, byStock)
    if(byStock)
       rownames(ans) = c("A", "B", "commission")
    return(ans)
  }
    # prices at the start and end of the positions
  priceA = stockPriceA[pos]
  priceB = stockPriceB[pos]

    # how many units can we by of A and B with $1
  unitsOfA = 1/priceA[1]
  unitsOfB = 1/priceB[1]

    # The dollar amount of how many units we would buy of A and B
    # at the cost at the end of the position of each.
  amt = c(unitsOfA * priceA[2], unitsOfB * priceB[2])

    # Which stock are we selling
  sellWhat = if(priceA[1]/priceB[1] > ratioMean) "A" else "B"

  profit = if(sellWhat == "A") 
              c((1 - amt[1]),  (amt[2] - 1), - p * sum(amt))
           else 
              c( (1 - amt[2]),  (amt[1] - 1),  - p * sum(amt))

  if(byStock)
     profit
  else
     sum(profit)
}

k=0
TP=0
a=list(PEP,KO,DPS)
name=c("PEP-KO","KO-DPS","DPS-PEP")
for(j in 1:3){
 if (j<3){
  b=data.frame(a[j])
  c=data.frame(a[j+1])
 } else {
   b=data.frame(a[j])
   c=data.frame(a[j-2])
 }
  b$Date = as.Date(b$Date,format="%m/%d/%Y")
 c$Date = as.Date(c$Date,format="%m/%d/%Y")
 
 overlap = data.frame(combine2Stocks(b, c))
 overlap=overlap[order(overlap$Date),]
 print(head(overlap))
 print(range(overlap$Date))
 cat("\n")

 i = 1:floor(nrow(overlap)/2)
 train = overlap[i, ]
 test = overlap[ - i, ]
 
 r.train = train$b/train$c
 r.test = test$b/test$c
 
 k.max = max((r.train - mean(r.train))/sd(r.train)) #+5
 
 k.min = min((abs(r.train - mean(r.train))/sd(r.train)))
 
 ks = seq(k.min, k.max, length = 100)
 m  = mean(r.train)
 
 profits =
   sapply(ks,
          function(k) {
            pos = getPositions(r.train, k)
            if(length(pos)>0){
              sum(positionProfit(pos, train$b, train$c, 
                                 mean(r.train)))
            }
            else{
              return(0)
              
            }
          })
 
 x11()
plot(ks, profits, type = "l", xlab = "k", ylab = "Profit", main=name[j])
 
 tmp.k = ks[  profits == max(profits) ]  
 pos = getPositions(r.train, tmp.k[1])
 all(sapply(tmp.k[-1],
            function(k) 
              identical(pos, getPositions(r.train, k))))
 
 k.star = mean(ks[  profits == max(profits) ]  )
 k=c(k,k.star)
 pos = getPositions(r.test, k.star, mean(r.train), sd(r.train))
 testProfit = sum(positionProfit(pos, test$b, test$c)) 
 testProfit
 TP=c(TP,testProfit)
 }



summ=matrix(c(k,TP),nrow = 4)
summ=summ[-1,]
colnames(summ)=c("K VALUE","Total Profit")
rownames(summ)=name
summ
#Finding the strategy with the maximum profit
a=which((summ) == max(summ[,2]), arr.ind = TRUE) 
cat("The pair trading strategy" , rownames(summ)[a[2]],"has a maximum profit of",summ[a[1],2], "and a K value of", summ[a[1],1] )

library('plyr')
library('gdata')
Bronx <- read.xls("rollingsales_bronx.xls",perl = "C:\\Strawberry\\perl\\bin\\perl.exe", pattern="BOROUGH")
Brooklyn <- read.xls("rollingsales_brooklyn.xls",perl = "C:\\Strawberry\\perl\\bin\\perl.exe", pattern="BOROUGH")
Manhattan <- read.xls("rollingsales_manhattan.xls",perl = "C:\\Strawberry\\perl\\bin\\perl.exe", pattern="BOROUGH")
Queens <- read.xls("rollingsales_queens.xls",perl = "C:\\Strawberry\\perl\\bin\\perl.exe", pattern="BOROUGH")
StatenIsland <- read.xls("rollingsales_statenisland.xls",perl = "C:\\Strawberry\\perl\\bin\\perl.exe", pattern="BOROUGH")

# cleaning Bronx data
names(Bronx) <- tolower(names(Bronx))
Bronx$sale.price.n <- as.numeric(gsub("[^[:digit:]]","",Bronx$sale.price))
count(is.na(Bronx$sale.price.n))
Bronx$sale.date <- as.Date(Bronx$sale.date)
Bronx$sale.date
Bronx$gross.sqft <- as.numeric(gsub("[^[:digit:]]","",Bronx$gross.square.feet))
Bronx$gross.sqft
Bronx$land.sqft <- as.numeric(gsub("[^[:digit:]]","",Bronx$land.square.feet))
Bronx$land.sqft
Bronx$year.built <- as.numeric(as.character(Bronx$year.built))
Bronx$year.built

#test to check if data is correct
attach(Bronx)
hist(sale.price.n)
summary(sale.price.n)
hist(sale.price.n[sale.price.n>0])
sale.price.n[sale.price.n>0]
hist(gross.sqft[sale.price.n==0])
detach(Bronx)

#removing sales=0
BronxSales <- Bronx[Bronx$sale.price.n!=0,]
plot(log(BronxSales$gross.sqft),log(BronxSales$sale.price.n))

#removing Outliers 
BronxFamily <- BronxSales [which(grepl("FAMILY",BronxSales$building.class.category)),]
plot(log(BronxFamily$gross.sqft),log(BronxFamily$sale.price.n))
BronxFamily$outliers <- (log(BronxFamily$sale.price.n) <= 5) + 0
BronxFamily <- BronxFamily[which(BronxFamily$outlier==0),]
hist(log(BronxFamily$sale.price.n), main = "Histogram of log of Bronx 1,2,3 Family sales price", breaks =10, xlab="Log Returns")

BronxCoops <- BronxSales [which(grepl("COOPS",BronxSales$building.class.category)),]
plot(log(BronxCoops$gross.sqft),log(BronxCoops$sale.price.n))

BronxCondos <- BronxSales [which(grepl("CONDOS",BronxSales$building.class.category)),]
hist(log(BronxCondos$sale.price.n), main = "Histogram of log of Bronx Condos sales price", breaks =10, xlab="Log Returns")

#deleting outlier column and creating a single list with family, coops and condos data
BronxFamily$outliers<-NULL
BronxFamily$category<-"Family"
BronxCoops$category<-"Coops"
BronxCondos$category<-"Condos"
data4<-(rbind(BronxFamily,BronxCoops,BronxCondos))
head(data4)
ggplot(data4,aes(x=category, y=log(sale.price.n)))+geom_boxplot()





# cleaning Brooklyn data
names(Brooklyn) <- tolower(names(Brooklyn))
Brooklyn$sale.price.n <- as.numeric(gsub("[^[:digit:]]","",Brooklyn$sale.price))
count(is.na(Brooklyn$sale.price.n))
Brooklyn$sale.date <- as.Date(Brooklyn$sale.date)
Brooklyn$sale.date
Brooklyn$gross.sqft <- as.numeric(gsub("[^[:digit:]]","",Brooklyn$gross.square.feet))
Brooklyn$gross.sqft
Brooklyn$land.sqft <- as.numeric(gsub("[^[:digit:]]","",Brooklyn$land.square.feet))
Brooklyn$land.sqft
Brooklyn$year.built <- as.numeric(as.character(Brooklyn$year.built))
Brooklyn$year.built

#test to check if data is correct
attach(Brooklyn)
hist(sale.price.n)
summary(sale.price.n)
hist(sale.price.n[sale.price.n>0])
sale.price.n[sale.price.n>0]
hist(gross.sqft[sale.price.n==0])
detach(Brooklyn)

#removing sales=0
BrooklynSales <- Brooklyn[Brooklyn$sale.price.n!=0,]
plot(log(BrooklynSales$gross.sqft),log(BrooklynSales$sale.price.n))

#removing Outliers 
BrooklynFamily <- BrooklynSales [which(grepl("FAMILY",BrooklynSales$building.class.category)),]
plot(log(BrooklynFamily$gross.sqft),log(BrooklynFamily$sale.price.n))
BrooklynFamily$outliers <- (log(BrooklynFamily$sale.price.n) <= 5) + 0
BrooklynFamily <- BrooklynFamily[which(BrooklynFamily$outlier==0),]
hist(log(BrooklynFamily$sale.price.n), main = "Histogram of log of Brooklyn 1,2,3 Family sales price", breaks =10, xlab="Log Returns")

BrooklynCoops <- BrooklynSales [which(grepl("COOPS",BrooklynSales$building.class.category)),]
plot(log(BrooklynCoops$gross.sqft),log(BrooklynCoops$sale.price.n))

BrooklynCondos <- BrooklynSales [which(grepl("CONDOS",BrooklynSales$building.class.category)),]
hist(log(BrooklynCondos$sale.price.n), main = "Histogram of log of Brooklyn Condos sales price", breaks =10, xlab="Log Returns")

#deleting outlier column and creating a single list with family, coops and condos data
BrooklynFamily$outliers<-NULL
BrooklynFamily$category<-"Family"
BrooklynCoops$category<-"Coops"
BrooklynCondos$category<-"Condos"
data4<-(rbind(BrooklynFamily,BrooklynCoops,BrooklynCondos))
head(data4)
ggplot(data4,aes(x=category, y=log(sale.price.n)))+geom_boxplot()





# cleaning Manhattan data
names(Manhattan) <- tolower(names(Manhattan))
Manhattan$sale.price.n <- as.numeric(gsub("[^[:digit:]]","",Manhattan$sale.price))
count(is.na(Manhattan$sale.price.n))
Manhattan$sale.date <- as.Date(Manhattan$sale.date)
Manhattan$sale.date
Manhattan$gross.sqft <- as.numeric(gsub("[^[:digit:]]","",Manhattan$gross.square.feet))
Manhattan$gross.sqft
Manhattan$land.sqft <- as.numeric(gsub("[^[:digit:]]","",Manhattan$land.square.feet))
Manhattan$land.sqft
Manhattan$year.built <- as.numeric(as.character(Manhattan$year.built))
Manhattan$year.built

#test to check if data is correct
attach(Manhattan)
hist(sale.price.n)
summary(sale.price.n)
hist(sale.price.n[sale.price.n>0])
sale.price.n[sale.price.n>0]
hist(gross.sqft[sale.price.n==0])
detach(Manhattan)

#removing sales=0
ManhattanSales <- Manhattan[Manhattan$sale.price.n!=0,]
plot(log(ManhattanSales$gross.sqft),log(ManhattanSales$sale.price.n))

#removing Outliers 
ManhattanFamily <- ManhattanSales [which(grepl("FAMILY",ManhattanSales$building.class.category)),]
plot(log(ManhattanFamily$gross.sqft),log(ManhattanFamily$sale.price.n))
ManhattanFamily$outliers <- (log(ManhattanFamily$sale.price.n) <= 5) + 0
ManhattanFamily <- ManhattanFamily[which(ManhattanFamily$outlier==0),]
hist(log(ManhattanFamily$sale.price.n), main = "Histogram of log of Manhattan 1,2,3 Family sales price", breaks =10, xlab="Log Returns")

ManhattanCoops <- ManhattanSales [which(grepl("COOPS",ManhattanSales$building.class.category)),]
plot(log(ManhattanCoops$gross.sqft),log(ManhattanCoops$sale.price.n))

ManhattanCondos <- ManhattanSales [which(grepl("CONDOS",ManhattanSales$building.class.category)),]
hist(log(ManhattanCondos$sale.price.n), main = "Histogram of log of Manhattan Condos sales price", breaks =10, xlab="Log Returns")

#deleting outlier column and creating a single list with family, coops and condos data
ManhattanFamily$outliers<-NULL
ManhattanFamily$category<-"Family"
ManhattanCoops$category<-"Coops"
ManhattanCondos$category<-"Condos"
data4<-(rbind(ManhattanFamily,ManhattanCoops,ManhattanCondos))
head(data4)
ggplot(data4,aes(x=category, y=log(sale.price.n)))+geom_boxplot()








# cleaning Queens data
names(Queens) <- tolower(names(Queens))
Queens$sale.price.n <- as.numeric(gsub("[^[:digit:]]","",Queens$sale.price))
count(is.na(Queens$sale.price.n))
Queens$sale.date <- as.Date(Queens$sale.date)
Queens$sale.date
Queens$gross.sqft <- as.numeric(gsub("[^[:digit:]]","",Queens$gross.square.feet))
Queens$gross.sqft
Queens$land.sqft <- as.numeric(gsub("[^[:digit:]]","",Queens$land.square.feet))
Queens$land.sqft
Queens$year.built <- as.numeric(as.character(Queens$year.built))
Queens$year.built

#test to check if data is correct
attach(Queens)
hist(sale.price.n)
summary(sale.price.n)
hist(sale.price.n[sale.price.n>0])
sale.price.n[sale.price.n>0]
hist(gross.sqft[sale.price.n==0])
detach(Queens)

#removing sales=0
QueensSales <- Queens[Queens$sale.price.n!=0,]
plot(log(QueensSales$gross.sqft),log(QueensSales$sale.price.n))

#removing Outliers
QueensFamily <- QueensSales [which(grepl("FAMILY",QueensSales$building.class.category)),]
plot(log(QueensFamily$gross.sqft),log(QueensFamily$sale.price.n))
QueensFamily$outliers <- (log(QueensFamily$sale.price.n) <= 5) + 0
QueensFamily <- QueensFamily[which(QueensFamily$outlier==0),]
hist(log(QueensFamily$sale.price.n), main = "Histogram of log of Queens 1,2,3 Family sales price", breaks =10, xlab="Log Returns")

QueensCoops <- QueensSales [which(grepl("COOPS",QueensSales$building.class.category)),]
plot(log(QueensCoops$gross.sqft),log(QueensCoops$sale.price.n))

QueensCondos <- QueensSales [which(grepl("CONDOS",QueensSales$building.class.category)),]
hist(log(QueensCondos$sale.price.n), main = "Histogram of log of Queens Condos sales price", breaks =10, xlab="Log Returns")

#deleting outlier column and creating a single list with family, coops and condos data
QueensFamily$outliers<-NULL
QueensFamily$category<-"Family"
QueensCoops$category<-"Coops"
QueensCondos$category<-"Condos"
data4<-(rbind(QueensFamily,QueensCoops,QueensCondos))
head(data4)
ggplot(data4,aes(x=category, y=log(sale.price.n)))+geom_boxplot()




# cleaning StatenIsland data
names(StatenIsland) <- tolower(names(StatenIsland))
StatenIsland$sale.price.n <- as.numeric(gsub("[^[:digit:]]","",StatenIsland$sale.price))
count(is.na(StatenIsland$sale.price.n))
StatenIsland$sale.date <- as.Date(StatenIsland$sale.date)
StatenIsland$sale.date
StatenIsland$gross.sqft <- as.numeric(gsub("[^[:digit:]]","",StatenIsland$gross.square.feet))
StatenIsland$gross.sqft
StatenIsland$land.sqft <- as.numeric(gsub("[^[:digit:]]","",StatenIsland$land.square.feet))
StatenIsland$land.sqft
StatenIsland$year.built <- as.numeric(as.character(StatenIsland$year.built))
StatenIsland$year.built

#test to check if data is correct
attach(StatenIsland)
hist(sale.price.n)
summary(sale.price.n)
hist(sale.price.n[sale.price.n>0])
sale.price.n[sale.price.n>0]
hist(gross.sqft[sale.price.n==0])
detach(StatenIsland)

#removing sales=0
StatenIslandSales <- StatenIsland[StatenIsland$sale.price.n!=0,]
plot(log(StatenIslandSales$gross.sqft),log(StatenIslandSales$sale.price.n))

#removing Outliers
StatenIslandFamily <- StatenIslandSales [which(grepl("FAMILY",StatenIslandSales$building.class.category)),]
plot(log(StatenIslandFamily$gross.sqft),log(StatenIslandFamily$sale.price.n))
StatenIslandFamily$outliers <- (log(StatenIslandFamily$sale.price.n) <= 5) + 0
StatenIslandFamily <- StatenIslandFamily[which(StatenIslandFamily$outlier==0),]
hist(log(StatenIslandFamily$sale.price.n), main = "Histogram of log of StatenIsland 1,2,3 Family sales price", breaks =10, xlab="Log Returns")

StatenIslandCoops <- StatenIslandSales [which(grepl("COOPS",StatenIslandSales$building.class.category)),]
StatenIslandCondos <- StatenIslandSales [which(grepl("CONDOS",StatenIslandSales$building.class.category)),]
hist(log(StatenIslandCondos$sale.price.n), main = "Histogram of log of StatenIsland Condos sales price", breaks =10, xlab="Log Returns")

#deleting outlier column and creating a single list with family, coops and condos data
StatenIslandFamily$outliers<-NULL
StatenIslandFamily$category<-"Family"
StatenIslandCoops$category<-"Coops"
StatenIslandCondos$category<-"Condos"
data4<-(rbind(StatenIslandFamily,StatenIslandCoops,StatenIslandCondos))
head(data4)
ggplot(data4,aes(x=category, y=log(sale.price.n)))+geom_boxplot()
































#Importing Data
data1 <- read.csv("nyt1.csv")
data2 <- read.csv("nyt2.csv")
data3 <- read.csv("nyt3.csv")

#categorize
data1$age_group <- cut(data1$Age, c(-Inf,0,20,29,39,49,59,69,Inf))
data2$age_group <- cut(data2$Age, c(-Inf,0,20,29,39,49,59,69,Inf))
data3$age_group <- cut(data3$Age, c(-Inf,0,20,29,39,49,59,69,Inf))
#view
summary(data1)
summary(data2)
summary(data3)

#distribution of number of impressions and click-through-rate
library("ggplot2")
ggplot(subset(data1, Impressions >0),aes(x=Impressions,y=Clicks/Impressions))+
  facet_grid(age_group~.)+geom_point() + labs(title = "Day 1: Number of impressions vs click-through-rate")
ggplot(subset(data2, Impressions >0),aes(x=Impressions,y=Clicks/Impressions))+
  facet_grid(age_group~.)+geom_point()+ labs(title = "Day 2: Number of impressions vs click-through-rate")
ggplot(subset(data3, Impressions >0),aes(x=Impressions,y=Clicks/Impressions))+
  facet_grid(age_group~.)+geom_point()+ labs(title = "Day 3: Number of impressions vs click-through-rate")

# create categories
data1$scode[data1$Impressions==0] <- "NoImps"
data1$scode[data1$Impressions >0] <- "Imps"
data1$scode[data1$Clicks >0] <- "Clicks"

data2$scode[data2$Impressions==0] <- "NoImps"
data2$scode[data2$Impressions >0] <- "Imps"
data2$scode[data2$Clicks >0] <- "Clicks"

data3$scode[data3$Impressions==0] <- "NoImps"
data3$scode[data3$Impressions >0] <- "Imps"
data3$scode[data3$Clicks >0] <- "Clicks"



# Covert the column to a factor
data1$scode <- factor(data1$scode)
head(data1)

data2$scode <- factor(data2$scode)
head(data1)

data3$scode <- factor(data3$scode)
head(data1)


#Impression per click for male and female for signed in users only
ggplot(subset(data1, Impressions >0 & Signed_In==1),aes(x=Impressions,y=Clicks/Impressions))+
  facet_grid(age_group~Gender)+geom_point()+ labs(title = "Day 1: Impression vs click/impression for male and female for signed in users only")

ggplot(subset(data2, Impressions >0 & Signed_In==1),aes(x=Impressions,y=Clicks/Impressions))+
  facet_grid(age_group~Gender)+geom_point()+ labs(title = "Day 2: Impression vs click/impression for male and female for signed in users only")

ggplot(subset(data3, Impressions >0 & Signed_In==1),aes(x=Impressions,y=Clicks/Impressions))+
  facet_grid(age_group~Gender)+geom_point()+ labs(title = "Day 3: Impression vs click/impression for male and female for signed in users only")

#Impression per click for signed in vs not
ggplot(subset(data1, Impressions >0 ),aes(x=Impressions,y=Clicks/Impressions))+
  facet_grid(.~Signed_In)+geom_point()+ labs(title = "Day 1: Impression vs click/impression for male and female for signed in vs not")

ggplot(subset(data2, Impressions >0 ),aes(x=Impressions,y=Clicks/Impressions))+
  facet_grid(.~Signed_In)+geom_point()+ labs(title = "Day 2: Impression vs click/impression for male and female for signed in vs not")

ggplot(subset(data3, Impressions >0 ),aes(x=Impressions,y=Clicks/Impressions))+
  facet_grid(.~Signed_In)+geom_point()+ labs(title = "Day 3: Impression vs click/impression for male and female for signed in vs not")


#combining all 3 days data into 1 file 
data1$day<-1
data2$day<-2
data3$day<-3
data4<-(rbind(data1,data2,data3))
head(data4)
typeof(data4)

#Impression per click across days
ggplot(subset(data4, Impressions >0 ),aes(x=Impressions,y=Clicks/Impressions))+
  facet_grid(.~day)+geom_point()+ labs(title = "Impression vs click/impression across days")


ggplot(subset(data4, Impressions >0 & Signed_In==1),aes(x=Impressions,y=Clicks/Impressions))+
  facet_grid(Gender~day)+geom_point()+ labs(title = "Impression vs click/impression across gender across days")

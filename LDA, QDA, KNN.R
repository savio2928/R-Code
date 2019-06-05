library(car) 
library(MASS)
library(class)


Training<-read.csv("Default.csv", header = T)
Testing<-read.csv("DefaultPredict.csv", header = T)
Training<-Training[,-1]
Testing<-Testing[,-1]

#attach(Defaults)
#detach(Defaults)


names(Training)
head(Training)
summary(Training)
scatterplot(income ~ balance | default, data=Training, xlab="Balance", ylab="Income", 
            main="Annual Income & Monthly Credit Card Payments", labels=row.names(Training))
par( mfrow = c( 1, 2 ) )
boxplot(balance ~ default, data=Training, xlab="Default", ylab="Balance")
boxplot(income ~ default, data=Training, xlab="Default", ylab="Income")
par( mfrow = c( 1, 1 ) )
cor(Training [, c( -1, -2 )])
pairs(Training [, c( -1, -2 )])
Default=Training$default

glm.default.balance=glm(default~balance,data=Training,family=binomial)
glm.default.balance
summary(glm.default.balance)

glm.default.student=glm(default~student,data=Training,family=binomial)
glm.default.student
summary(glm.default.student)

glm.default.multi=glm(default~balance+student+income,data=Training,family=binomial)
glm.default.multi
summary(glm.default.multi)

glm.default.multi.refined=glm(default~balance+student,data=Training,family=binomial)
glm.default.multi.refined
summary(glm.default.multi.refined)

##Logistic
glm.fit=glm(default~balance+student+income,data=Training,family=binomial)
summary(glm.fit)
confint(glm.fit)
confint.default(glm.fit)
exp(coef(glm.fit))
glm.probs=predict(glm.fit,newdata = Testing, type="response")
glm.probs[1:10]
dim(Testing)
glm.pred=rep("No",dim(Testing)[1])
glm.pred[glm.probs>0.5]="Yes"
prediction.glm=cbind(Testing,glm.pred)
head(prediction.glm)
colnames(prediction.glm)[4]="default prediction"
head(prediction.glm)
#View(prediction.glm)
table(glm.pred)


##LDA

lda.fit=lda(default~student+balance+income,data=Training)
lda.fit
plot(lda.fit)
lda.pred=predict(lda.fit,Testing)
head(lda.pred)
lda.class=lda.pred$class
prediction.lda=cbind(Testing,lda.class)
colnames(prediction.lda)[4]="default prediction"
head(prediction.lda)
#View(prediction.lda)
table(lda.class)


#QDA
qda.fit=qda(default~student+balance+income,data=Training)
qda.fit
qda.pred=predict(qda.fit,Testing)
qda.class=qda.pred$class
prediction.qda=cbind(Testing,qda.class)
colnames(prediction.qda)[4]="default prediction"
head(prediction.qda)
#View(prediction.lda)
table(qda.class)


#KNN
Testing.x=cbind(Testing$student,Testing$balance,Testing$income)
Training.x=cbind(Training$student,Training$balance,Training$income)


for (i in 5:10){
knn.pred=knn(Training.x,Testing.x,Training$default,k=i)
prediction.knn=cbind(Testing,knn.pred)
colnames(prediction.knn)[4]="default prediction"
cat("\n","K=",i,"\n")
print(head(prediction.knn))
print(table(qda.class))
}

#--------






d=0
e=0
f=0
g=0
b=seq(0.50,0.80,0.05)

for (i in b){
training=Training[1:round(dim(Training)[1]*i),]
test=Training[round(dim(Training)[1]*i+1):dim(Training)[1],]


glm.fit=glm(default~balance+student+income,data=training,family=binomial)
glm.probs=predict(glm.fit,newdata = test, type="response")
glm.pred=rep("No",dim(test)[1])
glm.pred[glm.probs>0.5]="Yes"
#View(prediction.glm)
contrasts(test$default)
table(glm.pred,test$default)
d=c(d,mean(glm.pred != test$default))


##LDA
lda.fit=lda(default~student+balance+income,data=training)
lda.pred=predict(lda.fit,test)
lda.class=lda.pred$class
#View(prediction.lda)
contrasts(test$default)
table(lda.class ,test$default)
e=c(e,mean(lda.class != test$default))

#QDA
qda.fit=qda(default~student+balance+income,data=training)
qda.pred=predict(qda.fit,test)
qda.class=qda.pred$class

#View(prediction.lda)
contrasts(test$default)
table(qda.class ,test$default)
f=c(f,mean(qda.class != test$default))

test.x=cbind(test$student,test$balance,test$income)
training.x=cbind(training$student,training$balance,training$income)

for (i in 5:10){
knn.pred=knn(training.x,test.x,training$default,k=i)
contrasts(test$default)
table(knn.pred ,test$default)
g=c(g,mean(knn.pred != test$default))
}

}

err1=matrix(c(d,e,f),nrow = 8)
err1=err1[-1,]
g=g[-1]
err2=matrix(g,nrow = 7)
error=cbind(err1,err2)
colnames(error)=c("GLM","LDA","QDA","KNN K=5","KNN K=6","KNN K=7","KNN K=8","KNN K=9","KNN K=10")
error=rbind(error,rowMeans(error))
rownames(error)=c("50% of Data","55% of Data","60% of Data","65% of Data","70% of Data","75% of Data","80% of Data","Average")
#comparing all error values
error
#Finding the method with the least error value
which((error["Average",]) == min(error["Average",]), arr.ind = TRUE)

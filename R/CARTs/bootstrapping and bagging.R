# Example 1

library(MASS)
library(rpart)
set.seed(1234)
data(Boston)

bag.fn<-function(data, R=100){
  MSE<-matrix(-99, R, 1)
  #sample with replacement  
  for ( i in 1:R){
    ss<-sample(nrow(data), 200, replace=TRUE)
    sam<-data[ss,]
    test<-data[-ss,]
    M1<-rpart(sam$medv~., sam)
    pre<-predict(M1, test)
    MSE[i]<-mean((test$medv-pre)^2)
  }
  return(list("Mean SE"= mean(MSE), "Lower CI"=quantile(MSE, p=0.05), "Upper CI"=quantile(MSE, p=0.95)))
}
bag.fn(Boston)


# Example 2 - Applying bagging to a regression problem


library(randomForest )
data("Boston")
set.seed(1234)# can be any number 
bag.Rtree<-randomForest(medv ~., data=Boston, mtry=13, importance=TRUE)
#setting mtry = number of predictors to get bagging results
print(bag.Rtree)
res<-Boston$medv-bag.Rtree$predicted
plot(Boston$medv, res)
bag.Rtree$importance 


# Example 3 - Classification problem

library("faraway")
set.seed(1234)# can be any number 
data(kanga)
set.seed(1234)
bag.ctree<-randomForest(sex~., mtry=15,  data=kanga, na.action = na.omit, importance=TRUE)
print(bag.ctree)
kanga3<-kanga
for ( i in 2:ncol(kanga)){
  kanga3[,i]<- na.roughfix(kanga[,i])
}
set.seed(1234)
bag.ctree1<-randomForest(sex~., mtry=15,  data=kanga3, importance=TRUE)
print(bag.ctree1)

# missing values imputation

set.seed(1234)
kanga4<- rfImpute(sex~., kanga)

bag.ctree2<-randomForest(sex~., mtry=15,  data=kanga4, importance=TRUE)
print(bag.ctree2)

# variable importance


imp<-data.frame(importance(bag.ctree2, type=1))
par(mfrow=c(2,1), cex=0.4)
barplot(sort(bag.ctree2$importance[,3], decreasing = TRUE)[1:10])
barplot(sort(bag.ctree2$importance[,4], decreasing = TRUE)[1:10])


# Example 4 - Marketing customer analysis

setwd("C:/Users/david/OneDrive/University/Data Mining and Machine Learning/Week 3")
bank<-read.csv("bank.csv", sep=";")
str(bank)
bank$y <- as.factor(bank$y)
str(bank)
set.seed(1234)

#bagged tree
bank.bg<-randomForest(y ~., data=bank, mtry=16, importance=TRUE)

#random forest 
bank.rf<-randomForest(y ~., data=bank, mtry=5, importance=TRUE)

# predict
new<-read.csv("new_clients.csv")
str(new)
new$bg<-predict(bank.bg, newdata = new)
new$rf<-predict(bank.rf, newdata = new)
table(new$bg, new$rf)

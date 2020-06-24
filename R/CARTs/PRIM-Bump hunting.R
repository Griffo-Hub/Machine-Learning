library(prim)

# Regression example

data(Boston)
x<-Boston[, 1:(ncol(Boston)-1)]
y<-Boston$medv
b.prim<-prim.box(x,y,threshold.type=1)
summary(b.prim, print.box = TRUE)



x<-Boston[, c("rm", "lstat")]
y<-Boston$medv
b1.prim<-prim.box(x,y,threshold.type=1)
summary(b1.prim, print.box = TRUE)


# Classification Example

library(supervisedPRIM)
library(faraway)
data(kanga)
kanga1<-na.exclude(kanga)
x<-kanga1[, 3:(ncol(kanga1)-1)]
y<-factor(kanga1$sex)
k.prim<-supervisedPRIM(x,y,threshold.type=1)
summary(k.prim, print.box = TRUE)

yhat<-predict(k.prim, x,y.fun.flag=TRUE)

table(yhat, y)

# Example 3


library(supervisedPRIM)
data("Caravan", package="ISLR")

x<-Caravan[, 3:85]
y<-factor(Caravan$Purchase)
C.prim<-supervisedPRIM(x,y,threshold.type=1)

# The algorithm found three boxes had highest concentration of people purchased carvan insurance.

summary(C.prim, print.box = TRUE)

# prediction vs observation

yhat<-predict(C.prim, x,y.fun.flag=TRUE)
table(yhat, y)

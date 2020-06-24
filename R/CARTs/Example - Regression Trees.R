library(MASS)
data(Boston)
library(rpart)
# set the seed for the random number generator, this ensures we can reproduce the results showing here

set.seed(1234)
# Here we are treating the Boston data as the training set

# "." represents all preditors in the data set

# Without prior specification, the default stopping criteria in rpart is either

# node size < 5 or complexity parameter <0.01, whichever was reached first/

M1<-rpart(medv ~., data=Boston)

#Full tree - stopping criteria

par(xpd = TRUE)
plot(M1, compress = TRUE)
text(M1, digits=2,use.n = TRUE )


# shows the changes in the cross-validated error with tree size and complexity parameter
# The errors remain mostly unchanged when alphg >0.051, therefore we set alpha=0.051 in pruning. 
# The rpart manual suggests a good choice of alpha is the left-most value for which the mean of the error is below the dotted line.

plotcp(M1)


M2<-prune(M1, cp=0.051)
par(xpd = TRUE)
plot(M2, compress = TRUE)
text(M2, digits=2, use.n = TRUE)

summary(M2)

# what variables are important for predicting the housing values.

barplot(M2$variable.importance, cex.names =0.8)

# test the goodness of fit is through examine the residuals.

par(mfrow=c(1,2))
plot(predict(M2),residuals(M2))
qqnorm(residuals(M2))
qqline(residuals(M2))


# Example 2

data(Hitters, package="ISLR")

M1<-rpart(Salary~., data=Hitters)
par(xpd = TRUE)
plot(M1, compress = TRUE)
text(M1, digits=2,use.n = TRUE)

plotcp(M1)

M2<-prune(M1, cp=0)
par(xpd = TRUE)
plot(M2, compress = TRUE)
text(M2, digits=2, use.n = TRUE)
barplot(M2$variable.importance)



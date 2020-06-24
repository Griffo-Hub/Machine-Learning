setwd("C:/Users/david/OneDrive/University/Data Mining and Machine Learning/Week 5")
data <- read.csv("dividends.csv")
str(data)
head(data)
apply(data, 2, function(x) sum(is.na(x)))

# normalise data (can also use scale function)
normalize <- function(x) { 
  return ((x - min(x)) / (max(x) - min(x))) 
} 
maxmindf <- as.data.frame(lapply(data, normalize))

# Training and Test Data
trainset <- maxmindf[1:160, ]
testset <- maxmindf[161:200, ]

#Neural Network
library(neuralnet)
nn <- neuralnet(dividend ~ fcfps + earnings_growth + de + mcap + current_ratio, 
                data=trainset, hidden=c(2,1), linear.output=FALSE, threshold=0.01)
nn$result.matrix
plot(nn)

#Test the resulting output
temp_test <- subset(testset, select = c("fcfps","earnings_growth", "de", "mcap", "current_ratio"))
head(temp_test)
nn.results <- compute(nn, temp_test)
results <- data.frame(actual = testset$dividend, prediction = nn.results$net.result)

# Test accuracy confusion matrix
roundedresults<-sapply(results,round,digits=0)
roundedresultsdf=data.frame(roundedresults)
attach(roundedresultsdf)
table(actual,prediction)


#Neural Network 2 - 1,1, hidden layer
library(neuralnet)
nn2 <- neuralnet(dividend ~ fcfps + earnings_growth + de + mcap + current_ratio, 
                data=trainset, hidden=c(1,1), linear.output=FALSE, threshold=0.01)
nn2$result.matrix
plot(nn2)

#Test the resulting output 1,1 hidden layer
nn2.results <- compute(nn2, temp_test)
results2 <- data.frame(actual = testset$dividend, prediction = nn2.results$net.result)

# Test accuracy confusion matrix 1,1 hidden layer
roundedresults2<-sapply(results,round,digits=0)
roundedresultsdf2=data.frame(roundedresults2)
attach(roundedresultsdf2)
table(actual,prediction)

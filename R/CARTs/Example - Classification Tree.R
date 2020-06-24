library(faraway)
data(kanga)
names(kanga)
head(kanga)
str(kanga)

library(tidyverse)

# cobine sex and species into a single column

kanga1<- kanga%>% unite("SS", c("species","sex")) 
str(kanga1)

class(kanga1)
# look for NA's

#number of rows with missing value

nrow(kanga1[rowSums(is.na(kanga1))>=1,])

# breakdown of NA's by variable

apply(kanga1, 2, function(x) sum(is.na(x)))

set.seed(1234)
M1<-rpart(SS~., data=kanga1)
par(xpd = TRUE)
plot(M1, compress = TRUE)
text(M1, digits=2, cex=0.8)

# prune the full tree to avoid overfitting. We will see how cp changes with the size of the tree.

plotcp(M1)

# Because cp=0.02 is the left-most value under the dotted line, we set cp=0.02 in the cost complexity pruning.

M2<-prune(M1, cp=0.02)
class(M2)
par(xpd = TRUE)
plot(M2, compress = TRUE)
text(M2, digits=2,cex=0.5)

# How good is this tree in predicting the kangaroo species?

pre<-predict(M2, kanga1, type="class")
tab<-table(pre, kanga1$SS)
tab

# which variables were important in classifying the three kangaroo species.

barplot(M2$variable.importance, cex.names =0.8, las=2)

# According to the variable importance ranking provided by CARTs, 
#the most important predictor for predicting kangaroo species is the nasal length, 
#followed by the occipitonasal length and basilar length. 
#The relative importance, however, did not differ greatly for the seven most important variables, 
#which suggests these predictors are likely correlated. We can check this by estimating the Pearson correlation.

d1<- kanga1%>% select("nasal.length", "palate.length","occipitonasal.length", 
                      "ramus.height","basilar.length","zygomatic.width", "lacrymal.width")
round(cor(d1, use = "pairwise.complete.obs"), 2)

# Example 2

setwd("C:/Users/david/OneDrive/University/Data Mining and Machine Learning/Week 3")

bank<-read.csv("bank.csv", sep=";")
M1<-rpart(y~., data=bank)
par(xpd = TRUE)
plot(M1, compress = TRUE)
text(M1, digits=2,use.n = TRUE)

plotcp(M1)

M2<-prune(M1, cp=0.031)
par(xpd = TRUE)
plot(M2, compress = TRUE)
text(M2, digits=2, use.n = TRUE)
barplot(M2$variable.importance, cex.names =0.8, las=1.5)
# Logicstic Regression - Use when predictors and numeric and dependant variable is binary

set.seed(0)
x0_10 <- runif(10, min=0, max=60); 
x11_20 <- runif(10, min=40, max=100)
y0_10 <- rep(-1,10); 
y11_20 <- rep(+1,10)
( dat <- data.frame(x=c(x0_10,x11_20), y=c(y0_10,y11_20)) )

# plot

library(ggplot2)
class <- as.factor(dat$y)
( g <- ggplot() + geom_point(data=dat, mapping=aes(x=x, y=y, shape=class)) )

# linear model

linear_model <- lm(y ~., data=dat)
new_dat <- data.frame(x=seq(from=0, to=100, by=0.1))
Y_hat_new_dat <- predict.lm(linear_model, newdata=new_dat)
g2 <- g + geom_point(data=data.frame(x=new_dat, y=Y_hat_new_dat), mapping=aes(x=x, y=y))
Y_hat_dat <- predict.lm(linear_model, newdata=NULL)
Y_dat <- sign(Y_hat_dat)
( g2 <- g2 + geom_point(data=dat, mapping=aes(x=x, y=y, shape=class, colour=Y_dat), show.legend=FALSE) )
 linear_model

 # Logistic model
 
 LR_fun <- function(v){1/(1 + exp(-v))}
 P_positive_new_dat <- LR_fun(Y_hat_new_dat)
 g3 <- g + geom_point(data=data.frame(x=new_dat, y=P_positive_new_dat), mapping=aes(x=x, y=y))
 P_positive_dat <- LR_fun(Y_hat_dat)
 Y_dat <- sign(P_positive_dat - 0.5)
 ( g3 <- g3 + geom_point(data=dat, mapping=aes(x=x, y=y, shape=class, colour=Y_dat), show.legend=FALSE) )
 
 
 # train data with logicstic model instead of initial linear model
 
 dat <- cbind(dat, class)
 LR_model <- glm(formula=class~x, data=dat, family=binomial)
 P_positive_new_dat <- predict.glm(LR_model, newdata=new_dat, type="response")
 g4 <- g + geom_point(data=data.frame(x=new_dat, y=P_positive_new_dat), mapping=aes(x=x, y=y))
 P_positive_dat <- predict(LR_model, type="response")
 Y_dat <- sign(P_positive_dat - 0.5)
 ( g4 <- g4 + geom_point(data=dat, mapping=aes(x=x, y=y, shape=class, colour=Y_dat), show.legend=FALSE) )
 
 
 # Example
 
 Data <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data",
                    header = FALSE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "",
                    na.strings =  "?")
 names(Data) <- c("Gender", "Age", "MonthlyExpenses", "MaritalStatus", "HomeStatus", "Occupation",
                  "BankingInstitution", "YearsEmployed", "NoPriorDefault", "Employed", "CreditScore",
                  "DriversLicense", "AccountType", "MonthlyIncome", "AccountBalance", "Approved") 

 Data <- na.omit(Data) # Remove rows with NA values
 numeric_predictors <- c("Age","MonthlyExpenses","YearsEmployed","CreditScore","MonthlyIncome","AccountBalance")
 set.seed(0)
 no_obs <- dim(Data)[1]
 test_index <- sample(no_obs, size = as.integer(no_obs*0.2), replace = FALSE) # 20% data records for test
 test_predictors <- Data[test_index, numeric_predictors]
 test_class_labels <- Data[test_index, "Approved"]
 training_index <- -test_index # 80% data records for training
 training_dat <- Data[training_index, c(numeric_predictors, "Approved")]

 # Solution for logistic model
 
LR_CRX <- glm(formula=Approved~., data=training_dat, family=binomial)
summary(LR_CRX)
P_positive_test <- predict.glm(LR_CRX, newdata=test_predictors, type="response")
Pred_class <- sign(P_positive_test - 0.5)
tab <- table(Pred_class, test_class_labels)
accuracy <- sum(diag(tab))/sum(tab)
accuracy
summary(LR_CRX)


# Exercise 2

LR_CRX <- glm(formula=Approved ~ YearsEmployed+CreditScore+AccountBalance, data=training_dat, family=binomial)
summary(LR_CRX)
P_positive_test <- predict.glm(LR_CRX, newdata=test_predictors, type="response")
Pred_class <- sign(P_positive_test - 0.5)
tab <- table(Pred_class, test_class_labels)
accuracy <- sum(diag(tab))/sum(tab)
accuracy
summary(LR_CRX)
 
  
 
 
P_positive_test
 
 
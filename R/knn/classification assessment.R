# Classification Assessment
# K-Fold cross validation
# data
library(class)
set.seed(0)
x0_10 <- runif(10, min=0, max=60)
x11_20 <- runif(10, min=40, max=100)
y0_10 <- rep(-1,10)
y11_20 <- rep(+1,10)
dat <- data.frame(x=c(x0_10,x11_20), y=c(y0_10,y11_20))

# perform cross validation (LOOCV default)

predictors <- iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
class_labels <- iris[, "Species"]
Pred_class <- knn.cv(train=predictors, cl=class_labels, k=1)
cont_tab <- table(Pred_class, class_labels)
cont_tab
accuracy <- sum(diag(cont_tab))/sum(cont_tab)

# perform cross validation 1 - 20
accuracy <- rep(0,20)
predictors <- iris[, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
class_labels <- iris[, "Species"]
for (k in 1:20){
  Pred_class <- knn.cv(train=predictors, cl=class_labels, k=k)
  cont_tab <- table(Pred_class, class_labels)
  accuracy[k] <- sum(diag(cont_tab))/sum(cont_tab)
}
plot(accuracy)

# ROC
library(ROCR)
rocplot <- function(pred, truth){
  predobj <- prediction(pred, truth)
  ROC     <- performance(predobj, "tpr", "fpr")
  plot(ROC)   # Plot the ROC Curve
  auc     <- performance(predobj, measure = "auc")
  auc     <- auc@y.values[[1]]
  return(auc) # Return the Area Under the Curve ROC
}
AUC <- rocplot(pred=predictors, truth=class_labels)
AUC

# Practice exercise 4

crx.data <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/credit-screening/crx.data",
                       header = FALSE, sep = ",", quote = "\"", dec = ".", fill = TRUE, comment.char = "",
                       na.strings =  "?")
names(crx.data) <- c("Gender", "Age", "MonthlyExpenses", "MaritalStatus", "HomeStatus", "Occupation",
                     "BankingInstitution", "YearsEmployed", "NoPriorDefault", "Employed", "CreditScore",
                     "DriversLicense", "AccountType", "MonthlyIncome", "AccountBalance", "Approved") 
predictors <- crx.data[, c("YearsEmployed", "CreditScore", "AccountBalance")]
predictors <- as.matrix(scale(predictors)) # Variable scaling required by KNN
class_labels <- crx.data[, "Approved"]
for(k in 1:20){
  Pred_class <- knn.cv(train=predictors, cl=class_labels, k=k, prob=TRUE)
  Pred_prob <- attr(Pred_class, "prob")
  Pred_prob <- ifelse(Pred_class=='+', Pred_prob, 1 - Pred_prob) # Make sure probabilities are for class "+"
  AUC[k] <- rocplot(pred=Pred_prob, truth=class_labels)
}
plot(AUC)


# Assessment 1 - Feature selection 
# Example Code

library(naivebayes)
Mushrooms <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/mushroom/aga
ricus-lepiota.data", header=FALSE, sep=",", dec=".", na.strings=c("?"))
summary(Mushrooms)

# try with more training and test sets
set.seed(0)
no_observations <- dim(Mushrooms)[1] # No. observations (8124)
no_predictors <- dim(Mushrooms)[2] - 1 # No. predictors (22) = No. variables (23) - dependent var. (1st column)
error <- 0
for (i in 1:10){
  test_index <- sample(no_observations, size=as.integer(no_observations*0.2), replace=FALSE)
  # 20% data for test
  training_index <- -test_index # Remaining 80% data observations for training
  NaiveBayesModel <- naive_bayes(V1 ~. , data = Mushrooms[training_index, ])
  Pred_class <- predict(NaiveBayesModel, newdata = Mushrooms[test_index, ])
  tab <- table(Pred_class, Mushrooms[test_index,"V1"])
  accuracy <- sum(diag(tab))/sum(tab)
  error <- error + (1 - accuracy)
}
error
m_error <- error/10
m_error

# Assignment 1 - Wrapper Function

#rm(list=ls())

suppressWarnings(RNGkind(sample.kind="Rounding"))

calc_error <- function(f){
  # Reset error
  error <- 0
  
  # Calculate error
  for (i in 1:10){
    # 20% data for test
    test_index <- sample(no_observations, size=as.integer(no_observations*0.2), replace=FALSE)
    
    # Remaining 80% data observations for training
    training_index <- -test_index
    
    # Train and test model
    NaiveBayesModel <- naive_bayes(f, data = Mushrooms[training_index, ])
    Pred_class <- predict(NaiveBayesModel, newdata = Mushrooms[test_index, ])
    tab <- table(Pred_class, Mushrooms[test_index,"V1"])
    accuracy <- sum(diag(tab))/sum(tab)
    error <- error + (1 - accuracy)
  }
  # Return average error for each predictor/s
  return (error/10)
}

set.seed(0)
no_observations <- dim(Mushrooms)[1] # No. observations (8124)
no_predictors <- dim(Mushrooms)[2] - 1 # No. predictors (22) = No. variables (23) - dependent var. (1st column)

# initialise vectors
min.pred.name <- c()
min.pred.error <- c()
min.pred.id <- c()
col.names <- colnames(Mushrooms)[-1]

# calculate error rate for each predictor
for(p in 1:length(col.names)){
  store.error <- c()
  for (k in 1:length(col.names)){
    # validate k value - remove before submitting code
    f.string <- c(min.pred.name, col.names[k])
    
    # create input string for naive_bayes function
    f = as.formula(paste("V1", paste(f.string, collapse = " + "), sep = " ~ "))
    
    # Return error for features based on calc_error function
    error <- calc_error(f)
    
    # Store error data for each [k]
    store.error[k] <- error
  }
  # Find id of predictor 
  min.pred.id <- which.min(store.error)
  min.pred.name[p] <- col.names[min.pred.id]
  min.pred.error[p] <- min(store.error)
  col.names <- col.names[!col.names %in% min.pred.name]
  print(min.pred.name)
}


plot(min.pred.error, ylim=c(0, 0.05), xlab='predictors')


min.pred.error


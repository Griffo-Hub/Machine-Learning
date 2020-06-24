library(class)
library(stats)


# prep data by removing undesired predictors, separating the remaining ones from the dependent variable

new_iris_predictors <- iris[, c("Sepal.Width", "Petal.Width")]
new_iris_class <- iris[, "Species"]

# find target observation


target <- which((iris$Sepal.Width==2.5)&(iris$Petal.Width==1.5))

# remove target from data

new_iris_test <- new_iris_predictors[target,]
new_iris_predictors <- new_iris_predictors[-target,]
new_iris_class <- new_iris_class[-target]

# perform kNN on the test observation (target)

knn(train = new_iris_predictors, test = new_iris_test, cl = new_iris_class, k = 1, prob = TRUE)
knn(train = new_iris_predictors, test = new_iris_test, cl = new_iris_class, k = 3, prob = TRUE)
knn(train = new_iris_predictors, test = new_iris_test, cl = new_iris_class, k = 5, prob = TRUE)
knn(train = new_iris_predictors, test = new_iris_test, cl = new_iris_class, k = 7, prob = TRUE)

# Exercise 1

D <- as.matrix(dist(iris[, c("Sepal.Width", "Petal.Width")]))
D
D_target <- D[target,]
D_target
D_target_sorted <- D_target[order(D_target)]
D_target_sorted[1:10]

# Exercise 2
# set up training and test data
set.seed(0)
no_obs <- dim(iris)[1] # No. of observations (150)
test_index <- sample(no_obs, size = as.integer(no_obs*0.2), replace = FALSE) # 20% data records for test
test_predictors <- iris[test_index, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
test_class <- iris[test_index, "Species"]
training_index <- -test_index # 80% data records for training
training_predictors <- iris[training_index, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
training_class <- iris[training_index, "Species"]

# use kNN

Pred_class <- knn(train=training_predictors, test=test_predictors, cl=training_class, k=1)
cont_tab <- table(Pred_class, test_class)
cont_tab

# loop 10 times
set.seed(0)
accuracy <- rep(0,10)
for (i in 1:10) {
test_index <- sample(no_obs, size = as.integer(no_obs*0.2), replace = FALSE) # 20% data records for test
test_predictors <- iris[test_index, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
test_class <- iris[test_index, "Species"]
training_index <- -test_index # 80% data records for training
training_predictors <- iris[training_index, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
training_class <- iris[training_index, "Species"]
cont_tab <- table(Pred_class, test_class)
(accuracy[i] <- sum(diag(cont_tab))/sum(cont_tab))
}
accuracy
mean(accuracy)
sd(accuracy)

# Exercise 3 - try K 1:20

set.seed(0)
mean_accuracy <- rep(0,20)
for(k in 1:20){
  accuracy <- rep(0,10)
  for(i in 1:10){
    test_index <- sample(no_obs, size = as.integer(no_obs*0.2), replace = FALSE) # 20% data records for test
    test_predictors <- iris[test_index, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
    test_class <- iris[test_index, "Species"]
    training_index <- -test_index # 80% data records for training
    training_predictors <- iris[training_index, c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width")]
    training_class <- iris[training_index, "Species"]
    Pred_class <- knn(train=training_predictors, test=test_predictors, cl=training_class, k=1)
    cont_tab <- table(Pred_class, test_class)
    accuracy[i] <- sum(diag(cont_tab))/sum(cont_tab)
  }
  mean_accuracy[k] <- mean(accuracy)
}
plot(mean_accuracy)




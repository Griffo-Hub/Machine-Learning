library (MASS)
set.seed(0)
no_obs <- dim(iris)[1] # No. of observations (150)
test_index <- sample(no_obs, size = as.integer(no_obs*0.2), replace = FALSE) # 20% data records for test
training_index <- -test_index # 80% data records for training
qda.fit <- qda(Species ~., data=iris, subset = training_index)
qda.pred <- predict(qda.fit, iris[test_index,])
qda.Pred_class <- qda.pred$class
cont_tab <- table(qda.Pred_class, iris$Species[test_index])
cont_tab
library(keras)
mnist <- dataset_mnist()
train_images <- mnist$train$x
train_labels <- mnist$train$y
test_images <- mnist$test$x
test_labels <- mnist$test$y

train_images <- array_reshape(train_images, c(60000, 28 * 28))
train_images <- train_images / 255

test_images <- array_reshape(test_images, c(10000, 28 * 28))
test_images <- test_images / 255


train_y_mtx <- keras::to_categorical(train_labels)
test_y_mtx <- keras::to_categorical(test_labels)


network <- keras_model_sequential() %>%
  layer_dense(units = 100, activation =  "relu", input_shape = c(28*28)) %>%
  layer_dense(units = 10, activation =  "softmax")

network %>% compile(
  optimizer = "adam",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

ModelHistory <- network %>%
  fit(train_images, train_y_mtx,
      epochs = 20,
      batch_size = 50,
      validation_data = list(test_images, test_y_mtx) )


plot(ModelHistory) 

# Import libraries

library(readxl)
library(mice)
library(ggplot2)
library(e1071)
library(lubridate)
library(dplyr)
library(reshape2)
library(gridExtra)
library(corrplot)
library(Hmisc)
library(neuralnet)

# read in unemployment data file

unemployment_data <- read_excel("AUS_Data.xlsx")

# additional employment data

employment_data <- read_excel("ABS_Employment_Data.xlsx")
str(employment_data)
colnames(employment_data)[2] <- "employed_total_persons_000s"
employment_data <- data.frame(employment_data)


# Explore data

head(unemployment_data)

# remove names from original data

unemployment_data <- unemployment_data[-1,]

# summary 

summary(unemployment_data)
str(unemployment_data)
sapply(unemployment_data, class)

# rename and format columns consistently 

colnames(unemployment_data)[1] <- "period"
colnames(unemployment_data)[2] <- "unemployment_rate"
colnames(unemployment_data)[3] <- "gdp"
colnames(unemployment_data)[4] <- "general_final_consumption_expenditure"
colnames(unemployment_data)[5] <- "all_final_consumption_expenditure"
colnames(unemployment_data)[6] <- "terms_of_trade_index"
colnames(unemployment_data)[7] <- "cpi"
colnames(unemployment_data)[8] <- "job_vacancies"
colnames(unemployment_data)[9] <- "estimated_res_population"


names(unemployment_data)

# Make backup of data for plotting prior to stripping date data values to their own columns

unemployment_data_original <- unemployment_data

# convert char columns to num

unemployment_data$job_vacancies <- as.numeric(as.character(unemployment_data$job_vacancies))
unemployment_data$estimated_res_population <- as.numeric(as.character(unemployment_data$estimated_res_population))

# Look for NA's

apply(unemployment_data, 2, function(x) sum(is.na(x)))

# remove NA's

unemployment_data <- na.omit(unemployment_data)

# Look for NA's

apply(unemployment_data, 2, function(x) sum(is.na(x)))

# Make a copy for plotting period

unemployment_data2 <- unemployment_data

# Plots

# employment

employment_plot <- ggplot(employment_data, aes(x = Period, y = employed_total_persons_000s)) +
  geom_line(color = "#E69F00") + 
  labs(x = "Period", y = "Employment Rate (thousands)") + 
  ggtitle("Australian employment Rate 1978 - 2020") +
  geom_hline(yintercept = c(max(employment_data$employed_total_persons_000s), 
                            min(employment_data$employed_total_persons_000s)), 
             color = 'dodgerblue2', linetype = "dashed") +
  theme_light()



# Unemployment

unemployment_plot <- ggplot(unemployment_data, aes(x = period, y = unemployment_rate)) +
  geom_line(color = "#E69F00") + 
  labs(x = "Period", y = "Unemployment Rate %") + 
  ggtitle("Australian Unemployment Rate 1980 - 2020") +
  geom_hline(yintercept = c(max(unemployment_data$unemployment_rate), 
                            min(unemployment_data$unemployment_rate)), 
             color = 'dodgerblue2', linetype = "dashed") +
  theme_light()

# Job Vacancies

job_vacancy_plot <- ggplot(unemployment_data, aes(x = period, y = job_vacancies)) +
  geom_line(color = "#E69F00") + 
  labs(x = "Period", y = "Job Vacancy Rate (In Thousands)") + 
  ggtitle("Australian Job Vacancy Rate (In Thousands) 1980 - 2020") +
  geom_hline(yintercept = c(max(unemployment_data$job_vacancies), 
                            min(unemployment_data$job_vacancies)), 
             color = 'dodgerblue2', linetype = "dashed") +
  theme_light()

# GDP & Unemployment

data <- unemployment_data[c(1,2,3)]
data <- melt(data,"period")
x <- data[250,1]
head(data)
gdp_unemployment_plot <- ggplot(data, aes(x = period, y = value, group = variable)) +
  geom_line(aes(color = variable)) + 
  labs(x = "Period", y = "%") + 
  ggtitle("Australian Unemployment Rate & GDP 1980 - 2020") +
  geom_vline(xintercept=x, colour="dodgerblue2", linetype = "dashed") +
  geom_text(data=data, mapping=aes(x=x, y=0, label="GDP Decrease"), 
            size=4, angle=360, vjust=-5.4, hjust=-0.1) +
  theme_light()

# display

employment_plot
unemployment_plot
job_vacancy_plot
gdp_unemployment_plot

# check data distribution

par(mfrow=c(2,4))

plot(density(unemployment_data$unemployment_rate))
plot(density(unemployment_data$gdp))
plot(density(unemployment_data$general_final_consumption_expenditure))
plot(density(unemployment_data$all_final_consumption_expenditure))
plot(density(unemployment_data$terms_of_trade_index))
plot(density(unemployment_data$cpi))
plot(density(unemployment_data$estimated_res_population))

par(mfrow=c(1,1))

# convert date to columns

unemployment_data <- unemployment_data %>%
  dplyr::mutate(period_year = lubridate::year(period), 
                period_month = lubridate::month(period), 
                period_day = lubridate::day(period))

str(unemployment_data)

# remove day column as its irrelevant to this problem as all data is captured at a quarter year interval on the same day

unemployment_data <- unemployment_data[-12]

# Pearson correlation with sig levels

cor <- cor(unemployment_data[,2:11], method = "pearson")


# Positive correlations are displayed in blue and negative correlations in red color. 
# Color intensity and the size of the circle are proportional to the correlation coefficients. 
# In the right side of the correlogram, the legend color shows the correlation coefficients and the corresponding colors.

#corrplot(cor, type = "upper", order = "hclust", 
#         tl.col = "black", tl.srt = 45)

# correlation heatmap

col<- colorRampPalette(c("red", "white", "blue"))(20)
heatmap(x = cor, col = col, symm = TRUE)


# scale data and drop period column as we have separated period data to own columns

unemployment_data <- unemployment_data[-1]


# Scale and Rescale Functions

# The scale function centers (subtracts mean value), and then scales (divides by standard deviation of data)

unemployment_data_scaled <- scale(unemployment_data, center=TRUE,scale=TRUE)

unemployment_data_scaled <- data.frame(unemployment_data_scaled)

############## SVM ##############

# Create training and test data sets

set.seed(112)
training_data <- unemployment_data_scaled[1:129,]
test_data <- unemployment_data_scaled[130:147,]

set.seed(112)
svm_model <- svm(unemployment_rate~., data = training_data)
svm_predict_training <- predict(svm_model, training_data)

# Error rate on training data (root mean squared error RMSE)

rmse <- function(errval)
{
  val = sqrt(mean(errval^2))
  return(val)
}

svm_error <- svm_predict_training - training_data$unemployment_rate
svm_RMSE <- rmse(svm_error)
svm_RMSE

# Tune the model and test

# Now use on test data

set.seed(112)
svm_model_test <- svm(unemployment_rate~., data = training_data)
svm_predict_test <- predict(svm_model_test, test_data)

# RMSE Error Rate

error_test <- svm_predict_test - test_data$unemployment_rate
svm_RMSE_test <- rmse(error_test)
svm_RMSE_test

# Try different cost to see if model accuracy can be further improved

costs <- c("1", "5", "10", "50", "100", "500", "1000", "1500", "2000")
svm_model_cost <- list()
predict_test_cost <- list()
error_cost <- NULL
svm_RMSE_cost <- NULL
i = 0

for (cost in costs) {
  
  print(cost)
  svm_model_cost <- svm(unemployment_rate~., data = training_data, cost = cost)
  predict_test_cost <- predict(svm_model_cost, test_data)
  
  # store errors
  
  error_cost[i] <- predict_test_cost - test_data$unemployment_rate
  svm_RMSE_cost[i] <- rmse(error_cost)
  i = i+1
  
}

cost_error_df <- data.frame(cbind(costs, svm_RMSE_cost))
colnames(cost_error_df)[2] <- "error_rate"
cost_error_df$error_rate <- as.factor(cost_error_df$error_rate)


cost_error_df <- cost_error_df %>%
  arrange(error_rate)

# plot the error rate of the costs

ggplot(cost_error_df, aes(x = costs, y = error_rate, fill = costs)) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette="RdBu") +
theme_minimal()

# Try different epsilons to see if model accuracy can be further improved

epsilons <- c(0, 0.0001, 0.001, 0.01, 0.1, 1, 2, 3)
svm_model_epsilon <- list()
predict_test_epsilon <- list()
error_epsilon <- NULL
svm_RMSE_epsilon <- NULL
i = 0

for (epsilon in epsilons) {
  
  print(epsilon)
  svm_model_epsilon <- svm(unemployment_rate~., data = training_data, cost = 5, 
                           epsilon = epsilon)
  predict_test_epsilon <- predict(svm_model_epsilon, test_data)
  
  # store errors
  
  error_epsilon[i] <- predict_test_epsilon - test_data$unemployment_rate
  svm_RMSE_epsilon[i] <- rmse(error_epsilon)
  i = i+1
  
}

epsilon_error_df <- data.frame(cbind(epsilons, svm_RMSE_epsilon))
colnames(epsilon_error_df)[2] <- "error_rate"
epsilon_error_df$error_rate <- as.factor(epsilon_error_df$error_rate)
epsilon_error_df$epsilons <- as.factor(epsilon_error_df$epsilons)

epsilon_error_df <- epsilon_error_df %>%
  arrange(error_rate)

# plot error rates for each epsilon value tested

ggplot(epsilon_error_df, aes(x = epsilons, y = error_rate, fill = epsilons)) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette="RdBu") +
theme_minimal()


# Try different kernals to see if model accuracy can be further improved

kernels <- c("radial", "linear", "polynomial", "sigmoid")
svm_model_kernel <- list()
predict_training_kernel <- list()
error_kernel <- NULL
svm_RMSE_kernel <- NULL

for (kernel in kernels) {
  svm_model_kernel <- svm(unemployment_rate~., data = training_data, cost = 5, 
                          kernel = kernel, epsilon = 0.01)
  predict_training_kernel <- predict(svm_model_kernel, test_data)
  
  # store errors
  
  error_kernel[kernel] <- predict_training_kernel - test_data$unemployment_rate
  svm_RMSE_kernel[kernel] <- rmse(error_kernel)
  
}

kernel_error_df <- data.frame(cbind(kernels, svm_RMSE_kernel))
colnames(kernel_error_df)[2] <- "error_rate"
kernel_error_df$kernels <- as.factor(kernel_error_df$kernels)
kernel_error_df$error_rate <- as.factor(kernel_error_df$error_rate)

kernel_error_df <- kernel_error_df %>%
  arrange(error_rate)

ggplot(kernel_error_df, aes(x = kernels, y = error_rate, fill = kernels)) + 
  geom_bar(stat = "identity") +
  scale_fill_brewer(palette="Pastel2") +
theme_minimal()


################ Tuned model ##############

set.seed(112)
svm_model_test <- svm(unemployment_rate~., data = training_data, cost = 5, kernel = "linear", epsilon = 0.01)
svm_predict_test <- predict(svm_model_test, test_data)

# RMSE Error Rate

error_test <- svm_predict_test - test_data$unemployment_rate
svm_RMSE_test <- rmse(error_test)
svm_RMSE_test


# unscale the data for plotting in reference to original data

unscaled_svm_pred <- svm_predict_test * sd(unemployment_data$unemployment_rate) + mean(unemployment_data$unemployment_rate)

# Plot predictions for 2015 - 2019 vs Actuals

svm_pred_df <- cbind(unemployment_data2[130:147,1],unemployment_data[130:147,1], unscaled_svm_pred)
colnames(svm_pred_df)[2] <- "Actual"
colnames(svm_pred_df)[3] <- "Prediction"


# Acual vs Predicted (SVM)

svm_pred_df <- melt(svm_pred_df,"period")
head(data)
pred_vs_actual_plot <- ggplot(svm_pred_df, aes(x = period, y = value, group = variable)) +
  geom_line(aes(color = variable)) + 
  labs(x = "Period", y = "Unemployment Rate") + 
  ggtitle("Predicted Vs Actual Unemployment Rates - Support Vector Regression (SVR)") +
  theme_light()
pred_vs_actual_plot


################### Neural Net ###################

# Training and tuning

set.seed(112)
nn_training <- neuralnet(unemployment_rate ~ ., 
                         data=training_data, 
                         hidden=c(2,1), 
                         linear.output=TRUE, 
                         threshold=0.01,
                         learningrate = 0.01,
                         stepmax = 1e7)
nn_predict_training <- predict(nn_training, training_data)
nn_error_training <- nn_predict_training - training_data$unemployment_rate
nn_RMSE_training <- rmse(nn_error_training)
nn_RMSE_training

# Testing

set.seed(112)
nn_testing <- neuralnet(unemployment_rate ~ ., 
                        data=training_data, 
                        hidden=c(2,1), 
                        linear.output=TRUE, 
                        threshold=0.01,
                        learningrate = 0.01,
                        stepmax = 1e7)
nn_predict_test <- predict(nn_testing, test_data)
nn_error_test <- nn_predict_test - test_data$unemployment_rate
nn_RMSE_test <- rmse(nn_error_test)
nn_RMSE_test

# Tuning - hidden Layers - trial and error


set.seed(112)
nn_tune_z <- neuralnet(unemployment_rate ~ ., 
                        data=training_data, 
                        hidden=c(3,1), 
                        linear.output=TRUE, 
                        threshold=0.01,
                        learningrate = 0.001)
nn_predict_tune_z <- predict(nn_tune_z, test_data)
nn_error_tune_z <- nn_predict_tune_z - test_data$unemployment_rate
nn_RMSE_tune_z <- rmse(nn_error_tune_z)
nn_RMSE_tune_z


set.seed(112)
nn_tune1 <- neuralnet(unemployment_rate ~ ., 
                      data=training_data, 
                      hidden=c(16,8), 
                      linear.output=TRUE, 
                      threshold=0.01,
                      learningrate = 0.001)
nn_predict_tune1 <- predict(nn_tune1, test_data)
nn_error_tune1 <- nn_predict_tune1 - test_data$unemployment_rate
nn_RMSE_tune1 <- rmse(nn_error_tune1)
nn_RMSE_tune1

set.seed(112)
nn_tune2 <- neuralnet(unemployment_rate ~ ., 
                      data=training_data, 
                      hidden=c(64,32), 
                      linear.output=TRUE, 
                      threshold=0.01,
                      learningrate = 0.001)
nn_predict_tune2 <- predict(nn_tune2, test_data)
nn_error_tune2 <- nn_predict_tune2 - test_data$unemployment_rate
nn_RMSE_tune2 <- rmse(nn_error_tune2)
nn_RMSE_tune2


set.seed(112)
nn_tune3 <- neuralnet(unemployment_rate ~ ., 
                      data=training_data, 
                      hidden=c(5,1), 
                      linear.output=TRUE, 
                      threshold=0.01,
                      learningrate = 0.001)
nn_predict_tune3 <- predict(nn_tune3, test_data)
nn_error_tune3 <- nn_predict_tune3 - test_data$unemployment_rate
nn_RMSE_tune3 <- rmse(nn_error_tune3)
nn_RMSE_tune3

# Another layer

set.seed(112)
nn_tune4 <- neuralnet(unemployment_rate ~ ., 
                      data=training_data, 
                      hidden=c(5,2,1), 
                      linear.output=TRUE, 
                      threshold=0.01,
                      learningrate = 0.001)
nn_predict_tune4 <- predict(nn_tune4, test_data)
nn_error_tune4 <- nn_predict_tune4 - test_data$unemployment_rate
nn_RMSE_tune4 <- rmse(nn_error_tune4)
nn_RMSE_tune4


# error rate for each hidden layer

hl_tune <- data.frame(rbind(nn_RMSE_tune_z, nn_RMSE_tune1, nn_RMSE_tune2, nn_RMSE_tune3, nn_RMSE_tune4))
colnames(hl_tune)[1] <- "error_rate"
rownames(hl_tune)[1] <- "2 Layers - (3,1)"
rownames(hl_tune)[2] <- "2 Layers - (16,8)"
rownames(hl_tune)[3] <- "2 Layers - (64,32)"
rownames(hl_tune)[4] <- "2 Layers - (5,1)"
rownames(hl_tune)[5] <- "3 Layers - (5,2,1)"

# activation function tuning

# sigmoid 

sigmoid <- function(x) {
  1 / (1 + exp(-x))
}

set.seed(112)
nn_tune6 <- neuralnet(unemployment_rate ~ ., 
                      data=training_data, 
                      hidden=c(5,1), 
                      linear.output=TRUE, 
                      threshold=0.01,
                      learningrate = 0.001,
                      act.fct = sigmoid)
nn_predict_tune6 <- predict(nn_tune6, test_data)
nn_error_tune6 <- nn_predict_tune6 - test_data$unemployment_rate
nn_RMSE_tune6 <- rmse(nn_error_tune6)
nn_RMSE_tune6

# Tanh

set.seed(112)
nn_tune7 <- neuralnet(unemployment_rate ~ ., 
                      data=training_data, 
                      hidden=c(5,1), 
                      linear.output=TRUE, 
                      threshold=0.01,
                      learningrate = 0.001,
                      act.fct = "tanh")
nn_predict_tune7 <- predict(nn_tune7, test_data)
nn_error_tune7 <- nn_predict_tune7 - test_data$unemployment_rate
nn_RMSE_tune7 <- rmse(nn_error_tune7)
nn_RMSE_tune7

# Softplus

softplus <- function(x) log(1+exp(x))

set.seed(112)
nn_tune8 <- neuralnet(unemployment_rate ~ ., 
                      data=training_data, 
                      hidden=c(5,1), 
                      linear.output=TRUE, 
                      threshold=0.01,
                      learningrate = 0.01,
                      act.fct = softplus)
nn_predict_tune8 <- predict(nn_tune8, test_data)
nn_error_tune8 <- nn_predict_tune8 - test_data$unemployment_rate
nn_RMSE_tune8 <- rmse(nn_error_tune8)
nn_RMSE_tune8

# Acual vs Predicted (NN)

# unscale the data for plotting in reference to original data

unscaled_nn_pred <- nn_predict_tune8 * sd(unemployment_data$unemployment_rate) + mean(unemployment_data$unemployment_rate)

# Plot predictions for 2015 - 2019 vs Actuals

nn_pred_df <- cbind(unemployment_data_original[130:147,1],unemployment_data[130:147,1], unscaled_nn_pred)
colnames(nn_pred_df)[2] <- "Actual"
colnames(nn_pred_df)[3] <- "Prediction"
nn_pred_df <- melt(nn_pred_df,"period")

pred_vs_actual_plot_nn <- ggplot(nn_pred_df, aes(x = period, y = value, group = variable)) +
  geom_line(aes(color = variable)) + 
  labs(x = "Period", y = "Unemployment Rate") + 
  ggtitle("Predicted Vs Actual Unemployment Rates - Neural Network") +
  theme_light()
pred_vs_actual_plot_nn

# plot the network

plot(nn_tune8, rep="best")

# comparison plot SVM & NN


comparison_df <- nn_pred_df
comparison_df$variable <- as.character(as.factor(comparison_df$variable))
comparison_df[19:36,2] <- "Neural Network"
comparison_df <- rbind(comparison_df, svm_pred_df[19:36,])
comparison_df[37:54,2] <- "SVR"
comparison_df$variable <- as.factor(comparison_df$variable)
str(comparison_df)
comparison_plot <- ggplot(comparison_df, aes(x = period, y = value, group = variable)) +
  geom_line(aes(color = variable)) + 
  labs(x = "Period", y = "Unemployment Rate") + 
  ggtitle("Neural Network vs SVR vs Actual") +
  theme_light()
comparison_plot
                      
                       







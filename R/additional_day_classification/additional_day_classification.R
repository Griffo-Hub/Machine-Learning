# Libraries
library(ggplot2)
library(class)
library(naivebayes)
library(psych)
library(dplyr)
library(reshape2)
library(ggplot2)
library (MASS)
library(dendextend)
library(knitr)
library(kableExtra)
library(dbscan)
library(factoextra)

# Get data

setwd("C:/Users/david.griffith/OneDrive/University/Data Mining/Assessment 4 - Capstone project")
data <- read.csv("capstone-data.csv")
# create copy of data for naive bayes using catagorical dataframe. original data will be converted to numerical
# create sample
set.seed(1)
data <- sample_n(data,2000)
data.orig <- data
str(data)

# separate class from predictors

data.class <- data$booking

# Explore data

head(data)

# NA's

sapply(data, function(x) sum(is.na(x)))


# plot data with class labels

## Create a frequency table

diagnosis.table <- table(data$booking)
colors <- terrain.colors(2) 
col <- c("#29507F", "#E2EFFF")

# Create a pie chart to view split of additional day variable

diagnosis.prop.table <- prop.table(diagnosis.table)*100
diagnosis.prop.df <- as.data.frame(diagnosis.prop.table)
pielabels <- sprintf("%s - %3.1f%s", diagnosis.prop.df[,1], diagnosis.prop.table, "%")
# plot pie
pie(diagnosis.prop.table,
    labels=pielabels,  
    clockwise=TRUE,
    col=col,
    border="gainsboro",
    radius=0.8,
    cex=0.8, 
    main="Frequency of additional day purchased")
legend(1, .4, legend=diagnosis.prop.df[,1], cex = 0.7, fill = col)


# Convert categorical data to numeric for clustering and explore data

# Pre processing

str(data)

data$booking <- as.integer(as.character(data$booking))
data$child_age <- as.numeric(as.character(data$child_age))
data$parent_age <- as.numeric(as.character(data$parent_age))
data$Opening_Hours <- as.factor(data$Opening_Hours)
data$OSHC_Capacity <- as.numeric(as.character(data$OSHC_Capacity))
data$numberofapprovals <- as.numeric(as.character(data$numberofapprovals))
data$Occupancy <- as.numeric(as.character(data$Occupancy))
data$Committment_Level <- as.numeric(as.character(data$Committment_Level))
data$Team_Turnover <- as.numeric(as.character(data$Team_Turnover))
data$NPS <- as.numeric(as.character(data$NPS))
data$CCS_Percentage <- as.numeric(as.character(data$CCS_Percentage))
str(data)

# remove NA's

sapply(data, function(x) sum(is.na(x)))
data <- na.omit(data)
sapply(data, function(x) sum(is.na(x)))


# dummy code variables that have more than 3 levels

Child_Status <- as.data.frame(dummy.code(data$Child_Status))
State <- as.data.frame(dummy.code(data$State))
Opening_Hours <- as.data.frame(dummy.code(data$Opening_Hours))
ACECQA_Rating <- as.data.frame(dummy.code(data$ACECQA_Rating))
Classification <- as.data.frame(dummy.code(data$Classification))
Parent_Employment_Activity <- as.data.frame(dummy.code(data$Parent_Employment_Activity))
incomegroup_groups <- as.data.frame(dummy.code(data$incomegroup_groups))

# bind dummy vars to data frame

data <- cbind(data, Child_Status, State, Opening_Hours, ACECQA_Rating, 
              Classification, Parent_Employment_Activity, incomegroup_groups)


# remove original columns

data <- data %>% 
  dplyr::select(-one_of(c("Child_Status", "Opening_Hours", "ACECQA_Rating", "State",
                   "Classification", "Parent_Employment_Activity", "incomegroup_groups")))

str(data)

# pairwise pearson correlation similarity matrix & heatmap

# pearson's correlation measure

correlation.matrix <- round(cor(data, method = "pearson"),2)
head(correlation.matrix)


# unpivot data using melt

melted.cormat <- melt(correlation.matrix)
head(melted.cormat)

# plot correlation matrix in heatmap

ggplot(data = melted.cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# normalise the numerical data in a new df

data.norm <- data

data.norm[, c("child_age", "parent_age", "LicenceNumber", "Capacity", "VAC_Capacity", "OSHC_Capacity", "numberofapprovals",
         "Occupancy", "Committment_Level", "Team_Turnover", "NPS",  "CCS_Percentage",
         "CCS_Withholding_Percentage", "Absence_count", "ACCS_Hourly_Rate_Cap_Increase_Percentage", 
         "CCS_Total_Hours_Per_Fortnight")] <- scale(data[, c("child_age", "parent_age", "LicenceNumber", "Capacity", "VAC_Capacity", "OSHC_Capacity", "numberofapprovals",
                                                             "Occupancy", "Committment_Level", "Team_Turnover", "NPS", "CCS_Percentage",
                                                             "CCS_Withholding_Percentage", "Absence_count", "ACCS_Hourly_Rate_Cap_Increase_Percentage", 
                                                             "CCS_Total_Hours_Per_Fortnight")], center=TRUE,scale=TRUE)


###################### Hierarchical clustering of original data #######################

matrix <- dist(data, method = "euclidean", diag = TRUE, upper = TRUE)

# hierarchical clustering

# complete

cl.orig <- hclust(matrix, method = "complete")
cl.dend <- as.dendrogram(cl.orig)
cl.dend %>% set("branches_k_color", k = 2) %>% plot(main = "Complete Linkage Original")
cl.dend
mean(cl.orig$height)

# ward's

ward.orig <- hclust(matrix, method = "ward.D2")
ward.dend <- as.dendrogram(ward.orig)
ward.dend %>% set("branches_k_color", k = 2) %>%
              set("labels", data.class) %>% plot(main = "Ward's Clustering Algorithm Original")
ward.dend
mean(ward.orig$height)


# plot(cl.orig, main = "Complete Linkage Original", xlab = "", sub = "", hang = -1, labels = FALSE)
# plot(ward.orig, main = "Ward's Clustering Algorithm Original", xlab = "", sub = "", hang = -1, labels = FALSE)


####################### Hierarchical clustering of normalised data #############################

matrix.norm <- dist(data.norm, method = "euclidean", diag = TRUE, upper = TRUE)


# hierarchical clustering

cl.norm <- hclust(matrix.norm, method = "complete")
cl.dend.norm <- as.dendrogram(cl.norm)
cl.dend.norm %>% set("branches_k_color", k = 2) %>% plot(main = "Complete Linkage Normalised")
cl.dend.norm



ward.norm <- hclust(matrix.norm, method = "ward.D2")
ward.dend.norm <- as.dendrogram(ward.norm)
ward.dend.norm %>% set("branches_k_color", k = 2) %>% plot(main = "Ward's Clustering Algorithm Normalised")
ward.dend.norm


#################################### Perform k-means ########################################################


################################################# calculate SWC  original ###############################################


# calculate and plot SWC & TWSS for K value

fviz_nbclust(data.norm, kmeans, method = "wss") +
  geom_vline(xintercept = 3, linetype = 2)

fviz_nbclust(data.norm, kmeans, method = "silhouette")

# perform k-means

data.km <- kmeans(x = data.norm[2:55], centers = 3, nstart = 10)
data.norm$cluster <- as.factor(data.km$cluster)

# plot NPS & Commitment level

ggplot() + geom_point(aes(x = NPS, y = Committment_Level , colour = as.factor(cluster)),data = data.norm)


# table of observations per cluster by classification

data.class <- data.norm$booking
table(data.km$cluster,data.class)

data.km$betweenss/data.km$totss
print(data.km)


###################### supervised classification modelling #############################

################################## naive_bayes categorical data ##################################

# set target variable to factor

no_observations <- dim(data.orig)[1]
no_predictors <- dim(data.orig)[2]-1   
data.orig$booking <- as.factor(data.orig$booking)
str(data.orig)


# Feature selection warapper

set.seed(0)
Feature_Set <- c()                                     # Initialise Feature Subset (empty)
Test_Error <- c()                                      # Initialise Error Subset
for(Size_Feature_Set in 1:no_predictors){              # Continue for each predictor starting from 1
  best_accuracy <- -Inf
  for(feature in 2:(no_predictors+1)){                 # Skip 1st variable (class labels)
    if (!(feature %in% Feature_Set)){
      Test_Feature_Set <- c(Feature_Set, feature)
      accuracy <- 0
      for(i in 1:10){
        test_index <- sample(no_observations, size=as.integer(no_observations*0.2), replace=FALSE)  # 20% test
        training_index <- -test_index                  # 80% training
        candidate_variables_index <- c(1, Test_Feature_Set) # "1" is the class variable (V1),
        # this vector selects specific data columns
        NaiveBayesModel <- naive_bayes(booking ~. ,         # . takes the available data columns   
                                       data = data.orig[training_index, candidate_variables_index], laplace = TRUE)
        Pred_class <- predict(NaiveBayesModel,
                              newdata = data.orig[test_index, candidate_variables_index])
        tab <- table(Pred_class, data.orig[test_index,"booking"])
        accuracy <- accuracy + sum(diag(tab))/sum(tab)
      }
      accuracy <- accuracy/10
      if (accuracy > best_accuracy){ # get the best feature that contributes to improve accuracy
        best_accuracy <- accuracy
        best_new_feature <- feature
      }
    }
  }
  Feature_Set <- c(Feature_Set, best_new_feature)  # list of best features in each iteration
  print(Feature_Set)
  Test_Error <- c(Test_Error, 1-best_accuracy)     # calculate the error rate in each iteration
}

plot(Test_Error) # finally plot the error rates


mean(Test_Error)

NaiveBayesModel

tab

tpr <- 320/(320+22)
fpr <- 22/(22+7)
tnr <- 7/(7+51)
fnr <- 51/(51+320)

model.performance <- cbind(c("tpr","fpr","tnr","fnr"),c(tpr,fpr,tnr,fnr))
as.data.frame(model.performance)



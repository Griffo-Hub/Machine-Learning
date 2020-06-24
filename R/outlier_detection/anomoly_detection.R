library(ggplot2)
library(plotly)
library(dbscan)
library(dplyr)
library(class)
library(ROCR)

setwd("C:/Users/David/OneDrive/University/Data Mining/Assessment 3 - Anomaly detection")
stamps <- read.table("Stamps_withoutdupl_09.csv", header=FALSE, sep=",", dec=".")
summary(stamps) # 9 Predictors (V1 to V9) and class labels (V10)
PB_Predictors <- stamps[,1:9] # 9 Predictors (V1 to V9)
PB_class <- stamps[,10] # Class labels (V10)
PB_class <- ifelse(PB_class == 'no',0,1) # Inliers (class "no") = 0, Outliers (class "yes") = 1

# Activity 1

#1

PCA <- prcomp(PB_Predictors, scale = TRUE)
PCA$rotation

# PVE - Proportion of the Variance Explained (PVE) 

PVE <- (PCA$sdev^2)/sum(PCA$sdev^2)

# plot PVE

PVE.df <- data.frame(PVE)
trans.data <- t(data[2:556])

PVE.df <- cbind(PVE.df, c("PC1","PC2","PC3","PC4","PC5","PC6","PC7","PC8","PC9"))
colnames(PVE.df) <- c("PVE", "PC")
PVE.df$PC <- as.factor(PVE.df$PC)
PVE.plot <- ggplot(data=PVE.df, aes(x=PC, y= PVE)) +
  geom_bar(stat="identity", fill = "#375094") +
  labs(title = "Proportion of the Variance Explained (PVE)", x = "Principle Component(s)", y = "PVE %") +
  geom_text(aes(label=sprintf("%0.3f", round(PVE.df$PVE, digits = 4))), position=position_dodge(width=0.9), vjust=-0.25) +
  theme_minimal()
PVE.plot

# Plot Cumulative sum of PVE

plot(cumsum(PVE.df$PVE), main = "Cumulative Sum of PVE", 
     type = "b", ylab = "Cumulative Sum of PVE",
     xlab = "Principle Component(s)")


# a)


PC1 <- 0.399
PC2 <- 0.152
PC3 <- 0.138
PC4 <- 0.100
PC5 <- 0.078
PC6 <- 0.063

PC1+PC2+PC3+PC4+PC5+PC6


# b)

PC1+PC2+PC3

# 2

# data manipulation for 3D plotting

PCA.subset <- cbind(PCA$x[,1], PCA$x[,2], PCA$x[,3])
PCA.subset <- data.frame(PCA.subset)
colnames(PCA.subset) <- c("PC1", "PC2", "PC3")
PB_class[which(PB_class == 0)] <- "Inlier"
PB_class[which(PB_class == 1)] <- "Outlier"
PB_class <- as.factor(PB_class)


# set scene for different camera angles

scene1 = list(camera = list(eye = list(x = 1.45, y = 1.45, z = 1.45)))
scene2 = list(camera = list(eye = list(x = -0.75, y = 2.40, z = 0.30)))
scene3 = list(camera = list(eye = list(x = -2.40, y = 0, z = 0.75)))
scene4 = list(camera = list(eye = list(x = 0, y = -2.40, z = 0.75)))

# 3D plot 1

plot.3d.stamps1 <- plot_ly(PCA.subset, x = ~PC1, y = ~PC2, z = ~PC3, color = PB_class, colors = c('black', 'red'), 
                           marker = list(size = 4)) %>%
                           add_markers() %>%
                           layout(scene = scene1,
                                  title = "PCA Plot Angle 1")

# 3D plot 2

plot.3d.stamps2 <- plot_ly(PCA.subset, x = ~PC1, y = ~PC2, z = ~PC3, color = PB_class, colors = c('black', 'red'), 
                           marker = list(size = 4)) %>%
                           add_markers() %>%
                           layout(scene = scene2,
                                  title = "PCA Plot Angle 2")

# 3D plot 3

plot.3d.stamps3 <- plot_ly(PCA.subset, x = ~PC1, y = ~PC2, z = ~PC3, color = PB_class, colors = c('black', 'red'), 
                           marker = list(size = 4)) %>%
                           add_markers() %>%
                           layout(scene = scene3,
                                  title = "PCA Plot Angle 3")

# 3D plot 4

plot.3d.stamps4 <- plot_ly(PCA.subset, x = ~PC1, y = ~PC2, z = ~PC3, color = PB_class, colors = c('black', 'red'), 
                           marker = list(size = 4)) %>%
                           add_markers() %>%
                           layout(scene = scene4,
                                  title = "Angle 4")

# plots
plot.3d.stamps1
plot.3d.stamps2
plot.3d.stamps3
plot.3d.stamps4


# Activity 2

# set k

k <- c(5,25,100)

# unsupervised outlier detection using knndist() 

# k = 5

KNN_Outlier.k5 <- kNNdist(x=PB_Predictors, k = k[1])[,k[1]] # KNN distance (outlier score) computation
KNN_Outlier.k5

# sort & display top 31 based on outlier scores

top_n <- 31 # No. of top outliers to be displayed

rank_KNN_Outlier.k5 <- order(x=KNN_Outlier.k5, decreasing = TRUE) # Sorting (descending)
KNN_Result.k5 <- data.frame(ID = rank_KNN_Outlier.k5, score = KNN_Outlier.k5[rank_KNN_Outlier.k5])
head(KNN_Result.k5, top_n)


# k = 25

KNN_Outlier.k25 <- kNNdist(x=PB_Predictors, k = k[2])[,k[2]] # KNN distance (outlier score) computation
KNN_Outlier.k25
class(KNN_Outlier.k5)
# sort & display top 31 based on outlier scores

rank_KNN_Outlier.k25 <- order(x=KNN_Outlier.k25, decreasing = TRUE) # Sorting (descending)
KNN_Result.k25 <- data.frame(ID = rank_KNN_Outlier.k25, score = KNN_Outlier.k25[rank_KNN_Outlier.k25])
head(KNN_Result.k25, top_n)


# k = 100

KNN_Outlier.k100 <- kNNdist(x=PB_Predictors, k = k[3])[,k[3]] # KNN distance (outlier score) computation
KNN_Outlier.k100

# sort & display top 31 based on outlier scores

rank_KNN_Outlier.k100 <- order(x=KNN_Outlier.k100, decreasing = TRUE) # Sorting (descending)
KNN_Result.k100 <- data.frame(ID = rank_KNN_Outlier.k100, score = KNN_Outlier.k100[rank_KNN_Outlier.k100])
head(KNN_Result.k100, top_n)

# Plot PCA - K = 5

plot.3d.knndist.k5 <- plot_ly(PCA.subset, x = ~PC1, y = ~PC2, z = ~PC3, color = KNN_Outlier.k5,
                              text = rownames(PCA.k5.t31),
                           marker = list(size = 4)) %>%
                           layout(title = "PCA ranked by KNN OUtlier score - K = 5")

# Plot PCA - K = 25

plot.3d.knndist.k25 <- plot_ly(PCA.subset, x = ~PC1, y = ~PC2, z = ~PC3, color = KNN_Outlier.k25, 
                              marker = list(size = 4)) %>%
                              layout(title = "PCA ranked by KNN OUtlier score - K = 25")

# Plot PCA - K = 100

plot.3d.knndist.k100 <- plot_ly(PCA.subset, x = ~PC1, y = ~PC2, z = ~PC3, color = KNN_Outlier.k100, 
                               marker = list(size = 4)) %>%
                               layout(title = "PCA ranked by KNN OUtlier score - K = 100")


# outlier score against PCA plots - continuous, diverging colourscale 

plot.3d.knndist.k5
plot.3d.knndist.k25
plot.3d.knndist.k100


# Plot PCA

# create top 31 results for each k

k5.t31 <- head(KNN_Result.k5, top_n)
k25.t31 <- head(KNN_Result.k25, top_n)
k100.t31 <- head(KNN_Result.k100, top_n)


# bind together PCA data and KNN data for plotting for each k
PCA.k5 <- cbind(PCA.subset, KNN_Result.k5)
PCA.k25 <- cbind(PCA.subset, KNN_Result.k25)
PCA.k100 <- cbind(PCA.subset, KNN_Result.k100)

# left outer join/merge PCA & KNN results with top31 results to create a dataframe(s) that can be used for plotting

PCA.k5.t31 <- merge(PCA.k5, k5.t31 , by = "ID", all.x = TRUE)
PCA.k25.t31 <- merge(PCA.k25, k25.t31 , by = "ID", all.x = TRUE)
PCA.k100.t31 <- merge(PCA.k100, k100.t31 , by = "ID", all.x = TRUE)

# add column for colouring of plots
# k5
PCA.k5.t31 <- PCA.k5.t31 %>% 
  mutate(class = case_when(is.na(score.y) ~ "PCA Obs",
                           score.y > 0 ~ "Top 31 KNN Outlier"))

# k25
PCA.k25.t31 <- PCA.k25.t31 %>% 
  mutate(class = case_when(is.na(score.y) ~ "PCA Obs",
                           score.y > 0 ~ "Top 31 KNN Outlier"))

# k100
PCA.k100.t31 <- PCA.k100.t31 %>% 
  mutate(class = case_when(is.na(score.y) ~ "PCA Obs",
                           score.y > 0 ~ "Top 31 KNN Outlier"))


# Plot the top 31 KNN Outlier's against original PCA data

# k5

plot.3d.knndist.k5.t31 <- plot_ly(PCA.subset, x = ~PC1, y = ~PC2, z = ~PC3, color = ~PCA.k5.t31$class, 
                                  text = rownames(PCA.k5.t31),
                                  colors = c("black", "red"),
                                  marker = list(size = 4)) %>%
  layout(title = "PCA with Top 31 KNN OUtlier's Ranked on Score - K = 5")


# k25

plot.3d.knndist.k25.t31 <- plot_ly(PCA.subset, x = ~PC1, y = ~PC2, z = ~PC3, color = ~PCA.k25.t31$class,
                                  text = rownames(PCA.k25.t31),
                                  colors = c("black", "red"),
                                  marker = list(size = 4)) %>%
  layout(title = "PCA with Top 31 KNN OUtlier's Ranked on Score - K = 25")

# k100

plot.3d.knndist.k100.t31 <- plot_ly(PCA.subset, x = ~PC1, y = ~PC2, z = ~PC3, color = ~PCA.k100.t31$class,
                                  text = rownames(PCA.k100.t31),
                                  colors = c("black", "red"),
                                  marker = list(size = 4)) %>%
  layout(title = "PCA with Top 31 KNN OUtlier's Ranked on Score - K = 100")

# Plots

plot.3d.knndist.k5.t31
plot.3d.knndist.k25.t31
plot.3d.knndist.k100.t31


# Activity 3

# 1

# Function to create ROC plot

rocplot <- function(pred, truth){
  predobj <- prediction(pred, truth)
  ROC     <- performance(predobj, "tpr", "fpr")
  # Plot the ROC Curve
  plot(ROC)
  auc     <- performance(predobj, measure = "auc")
  auc     <- auc@y.values[[1]]
  # Return the Area Under the Curve ROC
  return(auc)
}

# separate class labels from predictors

predictors <- stamps[c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8", "V9")]
class_labels <- stamps[, "V10"]

# Loop and plot through each k value of 5, 25, 100

for (i in 1:3){
Pred_class <- knn.cv(train=predictors, cl=class_labels, k=k[i], prob = TRUE)
Pred_prob <- attr(Pred_class, "prob")
Pred_prob <- ifelse(Pred_class=='yes', Pred_prob, 1 - Pred_prob)
AUC <- rocplot(pred=Pred_prob, truth=class_labels)
abline(a=0, b= 1)
text(x = .40, y = .6,paste("AUC = ", round(AUC[[1]],3), sep = ""))
}


# 2

kNN_scale.k5 <- (KNN_Outlier.k5 - min(KNN_Outlier.k5)) / (max(KNN_Outlier.k5) - min(KNN_Outlier.k5))
kNN_scale.k25 <- (KNN_Outlier.k25 - min(KNN_Outlier.k25)) / (max(KNN_Outlier.k25) - min(KNN_Outlier.k25))
kNN_scale.k100 <- (KNN_Outlier.k100 - min(KNN_Outlier.k100)) / (max(KNN_Outlier.k100) - min(KNN_Outlier.k100))


# Compare supervised against unsupervised and plot K = 5

# create 2 predictor classes for comparison. Supervised (knn.cv) & Unsupervised (Oulier score)
supervised.pred.k5 <- prediction(Pred_prob, class_labels)
unsupervised.pred.k5 <- prediction(kNN_scale.k5, class_labels)

# create 2 performance classes for comparison. Supervised (knn.cv) & Unsupervised (Oulier score)

supervised.perf.k5 <- performance( supervised.pred.k5, "tpr", "fpr" )
unsupervised.perf.k5 <- performance(unsupervised.pred.k5, "tpr", "fpr")

# get auc for K = 5 (outlier score)

auc.k5 <- performance(unsupervised.pred.k5, measure = "auc")
auc.k5 <- auc.k5@y.values[[1]]

# plot for comparison 

plot(supervised.perf.k5, col = "dark blue", main = "Supervised vs Unsupervised K = 5")
plot(unsupervised.perf.k5, add = TRUE, col = "dark green")
legend("topleft", legend = c("Supervised", "Unsupervised"),
       col = c("dark blue", "dark green"), lty = 1, cex = 0.8)
abline(a=0, b= 1)
text(x = .80, y = .27,paste("AUC Supervised = ", round(auc.k5[[1]],3), sep = ""), col = "dark blue")
text(x = .79, y = .21,paste("AUC Unsupervised = ", 0.942, sep = ""), col = "dark green")


# Compare supervised against unsupervised and plot K = 25

# create 2 predictor classes for comparison. Supervised (knn.cv) & Unsupervised (Oulier score)

supervised.pred.k25 <- prediction(Pred_prob, class_labels)
unsupervised.pred.k25 <- prediction(kNN_scale.k25, class_labels)

# create 2 performance classes for comparison. Supervised (knn.cv) & Unsupervised (Oulier score)

supervised.perf.k25 <- performance(supervised.pred.k25, "tpr", "fpr" )
unsupervised.perf.k25 <- performance(unsupervised.pred.k25, "tpr", "fpr")

# get auc for K = 25 (outlier score)

auc.k25 <- performance(unsupervised.pred.k25, measure = "auc")
auc.k25 <- auc.k25@y.values[[1]]

# plot for comparison 

plot(supervised.perf.k25, col = "dark blue", main = "Supervised vs Unsupervised K = 25")
plot(unsupervised.perf.k25, add = TRUE, col = "dark green")
legend("topleft", legend = c("Supervised", "Unsupervised"),
       col = c("dark blue", "dark green"), lty = 1, cex = 0.8)
abline(a=0, b= 1)
text(x = .80, y = .27,paste("AUC Supervised = ", round(auc.k25[[1]],3), sep = ""), col = "dark blue")
text(x = .79, y = .21,paste("AUC Unsupervised = ", 0.942, sep = ""), col = "dark green")

# Compare supervised against unsupervised and plot K = 100

# create 2 predictor classes for comparison. Supervised (knn.cv) & Unsupervised (Oulier score)

supervised.pred.k100 <- prediction(Pred_prob, class_labels)
unsupervised.pred.k100 <- prediction(kNN_scale.k100, class_labels)

# create 2 performance classes for comparison. Supervised (knn.cv) & Unsupervised (Oulier score)

supervised.perf.k100 <- performance(supervised.pred.k100, "tpr", "fpr" )
unsupervised.perf.k100 <- performance(unsupervised.pred.k100, "tpr", "fpr")

# get auc for K = 100 (outlier score)

auc.k100 <- performance(unsupervised.pred.k100, measure = "auc")
auc.k100 <- auc.k100@y.values[[1]]

# plot for comparison 

plot(supervised.perf.k100, col = "dark blue", main = "Supervised vs Unsupervised K = 100")
plot(unsupervised.perf.k100, add = TRUE, col = "dark green")
legend("topleft", legend = c("Supervised", "Unsupervised"),
       col = c("dark blue", "dark green"), lty = 1, cex = 0.8)
abline(a=0, b= 1)
text(x = .80, y = .27,paste("AUC Supervised = ", round(auc.k100[[1]],3), sep = ""), col = "dark blue")
text(x = .79, y = .21,paste("AUC Unsupervised = ", 0.942, sep = ""), col = "dark green")

# import data
library(foreign)
library(stats)
library(dbscan)
library(ggplot2)
library(reshape2)


#1
setwd("C:/Users/david/OneDrive/University/Introduction to Data Mining/Assessment 2 - Clustering gene-expression data")
data <- read.arff("golub-1999-v1_database.arff")
# Activity 1
#2
class <- data$Classe
data <- data[-1869]

# 3
matrix <- dist(data, method = "euclidean", diag = TRUE, upper = TRUE)
matrix
# 4
sl <- hclust(matrix, method = "single")
plot(sl, main = "Single Linkage", xlab = "", sub = "", hang = -1, labels = FALSE)
#5
cl <- hclust(matrix, method = "complete")
plot(cl, main = "Complete Linkage", xlab = "", sub = "", hang = -1, labels = FALSE)
#6
al <- hclust(matrix, method = "average")
plot(al, main = "Average Linkage", xlab = "", sub = "", hang = -1, labels = FALSE)
#7
ward <- hclust(matrix, method = "ward.D2")
plot(ward, main = "Ward's Clustering Algorithm", xlab = "", sub = "", hang = -1, labels = FALSE)
#9
plot(sl, main = "Single Linkage", xlab = "", sub = "", hang = -1, labels = class)
plot(cl, main = "Complete Linkage", xlab = "", sub = "", hang = -1, labels = class)
plot(al, main = "Average Linkage", xlab = "", sub = "", hang = -1, labels = class)
plot(ward, main = "Ward's Clustering Algorithm", xlab = "", sub = "", hang = -1, labels = class)

#10
normalised.data = scale(data,center=TRUE,scale=TRUE) # normalise data
colMeans(normalised.data) # verify mean = 0
apply(normalised.data,2,sd) # verify sd = 1
normalised.data <- dist(normalised.data, method = "euclidean", diag = TRUE, upper = TRUE)

# calculate and plot single linkage on normalised dataset
normalised.sl <- hclust(normalised.data, method = "single")
plot(normalised.sl, main = "Single Linkage Normalised", xlab = "", sub = "", hang = -1, labels = FALSE)

# calculate and plot complete linkage on normalised dataset
normalised.cl <- hclust(normalised.data, method = "complete")
plot(normalised.cl, main = "Complete Linkage Normalised", xlab = "", sub = "", hang = -1, labels = FALSE)

# calculate and plot single linkage on normalised dataset
normalised.al <- hclust(normalised.data, method = "average")
plot(normalised.al, main = "Average Linkage Normalised", xlab = "", sub = "", hang = -1, labels = FALSE)

# calculate and plot ward's clustering algorithm on normalised dataset
normalised.ward <- hclust(normalised.data, method = "ward.D2")
plot(normalised.ward, main = "Ward's Clustering Algorithm Normalised", xlab = "", sub = "", hang = -1, labels = FALSE)

# plot with class labels on normalised dataset

plot(normalised.sl, main = "Single Linkage Normalised", xlab = "", sub = "", hang = -1, labels = class)
plot(normalised.cl, main = "Complete Linkage Normalised", xlab = "", sub = "", hang = -1, labels = class)
plot(normalised.al, main = "Average Linkage Normalised", xlab = "", sub = "", hang = -1, labels = class)
plot(normalised.ward, main = "Ward's Clustering Algorithm Normalised", xlab = "", sub = "", hang = -1, labels = class)

#11
yeast.data <- read.arff("yeast.arff")
#12
yeast.data.class <- yeast.data$Classe
yeast.data.class
yeast.data <- yeast.data[-21]
summary(yeast.data)
#13

yeast.matrix.p  <- as.dist(1 - abs(cor(t(yeast.data),  method = "pearson")))
yeast.matrix.p
summary(yeast.matrix.p)

#14

# perform  clustering method for each method, single, complete, average, ward
yeast.p.sl <- hclust(yeast.matrix.p, method = "single")
yeast.p.cl <- hclust(yeast.matrix.p, method = "complete")
yeast.p.al <- hclust(yeast.matrix.p, method = "average")
yeast.p.ward <- hclust(yeast.matrix.p, method = "ward.D2")

plot(yeast.p.sl, main = "Single Linkage", xlab = "", sub = "", hang = -1, labels = FALSE)
plot(yeast.p.cl, main = "Complete Linkage", xlab = "", sub = "", hang = -1, labels = FALSE)
plot(yeast.p.al, main = "Average Linkage", xlab = "", sub = "", hang = -1, labels = FALSE)
plot(yeast.p.ward, main = "Ward's Clustering Algorithm", xlab = "", sub = "", hang = -1, labels = FALSE)

plot(yeast.p.sl, main = "Single Linkage", xlab = "", sub = "", hang = -1, labels = yeast.data.class)
plot(yeast.p.cl, main = "Complete Linkage", xlab = "", sub = "", hang = -1, labels = yeast.data.class)
plot(yeast.p.al, main = "Average Linkage", xlab = "", sub = "", hang = -1, labels = yeast.data.class)
plot(yeast.p.ward, main = "Ward's Clustering Algorithm", xlab = "", sub = "", hang = -1, labels = yeast.data.class)

#15
#Rescale Yeast.Data
## create magnitude function
magnitude <- function(x) {
  sqrt(sum(x^2))
}
yeast.mag <- apply(yeast.data,1,magnitude)
yeast.rescaled <- sweep(yeast.data,1,yeast.mag,"/")
yeast.rescaled

#16

yeast.hdbs <- hdbscan(yeast.rescaled, minPts = 5)
yeast.hdbs
plot(yeast.hdbs$hc, hang = -1, labels = FALSE)
plot(yeast.hdbs$hc, hang = -1, labels = yeast.data.class)

#17

cont.table <- table(yeast.hdbs$cluster,yeast.data.class)
cont.table

#18

cluster1 = length(as.vector(yeast.data.class[yeast.data.class == "cluster1"]))
cluster2 = length(as.vector(yeast.data.class[yeast.data.class == "cluster2"]))
cluster3 = length(as.vector(yeast.data.class[yeast.data.class == "cluster3"]))
cluster4 = length(as.vector(yeast.data.class[yeast.data.class == "cluster4"]))
cluster.length <- cbind(cluster1,cluster2,cluster3,cluster4)
rownames(cluster.length) <- "num_observations"
cluster.length

#19

# add row numbers to rescaled df
yeast.rescaled <- cbind(yeast.rescaled, row_number = seq(1,nrow(yeast.rescaled)))
# add cluster type to rescaled df
yeast.rescaled <- cbind(yeast.rescaled, cluster = yeast.data.class)
# unpivot rescaled df
yeast.rescaled.unpivot <- melt(yeast.rescaled, id.vars=c("row_number", "cluster" ) )
# plot

ggplot(yeast.rescaled.unpivot, aes(x = variable, y = value, group = row_number, colour=cluster),fill = as.factor(x)) +
  geom_line(stat= "identity") +
  facet_wrap(~cluster, ncol = 2, scales = "free") +
  scale_color_manual(values = c("#E7B800", "#2E9FDF", "#FC4E07", "#458B00")) +
  theme_minimal() +
  theme(axis.text.x = element_blank())

#20

# add hdbs cluster type to rescaled df
yeast.rescaled.unpivot <- cbind(yeast.rescaled.unpivot, hdbs.cluster = yeast.hdbs$cluster)
subset.yeast.rescaled.unpivot <- subset(yeast.rescaled.unpivot, hdbs.cluster != 0)
subset.yeast.rescaled.unpivot$hdbs.cluster <- as.factor(subset.yeast.rescaled.unpivot$hdbs.cluster)
str(subset.yeast.rescaled.unpivot)

# plot hdbs.cluster
  
ggplot(subset.yeast.rescaled.unpivot, aes(x = variable, y = value, group = row_number, colour=hdbs.cluster),fill = as.factor(x)) +
  geom_line(stat= "identity") +
  facet_wrap(~hdbs.cluster, ncol = 2, scales = "free") +
  scale_color_manual(values = c("#458B00", "#FC4E07", "#2E9FDF", "#E7B800")) +
  theme_minimal() +
  theme(axis.text.x = element_blank())



  


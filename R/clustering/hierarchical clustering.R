library(ggplot2)
x1 <- c(1,2,3,4,8,9,10)
x2 <- c(6,1,1,1.5,4.5,5,5)
Point_ID <- as.factor(c(1:7))
dat_7points <- data.frame(x1 = x1, x2 = x2, Point_ID = Point_ID)
g <- ggplot(data=dat_7points) + geom_point(mapping=aes(x=x1, y=x2), size = 3) +
    geom_text(mapping=aes(x=(x1-0.2), y=x2, label = Point_ID)) 
g

# Using Single Linkage as starting point for dissimilarity measure
DistMatrix <- dist(dat_7points[1:2])
SL_7points <- hclust(DistMatrix, method = "single")
cutree(tree = SL_7points, k = 1:7)
plot(SL_7points, main = "Single Linkage", xlab = "", sub = "", hang = -1)


# Dendogram on the Iris dataset

DMatrix <- dist(iris[1:4])
SL_iris <- hclust(DMatrix, method = "single")
plot(SL_iris, main = "Single Linkage", xlab = "", sub = "", hang = -1, labels = FALSE)

# add leaf labels

plot(SL_iris, main = "Single Linkage", xlab = "", sub = "", hang = -1, labels = iris$Species, cex = 0.6)

# Exercise 1 - Plot dendogram on Topic 1 dataset

datMat <- dist(dat)
SL_4Gauss <- hclust(datMat, method = "single")
plot(SL_4Gauss, main = "Single Linkage", xlab = "", sub = "", hang = -1, labels = FALSE)


# Use Complete linkage

CL_7points <- hclust(DistMatrix, method = "complete")
plot(CL_7points, main = "Complete Linkage", xlab = "", sub = "", hang = -1, labels = FALSE)

# Exercise 2 - Plot dendogram on Topic 1 dataset

datMat <- dist(dat)
CL_4Gauss <- hclust(datMat, method = "complete")
plot(CL_4Gauss, main = "Complete Linkage", xlab = "", sub = "", hang = -1, labels = FALSE)

# Exercise 3 - Iris

DMatrix <- dist(iris[1:4])
CL_Iris <- hclust(DistMatrix, method = "complete")
plot(CL_Iris, main = "Complete Linkage", xlab = "", sub = "", hang = -1, labels = FALSE)


# Use Average Linkage

AL_7points <- hclust(DistMatrix, method = "average")
plot(AL_7points, main = "Average Linkage", xlab = "", sub = "", hang = -1)

# Exercise 4 - Topic 1 dataset

datMat <- dist(dat)
AL_4Gauss <- hclust(datMat, method = "average")
plot(AL_4Gauss, main = "Average Linkage", xlab = "", sub = "", hang = -1, labels = FALSE)

# Exercise 5 - Iris

DMatrix <- dist(iris[1:4])
AL_iris <- hclust(DMatrix, method = "average")
plot(AL_iris, main = "Average Linkage", xlab = "", sub = "", hang = -1, labels = FALSE)

# Using Wards Algorithm

Ward_7points <- hclust(DistMatrix, method = "ward.D2")
plot(Ward_7points, main = "Ward's", xlab = "", sub = "", hang = -1)

# Exercise 6 - Topic 1 dataset

datMat <- dist(dat)
Ward_4Gauss <- hclust(datMat, method = "ward.D2")
plot(Ward_4Gauss, main = "Ward's", xlab = "", sub = "", hang = -1, labels = FALSE)

# Exercise 7 - Iris dataset

DMatrix <- dist(iris[1:4]) # Euclidean distances must be provided so that Ward's has a mathematical interpretation (they are squared internally in option "ward.D2")
Ward_iris <- hclust(DMatrix, method = "ward.D2") # "ward.D2" implements the original Ward's (1963) criterion, where the dissimilarities are squared before cluster updating
plot(Ward_iris, main = "Ward's", xlab = "", sub = "", hang = -1, labels = FALSE)

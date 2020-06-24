# showing that SSE decreases the more 'k' clusters there are

set.seed(0)
SSE <- rep(0, 10)
for (k in 1:10){
  km.out <- kmeans(x = dat, centers = k, nstart = 10)
  SSE[k] <- km.out$tot.withinss
}
plot(1:10, SSE, xlab="k", ylab="SSE", type = "b")

# Example on Iris
# Perform k-means
set.seed(0)
SSE <- rep(0, 10)
for (k in 1:10){
  km.out <- kmeans(x = iris[1:4], centers = k, nstart = 10)
  SSE[k] <- km.out$tot.withinss
}
plot(1:10, SSE, xlab="k", ylab="SSE", type = "b")

# Exercise 2

set.seed(0)
km.out <- kmeans(x = iris[1:4], centers = 3, nstart = 10)
table(km.out$cluster,iris$Species)


# Silhouette Width Criterion

SWC <- function(clusterLabels, dataPoints){
  library(cluster)
  sil <- silhouette(x = clusterLabels, dist = dist(dataPoints))
  return(mean(sil[,3]))
}

# calculate SWC 

set.seed(0)
Silhouette <- rep(0, 10)
for (k in 2:10){
  km.out <- kmeans(x = dat, centers = k, nstart = 10)
  Silhouette[k] <- SWC(clusterLabels = km.out$cluster, dataPoints = dat)
}

# plot to see where SWC is at its highest

plot(2:10, Silhouette[2:10], xlab="k", ylab="Silhouette Width Criterion (SWC)", type = "b")


# Exercise 4 (iris)

set.seed(0)
Silhouette <- rep(0, 10)
for (k in 2:10){
  km.out <- kmeans(x = iris[1:4], centers = k, nstart = 10)
  Silhouette[k] <- SWC(clusterLabels = km.out$cluster, dataPoints = iris[1:4])
}

plot(2:10, Silhouette[2:10], xlab="k", ylab="Silhouette Width Criterion (SWC)", type = "b")

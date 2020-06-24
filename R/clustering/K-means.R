# set up data
set.seed(0)
x11 <- rnorm(n = 100, mean = 10, sd = 1) # Cluster 1 (x1 coordinate)
x21 <- rnorm(n = 100, mean = 10, sd = 1) # Cluster 1 (x2 coordinate)
x12 <- rnorm(n = 100, mean = 20, sd = 1) # Cluster 2 (x1 coordinate)
x22 <- rnorm(n = 100, mean = 10, sd = 1) # Cluster 2 (x2 coordinate)
x13 <- rnorm(n = 100, mean = 15, sd = 3) # Cluster 3 (x1 coordinate)
x23 <- rnorm(n = 100, mean = 25, sd = 3) # Cluster 3 (x2 coordinate)
x14 <- rnorm(n = 50, mean = 25, sd = 1)  # Cluster 4 (x1 coordinate)
x24 <- rnorm(n = 50, mean = 25, sd = 1)  # Cluster 4 (x2 coordinate)
#plot data
dat <- data.frame(x1 = c(x11,x12,x13,x14), x2 = c(x21,x22,x23,x24))
plot(dat$x1, dat$x2, xlim = c(5,30), ylim = c(5,35), xlab = "x1", ylab = "x2")
# run k-means
set.seed(0)
km.out <- kmeans(x = dat, centers = 4)
km.out
# just SSE for the calculation
km.out$tot.withinss
#plot results
plot(dat, col=(km.out$cluster+1), main="k=4: single initialisation with set.seed(0)", xlab="x1", ylab="x2")

# Above example had "local minima" issues due to where the prototypes were originally located. +
# try initialising 10 times nstart = 10
set.seed(0)
km.out <- kmeans(x = dat, centers = 4, nstart = 50)
km.out$tot.withinss
plot(dat, col=(km.out$cluster+1), main="k=4: best out of 10 initialisations", xlab="x1", ylab="x2")

#Exercise
nstart <- seq(from=0, to=50, by=5); 
nstart[1] <- 1
nstart
SSE <- rep(0, 11)
for (i in 1:11){
  set.seed(0)
  km.out <- kmeans(x = dat, centers = 4, nstart = nstart[i])
  SSE[i] <- km.out$tot.withinss
}
plot(nstart, SSE, xlab="nstart", ylab="SSE")


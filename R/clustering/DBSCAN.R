library(dbscan)
set.seed(0)
data("DS3")
dbs_b <- dbscan(DS3, eps = 11, minPts = 25)
dbs_b
# store the resulting cluster label for each observation
dbs_b$cluster[1:30]
# Update the eps value to show impact on output
dbs_c <- dbscan(DS3, eps = 10, minPts = 25)
dbs_c

dbs_d <- dbscan(DS3, eps = 9, minPts = 25)
dbs_d

# Update minPts to show impact on output

dbs_e <- dbscan(DS3, eps = 11, minPts = 21)
dbs_e

dbs_f <- dbscan(DS3, eps = 11, minPts = 29)
dbs_f

par(mfrow = c(3, 2))
color_1to8 <- function(x) ifelse(x==0,1,((x-1)%%7)+2)
plot(DS3, pch=19, cex=0.5, main="(a) DS3 Data from the 'dbscan' Library", xlab="x1", ylab="x2")
plot(DS3, pch=19, cex=0.5, col=color_1to8(dbs_b$cluster), main="(b) DBSCAN DS3 data [minpts = 25, eps = 11]: 6 clusters", xlab="x1", ylab="x2")
plot(DS3, pch=19, cex=0.5, col=color_1to8(dbs_c$cluster), main="(c) DBSCAN DS3 data [minpts = 25, eps = 10]: 8 clusters", xlab="x1", ylab="x2")
plot(DS3, pch=19, cex=0.5, col=color_1to8(dbs_d$cluster), main="(d) DBSCAN DS3 data [minpts = 25, eps = 9]: 21 clusters", xlab="x1", ylab="x2")
plot(DS3, pch=19, cex=0.5, col=color_1to8(dbs_e$cluster), main="(e) DBSCAN DS3 data [minpts = 21, eps = 11]: 6 clusters", xlab="x1", ylab="x2")
plot(DS3, pch=19, cex=0.5, col=color_1to8(dbs_f$cluster), main="(f) DBSCAN DS3 data [minpts = 29, eps = 11]: 6 clusters", xlab="x1", ylab="x2")

# perform DBSCAN clustering on partitional dataset

dbs_g <- dbscan(dat, eps = 2.8, minPts = 15)
dbs_g

par(mfrow = c(1, 1))
plot(dat, pch=19, cex=0.5, col=color_1to8(dbs_g$cluster), main="DBSCAN Data Set Fig. 1 [minpts = 15, eps = 2.8]: 4 clusters", xlab="x1", ylab="x2")
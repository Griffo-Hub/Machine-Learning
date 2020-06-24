# HDBSCAN summarised dendogram
hdbs <- hdbscan(DS3, minPts = 25)
par(mfrow = c(1, 1))
plot(hdbs)
hdbs

# Traditional dendogram
hdbs <- hdbscan(iris[1:4], minPts = 5)
hdbs
plot(hdbs$hc, main="HDBSCAN* Hierarchy", xlab = "", sub = "", hang = -1, labels = iris$Species, cex = 0.6)
plot(hdbs)


# Calc for Topic 1 dataset
hdbs <- hdbscan(dat, minPts = 10, metric='euclidean')
?hdbscan
plot(hdbs)
Hdbs
plot(dat, pch=19, cex=0.5, col=color_1to8(hdbs$cluster), main="HDBSCAN* Data Set Fig. 1 (minpts = 10): 4 clusters", xlab="x1", ylab="x2")
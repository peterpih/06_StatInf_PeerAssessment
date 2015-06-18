# Statistical Inference Peer Assessment

There are two studies in this assessment:

##ExpSample.Rmd -> ExpSample.pdf

##ToothGrowth.Rmd -> ToothGrowth.pdf

## k-means Clustering Analysis Example

``{R}
set.seed(1234)
par(mar = c(0, 0, 0, 0))
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2)
plot(x, y, col = "blue", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))
```

```{R}
dataFrame <- data.frame(x, y)
kmeansObj <- kmeans(dataFrame, centers = 3)
names(kmeansObj)
kmeansObj$cluster
```

```{R}
par(mar = rep(0.2, 4))
plot(x, y, col = kmeansObj$cluster, pch = 19, cex = 2)
points(kmeansObj$centers, col = 1:3, pch = 3, cex = 3, lwd = 3)
```

##Heatmaps
```{R}
set.seed(1234)
dataMatrix <- as.matrix(dataFrame)[sample(1:12), ]
kmeansObj2 <- kmeans(dataMatrix, centers = 3)
par(mfrow = c(1, 2), mar = c(2, 4, 0.1, 0.1))
image(t(dataMatrix)[, nrow(dataMatrix):1], yaxt = "n")
image(t(dataMatrix)[, order(kmeansObj$cluster)], yaxt = "n")
```

Notes and further resources
K-means requires a number of clusters
- Pick by eye/intuition
- Pick by cross validation/information theory, etc.
- Determining the number of clusters: 
  https://en.wikipedia.org/wiki/Determining_the_number_of_clusters_in_a_data_set

K-means is not deterministic
- Different # of clusters
- Different number of iterations
- Can be unstable, try with different seeds and number of clusters to determine stability
- Rafael Irizarry's Distances and Clustering Video:  
  https://www.youtube.com/watch?v=wQhVWUcXM0A
- Elements of statistical learning:  
  http://statweb.stanford.edu/~tibs/ElemStatLearn/


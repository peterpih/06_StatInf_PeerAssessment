# Statistical Inference Peer Assessment

There are two studies in this assessment:

##ExpSample.Rmd -> ExpSample.pdf

##ToothGrowth.Rmd -> ToothGrowth.pdf

## k-means Clustering Analysis Example

```{R}
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

##Dimension Reduction
##Principal Component Analysis and Singular Value Decomposition

Matrix Data
```{R}
set.seed(12345)
par(mar = rep(0.2, 4))
dataMatrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])
```
Cluster the data
```{R}
par(mar = rep(0.2, 4))
heatmap(dataMatrix)
```
What if we add a pattern?
```{R}
set.seed(678910)
for (i in 1:40) {
    # flip a coin
    coinFlip <- rbinom(1, size = 1, prob = 0.5)
    # if coin is heads add a common pattern to that row
    if (coinFlip) {
        dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 3), each = 5)
    }
}
```
What if we add a pattern? - the data
```{R}
par(mar = rep(0.2, 4))
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])
```
What if we add a pattern? - the clustered data
```{R}
par(mar = rep(0.2, 4))
heatmap(dataMatrix)
```
Patterns in rows and columns
```{R}
hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order, ]
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered), 40:1, , xlab = "Row Mean", ylab = "Row", pch = 19)
plot(colMeans(dataMatrixOrdered), xlab = "Column", ylab = "Column Mean", pch = 19)
```
Related problems

You have multivariate variables X1,…,Xn so X1=(X11,…,X1m)  
Find a new set of multivariate variables that are uncorrelated and explain as much variance as possible.  
If you put all the variables together in one matrix, find the best matrix created with fewer variables (lower rank) that explains the original data.  
The first goal is statistical and the second goal is data compression.  

Related solutions - PCA/SVD

SVD  
If X is a matrix with each variable in a column and each observation in a row then the SVD is a "matrix decomposition"

X=UDVT  
where the columns of U are orthogonal (left singular vectors), the columns of V are orthogonal (right singular vectors) and D is a diagonal matrix (singular values).

PCA  
The principal components are equal to the right singular values if you first scale (subtract the mean, divide by the standard deviation) the variables.

Components of the SVD - u and v  
```{R}
svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(svd1$u[, 1], 40:1, , xlab = "Row", ylab = "First left singular vector", 
    pch = 19)
plot(svd1$v[, 1], xlab = "Column", ylab = "First right singular vector", pch = 19)
```
Components of the SVD - Variance explained  
```{R}
par(mfrow = c(1, 2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Prop. of variance explained", pch = 19)
```
Relationship to principal components  
```{R}
svd1 <- svd(scale(dataMatrixOrdered))
pca1 <- prcomp(dataMatrixOrdered, scale = TRUE)
plot(pca1$rotation[, 1], svd1$v[, 1], pch = 19, xlab = "Principal Component 1", 
    ylab = "Right Singular Vector 1")
abline(c(0, 1))
```
Components of the SVD - variance explained  
```{R}
constantMatrix <- dataMatrixOrdered*0
for(i in 1:dim(dataMatrixOrdered)[1]){constantMatrix[i,] <- rep(c(0,1),each=5)}
svd1 <- svd(constantMatrix)
par(mfrow=c(1,3))
image(t(constantMatrix)[,nrow(constantMatrix):1])
plot(svd1$d,xlab="Column",ylab="Singular value",pch=19)
plot(svd1$d^2/sum(svd1$d^2),xlab="Column",ylab="Prop. of variance explained",pch=19)
```
What if we add a second pattern?  
```{R}
set.seed(678910)
for (i in 1:40) {
    # flip a coin
    coinFlip1 <- rbinom(1, size = 1, prob = 0.5)
    coinFlip2 <- rbinom(1, size = 1, prob = 0.5)
    # if coin is heads add a common pattern to that row
    if (coinFlip1) {
        dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 5), each = 5)
    }
    if (coinFlip2) {
        dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 5), 5)
    }
}
hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order, ]
```
Singular value decomposition - true patterns  
```{R}
svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rep(c(0, 1), each = 5), pch = 19, xlab = "Column", ylab = "Pattern 1")
plot(rep(c(0, 1), 5), pch = 19, xlab = "Column", ylab = "Pattern 2")
```
v and patterns of variance in rows
```{R}
svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(svd2$v[, 1], pch = 19, xlab = "Column", ylab = "First right singular vector")
plot(svd2$v[, 2], pch = 19, xlab = "Column", ylab = "Second right singular vector")
```

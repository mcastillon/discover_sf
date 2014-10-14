library(skmeans)
library(slam)
library(doParallel)

args <- commandArgs(TRUE)
filename <- args[1]

datain <- read.csv(filename)
data <-  scale(datain) 

### sparse data matrix
stm <- as.simple_triplet_matrix(data)

### parallelize
cl <- makeCluster(2)
registerDoParallel(cl)

### calculate cosine distance
xdist <- skmeans_xdist(stm)

### coose max number of possible clusters
max_k <- round(2 * sqrt(nrow(data) / 2))

### determine number of clusters
num_clus <- foreach(i=2:max_k, .packages=c("skmeans", "slam", "cluster")) %dopar% {
	clus <- skmeans(stm, k=i, m=1.2, method="pclust", control=list(start="S", maxchains=25))
	s <- silhouette(clus$cluster, xdist)
	summary(s)$avg.width
}
mx <- which.max(num_clus) + 1

### determine soft/fuzzy clusters using spherical k-means
clus <- skmeans(stm, k=mx, m=1.2, method="pclust", control=list(start="S", maxchains=25))
write.csv(clus$membership, "test.csv")
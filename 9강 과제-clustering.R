# 문제 1. K-means clustering
# Importing a dataset
data(USArrests)
str(USArrests)
df <- scale(USArrests)
str(df)
head(df)

# Determining the optimal number of cluster
library(factoextra)
fviz_nbclust(df, kmeans, method="gap_stat")

# K-means clustering (k=4 from the result of previous step)
average = hclust(dist(df), method="average")
initial = tapply(df, 
                 list(rep(cutree(average,4), ncol(df)), col(df)),
                 mean)
km = kmeans(df, initial, algorithm="MacQueen")
km

# Visualising the result of K-means clustering
library(cluster)
clusplot(df, km$cluster, labels=2)


# 문제 2. 계층적 군집분석
# Single linkage
single = hclust(dist(df, method="manhattan"), method="single")
plclust(single)
cutree(single,2)
cutree(single,3)
cutree(single,4)

# 완전연결법
complete = hclust(dist(df, method="manhattan"), method="complete")
plclust(complete)
cutree(complete,2)
cutree(complete,3)
cutree(complete,4)

# Ward's method
Ward = hclust(dist(df, method="euclidean"), method="ward.D2")
plclust(Ward)
cutree(Ward,2)
cutree(Ward,3)
cutree(Ward,4)

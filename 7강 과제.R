data = read.csv("7강 과제 data.csv", header=FALSE)
head(data)
colnames(data) <- c("company","X1","X2","X3","X4","X5")
head(data)

X = data[,-1]
company = data[,1]

Z = scale(X, center=TRUE, scale=TRUE)
maxX = apply(X, 2, max)
minX = apply(X, 2, min)
z01x = scale(X, center=minX, scale=maxX-minX)

z01x.dist = dist(z01x, method="euclidean")
z01x.dist = as.matrix(z01x.dist)

library(smacof)
mds = smacofSym(z01x.dist, ndim=2)
par(mfrow=c(1,3))
plot(mds$conf[,1], mds$conf[,2], type="n")
text(mds$conf[,1], mds$conf[,2], rownames(z01x.dist), cex=0.9)
abline(h=0, v=0, lty=3)
mds$stress

mds.1 = smacofSym(z01x.dist, ndim=1)
mds.2 = smacofSym(z01x.dist, ndim=2)
mds.3 = smacofSym(z01x.dist, ndim=3)
mds.4 = smacofSym(z01x.dist, ndim=4)
stress.value = c(mds.1$stress, mds.2$stress,
                 mds.3$stress, mds.4$stress)
plot(stress.value, type="l")
points(stress.value, cex=0.9)
# 해석 : 2차원으로만 해도 stress 값 충분히 낮춰진다

plot(mds$confdist, mds$delta)

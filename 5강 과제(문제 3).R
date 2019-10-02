install.packages("ISLR")
library(ISLR)
data(Hitters)
head(Hitters)
data <- na.omit(Hitters)

library(glmnet)
names(data)
X=model.matrix(Salary~.,data)[,-1]
y=data$Salary

sh <- 10^seq(10,-2,length=100)
Ridge <- glmnet(X,y,alpha=0, lambda=sh)
summary(Ridge)
Lasso <- glmnet(X,y, alpha=1, lambda=sh)
summary(Lasso)

par(mfrow=c(1,2))
plot(Ridge)
plot(Lasso)

CrossValidError = cv.glmnet(X,y,alpha=0)
plot(CrossValidError)
CrossValidError$lambda.min
coef(CrossValidError)

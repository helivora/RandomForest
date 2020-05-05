
library(randomForest)
library(MASS)
?Boston
head(Boston)

set.seed(101)
train = sample(1:nrow(Boston), 300)


# Random Forests
########################

rf.boston=randomForest(medv~.,data=Boston,subset=train,importance=TRUE)
rf.boston
plot(rf.boston)
plot(rf.boston$mse)
rf.boston$mse[500]
names(rf.boston)
rf.boston$importance# Increase in MSE if variable is permuted

# Out-of-bag and test error for different mtry value
oob.err=numeric(13)
test.err=numeric(13)
for (mtry in 1:13){
  fit=randomForest(medv~.,data=Boston,subset=train,mtry=mtry,ntree=400)
  oob.err[mtry] = fit$mse[400]
  pred=predict(fit,Boston[-train,])
  test.err[mtry]=with(Boston[-train,],mean((medv-pred)^2))
  # Equivalent to: test.err[mtry]=mean((Boston[-train,]$medv-pred)^2)
  print(mtry)
}

matplot(1:mtry,cbind(test.err,oob.err),pch=19,col=c(2,4),type="b",ylab="Mean Squared Error")
legend("topright",legend=c("OOB","Test"),pch=19,col=c(2,4))


# Boosting
########################

library(gbm)
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",
                 n.trees=10000,shrinkage=0.01,interaction.depth=4)
summary(boost.boston)
plot(boost.boston,i="lstat")
plot(boost.boston,i="rm")

# How many tress do we need?
n.trees = seq(100,10000,100)
predmat = predict(boost.boston,newdata=Boston[-train,],n.trees=n.trees)
dim(predmat)
berr = with(Boston[-train,],apply((medv-predmat)^2,2,mean))
plot(n.trees,berr,pch=19,ylab="Mean Squared Error",xlab="# Trees",main="Boosting Test Error")
abline(h=min(test.err),col=2,lwd=2)


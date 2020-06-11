## ----results="hide"-----------------------------------------------------------
knitr::opts_chunk$set(warning=FALSE, message=FALSE)
knitr::purl("tree-based-methods.Rmd")


## ----results="hide"-----------------------------------------------------------
install.packages('tree')
library(tree)
library(ISLR)
install.packages('MASS')
library(MASS)
install.packages('randomForest')
library(randomForest)


## -----------------------------------------------------------------------------
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston = tree(medv~., Boston, subset=train)
summary(tree.boston)


## -----------------------------------------------------------------------------
plot(tree.boston)
text(tree.boston, pretty=0)


## -----------------------------------------------------------------------------
cv.boston = cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type='b')


## -----------------------------------------------------------------------------
prune.boston = prune.tree(tree.boston, best=5)
plot(prune.boston)
text(prune.boston, pretty =0)


## -----------------------------------------------------------------------------
yhat = predict(tree.boston, newdata=Boston[-train,])
boston.test = Boston[-train,"medv"]
plot(yhat, boston.test)
abline(0,1)
mean((yhat - boston.test)^2)


## -----------------------------------------------------------------------------
set.seed(1)
bag.boston = randomForest(medv~., data=Boston, subset=train, mtry=13, importance=TRUE)
bag.boston


## -----------------------------------------------------------------------------
yhat.bag = predict(bag.boston, newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean((yhat.bag - boston.test)^2)


## -----------------------------------------------------------------------------
bag.boston = randomForest(medv~., data=Boston, subset=train, mtry=13, ntree=25)
yhat.bag = predict(bag.boston, newdata=Boston[-train,])
mean((yhat.bag - boston.test)^2)


## -----------------------------------------------------------------------------
set.seed(1)
rf.boston = randomForest(medv~., data=Boston, subset=train, mtry=6, importance=TRUE)
yhat.rf = predict(rf.boston, newdata=Boston[-train,])
mean((yhat.rf - boston.test)^2)


## -----------------------------------------------------------------------------
importance(rf.boston)


## -----------------------------------------------------------------------------
varImpPlot(rf.boston)


## -----------------------------------------------------------------------------
library(gbm)
set.seed(1)
boost.boston = gbm(medv~.,data=Boston[train,], distribution="gaussian", n.trees=5000, interaction.depth=4)


## -----------------------------------------------------------------------------
summary(boost.boston)


## -----------------------------------------------------------------------------
par(mfrow=c(1,2))
plot(boost.boston, i="rm")
plot(boost.boston, i="lstat")


## -----------------------------------------------------------------------------
yhat.boost = predict(boost.boston, newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost - boston.test)^2)


## -----------------------------------------------------------------------------
boost.boston = gbm(medv~., data=Boston[train ,], distribution="gaussian", n.trees =5000, interaction.depth=4, shrinkage=0.2, verbose=F)
yhat.boost = predict(boost.boston, newdata=Boston[-train,], n.trees=5000)
mean((yhat.boost - boston.test)^2)


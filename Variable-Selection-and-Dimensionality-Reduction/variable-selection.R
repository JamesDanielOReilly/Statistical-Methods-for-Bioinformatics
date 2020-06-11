## ----results="hide"-----------------------------------------------------------
knitr::opts_chunk$set(warning=FALSE, message=FALSE)
knitr::purl("variable-selection.Rmd")


## ---- results="hide"----------------------------------------------------------
install.packages('ISLR')
install.packages('leaps')
install.packages('glmnet')
install.packages('pls')


## ---- results="hide"----------------------------------------------------------
library(ISLR)
library(leaps)
attach(Hitters)

Hitters = na.omit(Hitters)


## -----------------------------------------------------------------------------
regfit.full = regsubsets(Salary~., Hitters)
summary(regfit.full)


## -----------------------------------------------------------------------------
regfit.full = regsubsets(Salary~., data=Hitters, nvmax=19)
reg.summary = summary(regfit.full)
names(reg.summary)
reg.summary$rsq


## -----------------------------------------------------------------------------
par(mfrow=c(2,2))
plot(reg.summary$rss, xlab="Number of Variables ", ylab="RSS", type="l")

plot(reg.summary$adjr2, xlab="Number of Variables ", ylab="Adjusted RSq", type="l")
max.adjr2 = which.max(reg.summary$adjr2)
points(max.adjr2, reg.summary$adjr2[max.adjr2], col="red", cex=2, pch =20)

plot(reg.summary$cp, xlab="Number of Variables ", ylab="Cp", type="l")
min.cp = which.min(reg.summary$cp)
points(min.cp ,reg.summary$cp[min.cp], col ="red", cex=2, pch =20)

plot(reg.summary$bic, xlab="Number of Variables ", ylab="BIC", type="l")
min.bic = which.min(reg.summary$bic)
points(min.bic, reg.summary$bic[min.bic],col="red",cex=2,pch =20)


## -----------------------------------------------------------------------------
plot(regfit.full, scale="r2")
plot(regfit.full, scale="adjr2")
plot(regfit.full, scale="Cp")
plot(regfit.full, scale="bic")


## -----------------------------------------------------------------------------
coef(regfit.full, 6)


## -----------------------------------------------------------------------------
regfit.fwd = regsubsets(Salary~., data=Hitters, nvmax=19, method="forward")
summary(regfit.fwd)
regfit.bwd = regsubsets(Salary~., data=Hitters, nvmax=19, method="backward")
summary(regfit.bwd)


## -----------------------------------------------------------------------------
x = model.matrix(Salary~., Hitters)[,-1]
y = Hitters$Salary


## -----------------------------------------------------------------------------
library(glmnet)
grid=10^seq(10,-2, length =100)
ridge.mod=glmnet (x,y,alpha=0, lambda=grid)


## -----------------------------------------------------------------------------
dim(coef(ridge.mod))


## -----------------------------------------------------------------------------
ridge.mod$lambda[50]
coef(ridge.mod)[,50]
sqrt(sum(coef(ridge.mod)[-1,50]^2))


## -----------------------------------------------------------------------------
ridge.mod$lambda[60]
coef(ridge.mod)[,60]
sqrt(sum(coef(ridge.mod)[-1,60]^2))


## -----------------------------------------------------------------------------
predict(ridge.mod, s=50, type="coefficients")[1:20,]


## -----------------------------------------------------------------------------
set.seed(1)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
y.test = y[test]


## -----------------------------------------------------------------------------
ridge.mod = glmnet(x[train,], y[train], alpha=0, lambda=grid, thresh=1e-12)
ridge.pred = predict(ridge.mod, s=4, newx=x[test,])
mean((ridge.pred-y.test)^2)


## -----------------------------------------------------------------------------
set.seed(1)
cv.out = cv.glmnet(x[train,], y[train], alpha=0)
plot(cv.out)
bestlam = cv.out$lambda.min
bestlam


## -----------------------------------------------------------------------------
ridge.pred = predict(ridge.mod, s=bestlam, newx=x[test,])
mean((ridge.pred-y.test)^2)


## -----------------------------------------------------------------------------
out = glmnet(x, y, alpha=0)
predict(out, type="coefficients", s= bestlam)[1:20,]


## -----------------------------------------------------------------------------
library(pls)
set.seed(2)
pcr.fit = pcr(Salary~., data=Hitters, scale=TRUE, validation="CV")


## -----------------------------------------------------------------------------
summary(pcr.fit)


## -----------------------------------------------------------------------------
validationplot(pcr.fit, val.type="MSEP")


## -----------------------------------------------------------------------------
set.seed(1)
pcr.fit = pcr(Salary~., data=Hitters, subset=train, scale=TRUE, validation="CV")
validationplot(pcr.fit, val.type="MSEP")


## -----------------------------------------------------------------------------
pcr.pred = predict(pcr.fit, x[test,], ncomp=5)
mean((pcr.pred-y.test)^2)


## -----------------------------------------------------------------------------
pcr.fit = pcr(y~x, scale=TRUE, ncomp=5)
summary(pcr.fit)


## -----------------------------------------------------------------------------
set.seed(1)
pls.fit = plsr(Salary~., data=Hitters, subset=train, scale=TRUE, validation="CV")
summary(pls.fit)
validationplot(pls.fit, val.type="MSEP")


## -----------------------------------------------------------------------------
pls.pred=predict(pls.fit, x[test,], ncomp=1)
mean((pls.pred-y.test)^2)


## -----------------------------------------------------------------------------
pls.fit = plsr(Salary~., data=Hitters, scale=TRUE, ncomp=1)
summary (pls.fit)


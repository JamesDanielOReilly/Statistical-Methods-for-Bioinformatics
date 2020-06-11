## ----results="hide"-----------------------------------------------------------
knitr::opts_chunk$set(warning=FALSE, message=FALSE)
knitr::purl("resampling-methods.Rmd")


## -----------------------------------------------------------------------------
install.packages('ISLR')
install.packages('boot')


## -----------------------------------------------------------------------------
library(ISLR)
attach(Auto)
set.seed(1)
train=sample(392,196)

lm.fit=lm(mpg~horsepower, data=Auto, subset=train)


## -----------------------------------------------------------------------------
mean((mpg-predict(lm.fit ,Auto))[-train ]^2)


## -----------------------------------------------------------------------------
lm.fit2 = lm(mpg~poly(horsepower, 2), data=Auto, subset=train)
mean((mpg-predict(lm.fit2, Auto))[- train]^2)
lm.fit3 = lm(mpg~poly(horsepower, 3), data=Auto, subset=train)
mean((mpg-predict(lm.fit3, Auto))[- train]^2)


## -----------------------------------------------------------------------------
library(boot)
glm.fit=glm(mpg~horsepower, data=Auto)
cv.err=cv.glm(Auto, glm.fit)
cv.err$delta


## -----------------------------------------------------------------------------
cv.error = rep(0,5)
for (i in 1:5){
glm.fit = glm(mpg~poly(horsepower, i), data=Auto)
cv.error[i] = cv.glm(Auto, glm.fit)$delta [1]
}
cv.error


## -----------------------------------------------------------------------------
set.seed(17)
cv.error.10 = rep(0,10)
for (i in 1:10){
glm.fit = glm(mpg~poly(horsepower, i), data=Auto)
cv.error.10[i] = cv.glm(Auto, glm.fit, K=10)$delta[1]
}
cv.error.10


## -----------------------------------------------------------------------------
boot.fn = function(data, index)
return(coef(lm(mpg~horsepower, data=data, subset=index)))
boot.fn(Auto, 1:392)


## -----------------------------------------------------------------------------
set.seed(1)
boot.fn(Auto, sample(392,392,replace=T))
boot.fn(Auto, sample(392,392,replace=T))


## -----------------------------------------------------------------------------
boot(Auto ,boot.fn ,1000)


## -----------------------------------------------------------------------------
summary(lm(mpg~horsepower, data=Auto))$coef


## -----------------------------------------------------------------------------
boot.fn = function(data, index)
coefficients(lm(mpg~horsepower+I(horsepower^2), data=data, subset=index))
set.seed(1)
boot(Auto, boot.fn, 1000)


## -----------------------------------------------------------------------------
summary(lm(mpg~horsepower+I(horsepower^2), data=Auto))$coef


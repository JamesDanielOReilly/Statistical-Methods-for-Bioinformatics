## ----results="hide"-----------------------------------------------------------
knitr::opts_chunk$set(warning=FALSE, message=FALSE)
knitr::purl("missing-data.Rmd")


## -----------------------------------------------------------------------------
install.packages('mice')
install.packages('lattice')
install.packages('VIM')
install.packages('aod')
install.packages('BaM')

library(mice)
library(lattice)
library(VIM)
library(aod)
library(BaM)


## -----------------------------------------------------------------------------
titanic = read.table("titanic.txt", header=T, sep=",")
titanic.missing = titanic[,c(1,2,3,5,11)]
head(titanic.missing, 10)


## -----------------------------------------------------------------------------
titanic.missing.aggr = aggr(titanic.missing, numbers=TRUE, prop=FALSE, ylab=c("Histogram of missing data","Pattern"))
titanic.missing.aggr

aggr(titanic.missing, combined=TRUE, numbers=TRUE, prop=TRUE, cex.numbers=0.87, varheight=FALSE)


## -----------------------------------------------------------------------------
barMiss(titanic.missing[,c("survived","age")])


## -----------------------------------------------------------------------------
barMiss(titanic.missing[,c("sex","age")])
histMiss(titanic.missing)


## -----------------------------------------------------------------------------
titanic.logistic.omit = glm(survived ~ pclass+sex+age, family=binomial, data=titanic.missing)
summary(titanic.logistic.omit)


## -----------------------------------------------------------------------------
wald.test(b=coef(titanic.logistic.omit), Sigma=vcov(titanic.logistic.omit), Terms=2:3)


## -----------------------------------------------------------------------------
exp(cbind(OR=titanic.logistic.omit$coefficients, confint(titanic.logistic.omit)))


## -----------------------------------------------------------------------------
pattern = md.pattern(titanic.missing)
pattern

pairs = md.pairs(titanic.missing)
pairs


## ----results="hide"-----------------------------------------------------------
imp = mice(titanic.missing, m=100)
imp


## -----------------------------------------------------------------------------
imp$imp$age[1:10,1:5]


## -----------------------------------------------------------------------------
complete(imp,1)[1:10,]


## -----------------------------------------------------------------------------
com = complete(imp, "long", inc=T)
col = rep(c("blue","red")[1+as.numeric(is.na(imp$data$age))],101)
stripplot(age~.imp, data=com, jit=TRUE, fac=0.8, col=col, pch=20, cex=1.4, xlab="Imputation number")


## -----------------------------------------------------------------------------
# Analyzing the imputed data sets
fit = with(data=imp, exp=glm(survived ~ pclass + sex + age, family=binomial))

# Creating a data set with the results of all the analysis
MI.matrix<-matrix(0,100,5)
for(k in 1:100) {
  MI.matrix[k,] = coefficients(fit$analyses[[k]])
  MI.results = data.frame(Intercept=MI.matrix[,1], pclass2=MI.matrix[,2], pclass3=MI.matrix[,3], sex=MI.matrix[,4], age=MI.matrix[,5])
}

MI.results[1:10,]


## -----------------------------------------------------------------------------
est = pool(fit)
summary(est)


## -----------------------------------------------------------------------------
titanic.missing$r = as.numeric(!is.na(titanic.missing$age))*as.numeric(!is.na(titanic.missing$sex))
head(titanic.missing,15)


## -----------------------------------------------------------------------------
titanic.ipw.glm<-glm(r ~ pclass + survived, data=titanic.missing,family=binomial)
summary(titanic.ipw.glm)


## -----------------------------------------------------------------------------
titanic.missing$w<-1/fitted(titanic.ipw.glm)
head(titanic.missing, 15)


## -----------------------------------------------------------------------------
titanic.results.ipw = glm(survived ~ pclass + sex + age, data=titanic.missing, weights=titanic.missing$w, family=binomial)
summary(titanic.results.ipw)


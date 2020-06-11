## ----results="hide"-----------------------------------------------------------
knitr::opts_chunk$set(warning=FALSE, message=FALSE)
knitr::purl("linear-regression.Rmd")


## -----------------------------------------------------------------------------
install.packages('pastecs')


## -----------------------------------------------------------------------------
library(pastecs)
kalama = read.table("kalama.txt", header=T)
attach(kalama)
kalama


## -----------------------------------------------------------------------------
options(digits=2)
descrip.kalama = stat.desc(kalama[,c("age","height")],basic=TRUE, desc=TRUE)
descrip.kalama


## -----------------------------------------------------------------------------
cov.age.height = cov(age, height)
corr.age.height = cor(age, height)
cov.age.height
corr.age.height


## -----------------------------------------------------------------------------
corr.age.height.test = cor.test(age, height, alternative="two.sided", method="pearson")
corr.age.height.test


## -----------------------------------------------------------------------------
plot(age, height, main="Age vs Height", xlab="Age", ylab="Height", pch=19)
abline(lm(height~age), col="red")


## -----------------------------------------------------------------------------
res = lm(height~age, data=kalama)
kalama.anova = anova(res)
kalama.summary = summary(res)
kalama.anova
kalama.summary


## -----------------------------------------------------------------------------
satisfaction = read.table("satisfaction.txt", header=T)
attach(satisfaction)
satisfaction


## -----------------------------------------------------------------------------
cor(satisfaction)
plot(satisfaction)


## -----------------------------------------------------------------------------
options(digits=2)
descrip.satisfaction = stat.desc(satisfaction,basic=TRUE, desc=TRUE)
descrip.satisfaction


## -----------------------------------------------------------------------------
satisfaction.lm = lm(satis~age+severity+anxiety, data=satisfaction)
satisfaction.summary = summary(satisfaction.lm)
satisfaction.summary


## -----------------------------------------------------------------------------
satisfaction.lm.int = lm(satis~1, data=satisfaction) # Null model
anova(satisfaction.lm.int,satisfaction.lm) # Null versus full


## -----------------------------------------------------------------------------
satisfaction.anova = anova(satisfaction.lm)
satisfaction.anova


## -----------------------------------------------------------------------------
satisfaction.lm.final = lm(satis~age+anxiety, data=satisfaction)
satisfaction.final.summary = summary(satisfaction.lm.final)
satisfaction.final.summary


## -----------------------------------------------------------------------------
newdata = data.frame(age=43, anxiety=2.7)
pred.w.plim = predict(satisfaction.lm.final, newdata, interval="predict")
pred.w.clim = predict(satisfaction.lm.final, newdata, interval = "confidence")
pred.w.plim
pred.w.clim


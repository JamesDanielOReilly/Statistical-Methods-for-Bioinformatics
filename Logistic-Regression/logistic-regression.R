## ----results="hide"-----------------------------------------------------------
knitr::opts_chunk$set(warning=FALSE, message=FALSE)
knitr::purl("logistic-regression.Rmd")


## -----------------------------------------------------------------------------
install.packages("AICcmodavg")


## -----------------------------------------------------------------------------
donner = read.table("donner-class.txt", row.names = 1, header=TRUE)
attach(donner)
head(donner,10)

# Keeping only the variables of interest
donner.na = na.omit(subset(donner,select=c('Age','Outcome','Sex')))
donner.na$fem = as.numeric(donner.na$Sex=="Female")
head(donner.na,10)


## -----------------------------------------------------------------------------
donner.log = glm(Outcome ~ Age+fem, data=donner.na, family=binomial(link="logit"))
summary(donner.log)


## -----------------------------------------------------------------------------
exp(donner.log$coefficients)
exp(confint(donner.log))
exp(cbind(OR=donner.log$coefficients, confint(donner.log)))


## -----------------------------------------------------------------------------
#exp(donner.log$coefficients*10)
exp(c(OR=donner.log$coefficients[2]*10, confint(donner.log)[2,]*10))



## -----------------------------------------------------------------------------
logit = function(x){
  log(x/(1-x))
}
ilogit = function(x,a,b){
  exp(a+b*x)/(1+exp(a+b*x))
}

# Plotting survival for men vs women
cl = coef(donner.log)
plot(donner.na$Age, jitter(donner.na$Outcome,.2), col=Sex, pch=20, cex=1.2, xlab="Age", ylab="Status (jittered)")
curve(ilogit(cl[1]+cl[2]*x+cl[3]*0, 0, 1), add=T)
curve(ilogit(cl[1]+cl[2]*x+cl[3]*1, 0, 1), add=T, col="red")
legend("topright", pch=20, lty="solid", col=c("red","black"), c("women","men"))


## -----------------------------------------------------------------------------
newdata2 = data.frame(fem=1, Age=mean(donner.na$Age))
newdata2$greP = predict(donner.log, newdata=newdata2,type="response")
newdata2


## -----------------------------------------------------------------------------
newdata3 = data.frame(fem=0, Age=mean(donner.na$Age))
newdata3$greP = predict(donner.log, newdata=newdata3,type="response")
newdata3

## -----------------------------------------------------------------------------
newdata4 = data.frame(fem=c(0,1), Age=mean(donner.na$Age))
newdata4$greP = predict(donner.log, newdata=newdata4,type="response")
newdata4


## -----------------------------------------------------------------------------
m4 = glm(Outcome ~ Age*fem, data=donner.na, family=binomial(link="logit"))
summary(m4)


## -----------------------------------------------------------------------------
donner.list=list()

donner.list[[1]] = glm(Outcome ~ Age,data=donner.na, family=binomial(link="logit"))
donner.list[[2]] = glm(Outcome ~ fem,data=donner.na, family=binomial(link="logit"))
donner.list[[3]] = glm(Outcome ~ Age+fem, data=donner.na, family=binomial(link="logit"))
donner.list[[4]] = glm(Outcome ~ Age*fem, data=donner.na, family=binomial(link="logit"))

donner.modnames = c("Age", "Sex", "Age+Sex", "Age+Sex+Age:Sex")


## -----------------------------------------------------------------------------
library('AICcmodavg')
donner.aictab=aictab(cand.set = donner.list, modnames = donner.modnames)
donner.aictab


## -----------------------------------------------------------------------------
modavg(cand.set=donner.list, parm="Age", second.ord=TRUE, modnames=donner.modnames, uncond.se="revised", exclude=list("Age:fem"), conf.level=0.95, warn=TRUE)

modavg(cand.set=donner.list, parm="fem", second.ord=TRUE, modnames=donner.modnames, uncond.se="revised", exclude=list("Age:fem"), conf.level=0.95, warn=TRUE)


## -----------------------------------------------------------------------------
x = seq(1, 70, 0.01)
y = exp(coef(m4)[3]+coef(m4)[4]*x)

plot(x, y, type="n", ylim=c(0.7,4.5), xlab="Age", ylab="Odds ratio", main="Odds ratio for gender")
lines(x, y, lty=1, col="red")
abline(h=1)


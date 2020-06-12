## ----results="hide"------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(warning=FALSE, message=FALSE)
knitr::purl("longitudinal-models.Rmd")


## ------------------------------------------------------------------------------------------------------------------------------
install.packages("Hmisc")
install.packages("lattice")
install.packages('grid')
install.packages('nlme')


## ------------------------------------------------------------------------------------------------------------------------------
ratpup = read.table("rat_pup.dat", h = T)
ratpup$sex1[ratpup$sex == "Female"] = 1
ratpup$sex1[ratpup$sex == "Male"] = 0
attach(ratpup)


## ------------------------------------------------------------------------------------------------------------------------------
library(Hmisc)
g = function(x){
  c(N=length(x), Mean=mean(x,na.rm=TRUE), SD=sd(x,na.rm=TRUE), Min=min(x,na.rm=TRUE), Max=max(x,na.rm=TRUE))
}
summarize(weight,by=llist(treatment,sex),g)


## ------------------------------------------------------------------------------------------------------------------------------
library(lattice) # trellis graphics
library(grid)

bwplot(weight ~ sex|treatment, data=ratpup,aspect = 2, ylab="Birth Weights", xlab="SEX", main = "Boxplots of birth weights for levels of treatment by sex")


## ------------------------------------------------------------------------------------------------------------------------------
dotplot(litterid ~ weight,group=treatment, data=ratpup, xlab="Weight", ylab="Litter", auto.key=list(space="top", column=3, cex=.8, title="", cex.title=1, lines=FALSE, points=TRUE))
with(ratpup, interaction.plot(treatment,sex,weight))


## ------------------------------------------------------------------------------------------------------------------------------
library(nlme)

meanfull.hom <- lme(weight ~ treatment + sex1 + litsize + treatment:sex1, random=~1 | litterid, ratpup, method="REML")


## ------------------------------------------------------------------------------------------------------------------------------
summary(meanfull.hom)


## ------------------------------------------------------------------------------------------------------------------------------
anova(meanfull.hom)


## ------------------------------------------------------------------------------------------------------------------------------
random.effects(meanfull.hom)


## ------------------------------------------------------------------------------------------------------------------------------
meanfull.het <- lme(weight ~ treatment + sex1 + litsize + treatment:sex1, random=~1 | litterid, ratpup, method = "REML", weights = varIdent(form = ~1 | treatment))


## ------------------------------------------------------------------------------------------------------------------------------
summary(meanfull.het)


## ------------------------------------------------------------------------------------------------------------------------------
anova(meanfull.hom, meanfull.het)


## ------------------------------------------------------------------------------------------------------------------------------
ratpup$trtgrp[treatment=="Control"] <- 1
ratpup$trtgrp[treatment == "Low" | treatment == "High"] <- 2

meanfull.hilo <- lme(weight ~ treatment + sex1 + litsize + treatment:sex1, random=~1 | litterid, ratpup, method = "REML", weights=varIdent(form=~1 | trtgrp))

summary(meanfull.hilo)
anova(meanfull.hilo)


## ------------------------------------------------------------------------------------------------------------------------------
anova(meanfull.het, meanfull.hilo)


## ------------------------------------------------------------------------------------------------------------------------------
meanfull.hilo.nolitter <- gls(weight ~ treatment + sex1 + litsize + treatment:sex1, data = ratpup, weights = varIdent(form = ~1 | trtgrp))

summary(meanfull.hilo.nolitter)


## ------------------------------------------------------------------------------------------------------------------------------
anova(meanfull.hilo.nolitter,meanfull.hilo)


## ------------------------------------------------------------------------------------------------------------------------------
meanfull.hilo.ml <- lme(weight ~ treatment + sex1 + litsize + treatment:sex1, random = ~1 | litterid, ratpup, method = "ML", weights = varIdent(form = ~1 | trtgrp))

summary(meanfull.hilo.ml)


## ------------------------------------------------------------------------------------------------------------------------------
summary(meanfull.hilo.ml)


## ------------------------------------------------------------------------------------------------------------------------------
anova(meanfull.hilo.ml)


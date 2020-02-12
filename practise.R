## libraries
library(pastecs)

## Reading the data
kalama = read.table("kalama.txt", header=T)

## Descriptive statistics
options(digits=2)
descrip.kalama <- stat.desc(kalama[,c("age", "height")], basic = TRUE, desc = TRUE)

## Calculating the covariance and correlation
cov.age.height <- cov(kalama$age, kalama$height)
cor.age.height <- cor(kalama$age, kalama$height)

## Testing if the population correlation is zero
corr.age.height.test <- cor.test(kamala$age, kalama$height, alternative="two.sided", method = "pearson")

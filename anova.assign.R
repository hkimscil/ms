######################################
#
# ToothGrowth eg assignment
#
######################################
rm(list=ls())
d <- ToothGrowth
str(d)
attach(d)
d$dose <- factor(dose)
str(d)

# anova test
d.ftest <- aov(len~supp*dose, data=d) 
summary(d.ftest)

mean.tot <- mean(len)
n.tot <- length(len)
df.tot <- n.tot - 1
var.tot <- var(len)
ss.tot <- var.tot * df.tot

var.tot
ss.tot
df.tot


mean.each <- tapply(len, list(supp,dose), mean)
mean.each

df.each <- tapply(len, list(supp,dose), length) - 1
df.each
var.each <-  tapply(len, list(supp,dose), var)
var.each
ss.each <-  tapply(len, list(supp,dose), var) * df.each
ss.each

ss.within <- sum(ss.each)
df.within <- sum(df.each)
ms.within <- ss.within/df.within

df.bet <- 6-1
df.supp <- 2-1
df.dose <- 3-1
df.ab <- df.bet - (df.supp+df.dose)

ss.bet <- ss.tot - ss.within
ss.bet
ms.bet <- ss.bet/df.bet

ss.supp <- sum(30*(tapply(len, supp, mean) - mean.tot)^2)
ss.dose <- sum(20*(tapply(len, dose, mean) - mean.tot)^2)
ss.ab <- ss.bet - (ss.supp + ss.dose)
ss.bet
ss.supp
ss.dose
ss.ab

ms.supp <- ss.supp/df.supp
ms.dose <- ss.dose/df.dose
ms.ab <- ss.ab/df.ab

summary(d.ftest)
ss.supp
ss.dose
ss.ab
df.supp
df.dose
df.ab
ss.within
df.within

ms.supp
ms.dose
ms.ab
ms.within 
f.supp <- ms.supp/ms.within
f.dose <- ms.dose/ms.within
f.ab <- ms.ab/ms.within
f.supp
f.dose
f.ab

summary(d.ftest)

# install.packages("gplots")
library(gplots)
plotmeans(len ~ interaction(supp, dose, sep=" "),
          connect=list(c(1, 3, 5),c(2, 4, 6)), 
          col=c("red","darkgreen"),
          main = "Interaction Plot with 95% CIs", 
          xlab="Treatment and Dose Combination")
# or
interaction.plot(dose, supp, len, mean)
interaction.plot(supp, dose, len, mean)


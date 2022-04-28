# ANOVA
# set.seed(1024)
n <-30
s1 <- round(rnorm(n, 5.6, 1),0)
s2 <- round(rnorm(n, 6.7, 1),0)
s3 <- round(rnorm(n, 8.9, 1),0)
s1
s2
s3


sam <- c(s1,s2,s3)
sam
group <- c(rep("g1",n), rep("g2",n), rep("g3",n))
group

sa <- data.frame(sam,group)
sa

df.tot <- 3*n-1
ss.tot <- var(sam)*df.tot
var.tot <- var(sam)
df.tot
ss.tot
var.tot

df.bet <- 3 - 1
df.s1 <- n-1
df.s2 <- n-1
df.s3 <- n-1
df.within <- df.s1+df.s2+df.s3

ss.s1 <- var(s1)*df.s1
ss.s2 <- var(s2)*df.s2
ss.s3 <- var(s3)*df.s3
ss.s1
ss.s2
ss.s3

mean.grand <- mean(sam)
mean.s1 <- mean(s1)
mean.s2 <- mean(s2)
mean.s3 <- mean(s3)
mean.grand
mean.s1
mean.s2
mean.s3

diff.g1 <- n * (mean.grand - mean.s1)^2
diff.g2 <- n * (mean.grand - mean.s2)^2
diff.g3 <- n * (mean.grand - mean.s3)^2

diff.g1 
diff.g2
diff.g3

ss.bet <-  diff.g1 + diff.g2 + diff.g3
ss.bet

ss.within <- ss.s1 + ss.s2 + ss.s3
ss.within
ss.tot
ss.bet + ss.within

ss.tot == ss.bet + ss.within
df.tot == df.bet + df.within

ms.bet <- ss.bet / df.bet
ms.wit <- ss.within / df.within
ms.bet
ms.wit

fvalue <- ms.bet/ms.wit

fvalue 

f.res <- (aov(sa$sam~sa$group))
summary(f.res)

##########################
# http://commres.net/wiki/r/oneway_anova
#
##########################
x1 <- c(50.5, 52.1, 51.9, 52.4, 50.6, 51.4, 51.2, 52.2, 51.5, 50.8)
x2 <- c(47.5, 47.7, 46.6, 47.1, 47.2, 47.8, 45.2, 47.4, 45.0, 47.9)
x3 <- c(46.0, 47.1, 45.6, 47.1, 47.2, 46.4, 45.9, 47.1, 44.9, 46.2)
xc <- data.frame(x1,x2,x3)
xs <- stack(xc)
xs

colnames(xs) <- c("score", "temp")
xs
levels(xs$temp) <- c("low", "mid", "hi")
xs

str(xs) # 데이터 구조 체크 

attach(xs)
aov.xs <- aov(score ~ temp, data = xs)
summary(aov.xs)

mean.tot <- mean(score)
mean.tot

df.tot <- length(score)-1
ss.tot <- var(score) * df.tot
ss.tot
df.tot
var.tot <- var(score)
var.tot2 <- ss.tot/df.tot
var.tot == var.tot2

mean.each <- tapply(score, list(temp), mean)
mean.each
var.each <- tapply(score, list(temp), var)
df.within <- 3*(10-1)
ss.each <- var.each*9
ss.within <- sum(ss.each)

ss.bet <- sum(10*(mean.tot - mean.each)^2)
df.bet <- 3-1

ss.tot
ss.bet + ss.within

df.tot
df.bet + df.within

ms.bet <- ss.bet / df.bet
ms.within <- ss.within / df.within
f.value <- ms.bet / ms.within

ss.bet
ss.within
df.bet
df.within
ms.bet
ms.within
f.value

alpha <- .05
qf(alpha, df.bet, df.within, lower.tail = F)
pf(f.value, df.bet, df.within, lower.tail = F)

aov.xs <- aov(score ~ temp, data = xs)
summary(aov.xs)


#################################################
# two-way anova
# subject = factor(paste('sub', 1:30, sep=''))
#################################################

n.a <- 3 # a treatment 숫자
n.b <- 2 # b 그룹 숫자
n.sub <- 30 # 총 샘플 숫자
# 데이터 생성
set.seed(9)
a <- gl(3,10,30, labels=c('a1', 'a2', 'a3'))
b <- gl(2,5,30, labels=c('b1', 'b2'))
a
b
y <- rnorm(30, mean=10) + 3.14 * (a=='a1' & b=='b2') + 1.43 * (a=='a3' & b=='b2') 
y

dat <- data.frame(a, b, y)
aov.dat <- aov(y~a*b) # anova test
summary(aov.dat) # summary of the test output

# hand calculation
table(a,b)

tapply(y, list(a,b), mean) # 각 셀에서의 평균
tapply(y, list(a,b), length) # 각 셀에서의 샘플숫자
df.within <- 5-1 # df within
var.within <- tapply(y, list(a,b), var) # var.within
ss.within.each <- tapply(y, list(a,b), var)*df.within
ss.within <- sum(ss.within.each) # ss.within
ss.within


interaction.plot(a,b,y)

mean.a <- tapply(y, list(a), mean)
mean.b <- tapply(y, list(b), mean)
mean.a
mean.b

var.a <- tapply(y, list(a), var)
var.b <- tapply(y, list(b), var)


mean.tot <- mean(dat$y)
var.tot <- var(dat$y)
df.tot <- n.sub - 1 
ss.tot <- var.tot * df.tot

## between
mean.each <- tapply(y, list(a,b), mean)
mean.each
mean.tot <- mean(y)
mean.tot
ss.bet <- sum(5*(mean.each-mean.tot)^2)

ss.tot
ss.within
ss.bet
ss.bet + ss.within

ss.a <- sum(10 * ((mean.tot - mean.a)^2))
ss.b <- sum(15 * ((mean.tot - mean.b)^2))
ss.a
ss.b
ss.ab <- ss.bet - (ss.a + ss.b)
ss.ab

ss.tot
ss.bet
ss.within
ss.a
ss.b
ss.ab

df.tot <- n.sub - 1
df.bet <- 6 - 1
df.a <- 3 - 1
df.b <- 2 - 1
df.ab <- df.bet - (df.a + df.b)
df.within <- 6 * (5 - 1)

df.tot
df.bet
df.a
df.b
df.ab
df.within

ms.a <- ss.a / df.a
ms.b <- ss.b / df.b
ms.ab <- ss.ab / df.ab
ms.within <- ss.within / df.within

ms.a
ms.b
ms.ab
ms.within


f.a <- ms.a / ms.within
f.b <- ms.b / ms.within
f.ab <- ms.ab / ms.within

alpha <- .05
f.a
# 봐야할 F분포표에서의 F-value
# qt 처럼 qf 사용
# qf(alpha, df.between, df.within, lower.tail=F) 처럼 사용
qf(alpha, df.a, df.within, lower.tail = F)
pf(f.a, df.a, df.within, lower.tail = F)
f.b
qf(alpha, df.b, df.within, lower.tail = F)
pf(f.b, df.b, df.within, lower.tail = F)
f.ab
qf(alpha, df.ab, df.within, lower.tail = F)
pf(f.ab, df.ab, df.within, lower.tail = F)
# aov result
summary(aov.dat)




## HEADER ####
## HARUp!
## Basic power analysis with {pwr}
## Last editor: Ed
## Last edited: 2019.10.15

## CONTENTS ####
## 01 Library pwr
## 02 1 and 2 proportions test
## 03 1 and 2 sample t-test
## 04 Correlation
## 05 Chi squared
## 06 General linear model
## 07 Data for power curves
## 08 Setting up a Graph of Power

## 01 Library pwr ####
# install.packages("pwr", dependencies=T)
library(pwr)

## 02 1 and 2 proportions test ####

# 1 sample

help(ES.h)
(myProp <- ES.h(p1 = 0.5, p2 = 0.25)) #effect size h

help(pwr.p.test)

pwr.p.test(h = myProp, n = NULL, sig.level = 0.05, power = 0.8)

pwr.p.test(h = myProp, n = 25, sig.level = 0.05, power = NULL)

#2 samples
help(pwr.2p.test)

pwr.2p.test(h=ES.h(0.5,0.40), power = 0.8, sig.level=0.05, alternative = "two.sided")

pwr.2p.test(h=ES.h(0.5,0.40), power = 0.8, sig.level=0.05, alternative = "greater")

ES.h(0.8, 0.65)
pwr.p.test(h = ES.h(0.8, 0.65), n = NULL, power = 0.8, sig.level = 0.05) #69
pwr.p.test(h = ES.h(0.8, 0.65), n = 55, sig.level = 0.05)#0.71

## 03 1 and 2 sample t-test ####
#t-test e.s.
#d = abs(mean.01 - mean.02)/(standard dev)

?pwr.t.test
pwr.t.test(d=0.2,n=60,sig.level=0.05,
           type="one.sample",alternative="two.sided") #0.33 power

pwr.t.test(d=0.2,n=NULL,sig.level=0.05, power=0.80,
           type="one.sample",alternative="two.sided") #

pwr.t.test(d=0.5,n=40,sig.level=0.05,
           type="paired",alternative="two.sided") #good power

pwr.t.test(d=(2/2.8),n=30,sig.level=0.05,
           type="two.sample",alternative="two.sided") #okay power



## 04 Correlation ####
?pwr.r.test

pwr.r.test(r=0.3, n=50, sig.level=0.05, alternative="two.sided") #p=0.56

pwr.r.test(r=0.3, n=50, sig.level=0.05, alternative="greater") #p=0.69

pwr.r.test(r=0.3, power=.8, sig.level=0.05, alternative="two.sided")

pwr.r.test(r=0.5, power=.8, sig.level=0.05, alternative="two.sided")

pwr.r.test(r=0.1, power=.8, sig.level=0.05, alternative="two.sided")

## 05 Chi squared ####

?pwr.chisq.test

pwr.chisq.test(w=0.5, df=(3-1), N=200)
pwr.chisq.test(w=0.5, df=(3-1), power=0.8)  #N=~39 total N

#w for obs 40 and 60 exp 50 and 50
p01 = 50/100; p02 = 50/100; p11 = 40/100; p12 = 60/100
w = sqrt( sum(  ( (p01-p11)^2 /p01  ) + ( (p02-p12)^2/p02 )  ) )
w

## 06 General linear model ####
?pwr.f2.test

pwr.f2.test(u = 3, v = 97, f2 = .15, sig.level = 0.5)
pwr.f2.test(u = 3, v = , f2 = .05, sig.level = 0.5, power=0.8) #57 + 3!

## 07 Data for power curves ####
# Plot sample size curves for detecting correlations of
# various sizes.


# range of correlations
r <- seq(.1,.5,.01)
nr <- length(r)

# power values
p <- seq(.4,.9,.1)
np <- length(p)

# obtain sample sizes
?array
samsize <- array(data = numeric(nr*np), dim=c(nr,np)) #initialize array

Foelix <- pwr.r.test(n = NULL, r = .5,
                     sig.level = .05, power = .5,
                     alternative = "two.sided")
Foelix
class(Foelix)
names(Foelix)
Foelix$n

for (i in 1:np){
  for (j in 1:nr){
    Foelix <- pwr.r.test(n = NULL, r = r[j],
                         sig.level = .05, power = p[i],
                         alternative = "two.sided")
    samsize[j,i] <- ceiling(Foelix$n)
  }
}

head(samsize)

# 08 Setting up a Graph of Power  (TM) ####

# set up graph
xrange <- range(r)
yrange <- round(range(samsize))
colors <- rainbow(length(p))
plot(xrange, yrange, type="n",
     xlab="Correlation Coefficient (r)",
     ylab="Sample Size (n)" )

# add power curves
for (i in 1:np){
  lines(r, samsize[,i], type="l", lwd=2, col=colors[i])
}

# add annotation (grid lines, title, legend)
abline(v=0, h=seq(0,yrange[2],50), lty=2, col="grey89")
abline(h=0, v=seq(xrange[1],xrange[2],.02), lty=2,
       col="grey89")
title("Sample Size Estimation for Correlation Studies\n
  Sig=0.05 (Two-tailed)")
legend("topright", title="Power", as.character(p),
       fill=colors)




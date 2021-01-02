###Analysis of Environmental Data
###Probability distributions -- R script

###1. Set up your R work session
setwd('c:/work/stats/ecodata/lab/distributions/')
source('c:/work/r/functions/biostats.R')

###3. Plotting probability distributions

#3.1 Probability mass or density

dbinom(4,size=10,prob=.3)
dbinom(0:10,size=10,prob=.3)
barplot(dbinom(0:10,size=10,prob=.3),names=c(0:10),
	xlab='# Presences',ylab='Probability',main='Probability Mass')

dnorm(12,mean=10,sd=3)
dnorm(0:20,mean=10,sd=3)
curve(dnorm(x,mean=10,sd=3),0,20,xlab='Z',ylab='Probability density',main='Probability Density')

#3.2 Cumulative probability distribution

barplot(pbinom(0:10,size=10,prob=.3),names=c(0:10),
	xlab='# Presences',ylab='Cumulative probability (quantile)',main='Cumulative Probability')

curve(pnorm(x,mean=10,sd=3),0,20,xlab='Z',ylab='Cumulative probability (Quantile)',main='Cumulative Probability')

#3.3 Quantile distribution
barplot(qbinom(seq(0,1,.1),size=10,prob=.3),names=seq(0,1,.1),
	xlab='Cumulative probability (Quantile)',ylab='Zz',main='Quantiles')

curve(qnorm(x,mean=10,sd=3),0,1,xlab='Cumulative probability (Quantile)',ylab='Z',main='Quantiles')

#3.4 Random numbers
rbinom(1,size=10,prob=.3)
y<-rbinom(1000,size=10,prob=.3)
hist(y,xlab='# Successes',ylab='Frequency',main='Random Observations',col='gray')

y<-rnorm(1000,mean=10,sd=3)
hist(y,xlab='Z',ylab='Frequency',main='Random Observations',col='gray')

#3.5 Other plotting functions

dbinom.plot(size=1,prob=c(.1,.5,.9))
dnorm.plot(mean=c(10,12),sd=c(1,2,3),xlim=c(0,20),ylim=c(0,1))

binom.plot(size=c(10,100),prob=c(.1,.5,.9))
norm.plot(mean=c(10,12),sd=c(1,2,3),xlim=c(0,20))

###4. Bestiary of probability distributions

#Discrete distributions

dpois.plot(events=25,lambda=c(.5,1,3,12))
pois.plot(events=25,lambda=c(.5,1,3,12))

dnbinom.plot(events=25,mu=c(1,2),size=c(.1,1,10))
nbinom.plot(events=25,mu=c(1,2),size=c(.1,1,10))

#Continuous distributions

dgamma.plot(shape=c(1,2,5),scale=c(1,2,3),xlim=c(0,25),ylim=c(0,1))
gamma.plot(shape=c(1,2,5),scale=c(1,2,3),xlim=c(0,25))

dexp.plot(rate=c(1,.5,.1),xlim=c(0,15),ylim=c(0,1))
exp.plot(rate=c(1,.5,.1),xlim=c(0,15))

dbeta.plot(shape1=c(.5,1,2),shape2=c(.5,1,2),ylim=c(0,5))
beta.plot(shape1=c(.5,1,2,5),shape2=c(.5,1,2,5))

dlnorm.plot(mean=c(0,2),sd=c(.2,.5,1),xlim=c(0,15),ylim=c(0,2))
lnorm.plot(mean=c(0,2),sd=c(.2,.5,1),xlim=c(0,15))

dchisq.plot(df=c(1,2,10),xlim=c(0,20))
chisq.plot(df=c(1,2,10),xlim=c(0,20))

df.plot(df1=c(1,2,20),df2=c(10,20))
f.plot(df1=c(1,2,20),df2=c(10,20))

dt.plot(df=c(1,10,100))
t.plot(df=c(1,10))
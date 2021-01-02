###Analysis of Environmental Data
###Classical tests -- R script

###1. Set up your R work session
setwd('c:/work/stats/ecodata/lab/classical/')
source('c:/work/r/packages/biostats/r/biostats.R')


###2. Single samples

catrate<-read.csv('catrate.csv',header=TRUE)
catrate

#2.1 Distribution plots and tests for normality

uv.plots(catrate,var='cat.rate')

#show normal plot with catrate
hist(catrate$cat.rate,prob=TRUE,col='grey')
curve(dnorm(x,mean=mean(catrate$cat.rate),sd=sd(catrate$cat.rate)),
      lwd=2,add=TRUE)

#tests for normality
shapiro.test(catrate$cat.rate)
library(nortest)
ad.test(catrate$cat.rate)
sf.test(catrate$cat.rate)
cvm.test(catrate$cat.rate)
lillie.test(catrate$cat.rate)
pearson.test(catrate$cat.rate)

#2.2 Tests for difference from expectation

t.test(catrate$cat.rate,mu=2/7)

#####################################################
#supplemental - manual calculation of two-sided t-test
#####################################################

(tStat<-(mean(catrate$cat.rate)-(2/7))/(sd(catrate$cat.rate)/sqrt(nrow(catrate))))
t.plot(df=12)
(pval<-2*(1-pt(tStat,df=12)))                                

#####################################################

t.test(catrate$cat.rate,mu=2/7,alternative='greater')
wilcox.test(catrate$cat.rate,mu=2/7,conf.int=TRUE)	
wilcox.test(catrate$cat.rate,mu=2/7,alternative='greater',conf.int=TRUE)	
success<-sum(catrate$success)
years<-sum(catrate$years)
binom.test(success,years)
binom.test(success,years,p=5/7)
binom.test(success,years,p=5/7,alternative='less')

#####################################################
#supplemental - manual calculation
#####################################################

#first show binomial pmf
binom.plot(size=61,p=.71)

#demonstrate the binomial test - lower one-sided test
sum(dbinom(0:33,size=61,prob=5/7))
pbinom(33,size=61,prob=5/7)

#demonstrate the binomial test - two sided using PDFs
(lower.tail<-sum(dbinom(0:33,size=61,prob=5/7)))
(expected<-(5/7)*61)
(effect.size<-expected-33)
(upper<-round(expected+effect.size))
(upper.tail<-sum(dbinom(upper:61,size=61,prob=5/7)))
lower.tail+upper.tail

#demonstrate the binomial test - two sided using CDFs
(lower.tail<-pbinom(33,size=61,prob=5/7))
(upper.tail<-1-pbinom(upper-1,size=61,prob=5/7)) #note,need to subtract 1
lower.tail+upper.tail

#####################################################


###3. Two samples

#3.1 Comparing two variances

veg<-read.csv('vegdata.csv',header=TRUE)
veg
box.plots(veg,var='pine',by='treatment')
var.test(pine~treatment,data=veg,
         subset=treatment %in% c('control','clipped'))


#####################################################
#supplemental - manual calculation
#####################################################

(Fratio<-var(veg$pine[veg$treatment=='clipped'])/
   var(veg$pine[veg$treatment=='control']))
2*(1-pf(Fratio,df1=7,df2=7)) #2* for a two-sided test

#####################################################


shapiro.test(veg$pine[veg$treatment=='control'])
shapiro.test(veg$pine[veg$treatment=='clipped'])
fligner.test(pine~treatment,data=veg,
             subset=treatment %in% c('control','clipped'))
bartlett.test(pine~treatment,data=veg)
fligner.test(pine~treatment,data=veg)


#3.2 Comparing two sample means (or medians)

t.test(pine~treatment,data=veg,
       subset=treatment %in% c('control','clipped'),
       conf.int=TRUE,var.equal=TRUE)

#####################################################
#supplemental - manual calculation
#####################################################

control<-veg$pine[veg$treatment=='control']
clipped<-veg$pine[veg$treatment=='clipped']
(diff<-mean(clipped)-mean(control))
(SEpooled<-sqrt((var(clipped)/length(clipped))+(var(control)/length(control))))
(tStat<-diff/SEpooled)
(pval<-2*(1-pt(tStat,df=length(control)-1+length(clipped)-1))) #two-sided test

#####################################################

t.test(pine~treatment,data=veg,
       subset=treatment %in% c('control','clipped'),
       conf.int=TRUE,var.equal=FALSE)

wilcox.test(pine~treatment,data=veg,
            subset=treatment %in% c('control','clipped'),
            conf.int=TRUE)	


#3.3 Tests on paired samples

control<-veg$pine[veg$treatment=='control']
clipped<-veg$pine[veg$treatment=='clipped']
t.test(control,clipped,paired=TRUE)

#####################################################
#supplemental - manual calculation
#####################################################

(diff<-control-clipped)
(tStat<-mean(diff)/(sd(diff)/sqrt(length(diff))))
(pval<-2*(pt(tStat,df=length(diff)-1)))

#####################################################

wilcox.test(control,clipped,paired=TRUE,conf.int=TRUE)


#3.4	Correlating two variables

disp<-read.csv('dispersal.csv',header=TRUE)
disp<-na.omit(disp)
plot(disp$disp.rate.ftb,disp$disp.rate.eb,pch=19)
cor.test(disp$disp.rate.ftb,disp$disp.rate.eb,use='complete.obs')

#####################################################
#supplemental - manual calculation
#####################################################

(rval<-cor(disp$disp.rate.ftb,disp$disp.rate.eb,use='complete.obs'))
(tstat<-rval*sqrt((nrow(disp)-2)/(1-rval^2)))
(pval<-2*(1-pt(tstat,df=nrow(disp)-2)))

#####################################################

plot(rank(disp$disp.rate.ftb),rank(disp$disp.rate.eb),pch=19)
cor.test(disp$disp.rate.ftb,disp$disp.rate.eb,use='complete.obs',
         method='spearman')


#3.5 Comparing two distributions

plot(ecdf(disp$disp.rate.ftb),verticals=TRUE)
plot(ecdf(disp$disp.rate.eb),verticals=TRUE,lty=3,add=TRUE)
ks.test(disp$disp.rate.ftb,disp$disp.rate.eb)

#####################################################
#supplemental - one sample comparison to theoretical
#####################################################

#compare to normal distribution
plot(ecdf(disp$disp.rate.ftb),verticals=TRUE)
lines(sort(disp$disp.rate.ftb),pnorm(sort(disp$disp.rate.ftb),
                                     mean=mean(disp$disp.rate.ftb),
                                     sd=sd(disp$disp.rate.ftb)),type='b',lty=2,add=TRUE)
ks.test(disp$disp.rate.ftb,'pnorm',mean=mean(disp$disp.rate.ftb),
        sd=sd(disp$disp.rate.ftb))

#compare to beta distribution
library(fitdistrplus)
(fitBeta<-fitdist(disp$disp.rate.ftb+0.01,dbeta)) #fix problem with zeros
(fitBeta<-fitdist(disp$disp.rate.ftb,dbeta))
plot(ecdf(disp$disp.rate.ftb+0.01),verticals=TRUE)
lines(sort(disp$disp.rate.ftb+0.01),pbeta(sort(disp$disp.rate.ftb+0.01),
                                          shape1=coef(fitBeta)[1],shape2=coef(fitBeta)[2]),
      type='b',lty=2,add=TRUE)
ks.test(disp$disp.rate.ftb+0.01,'pbeta',shape1=coef(fitBeta)[1],
        shape2=coef(fitBeta)[2])


##################################################
#supplemental - KS versus correlation
##################################################

#two samples identical distributions - no correlation
t1<-rnorm(1000,0,1)
t2<-rnorm(1000,0,1)
hist(t1)
hist(t2)
plot(density(t1),lwd=2)
lines(density(t2),lty=2,lwd=2,col='red')
plot(ecdf(t1),lwd=2)
plot(ecdf(t2),lwd=2,col='red',add=TRUE)
ks.test(t1,t2,exact=TRUE)
plot(t1,t2)
cor.test(t1,t2)

#two samples different distributions - no correlation
t1<-rnorm(1000,0,1)
t2<-rnorm(1000,0,2)
plot(ecdf(t1),lwd=2)
plot(ecdf(t2),lwd=2,col='red',add=TRUE)
ks.test(t1,t2,exact=TRUE)
plot(t1,t2)
cor.test(t1,t2)

#two samples identical ditstributions - perfect correlation
t1<-rnorm(1000,0,1)
t2<-t1
plot(ecdf(t1),lwd=3)
plot(ecdf(t2),lwd=2,col='red',add=TRUE)
ks.test(t1,t2,exact=TRUE)
plot(t1,t2)
cor.test(t1,t2)

#two samples different ditstributions - pefect rank correlation
t1<-rgamma(1000,shape=1,scale=1)
t2<-log(t1)
plot(ecdf(t1),lwd=2,xlim=c(min(t2),max(t1)))
plot(ecdf(t2),lwd=2,col='red',add=TRUE)
ks.test(t1,t2,exact=TRUE)
plot(t1,t2)
cor.test(t1,t2)
cor.test(t1,t2,method='spearman')

########################################################


#3.6 Comparing two (or more) proportions

prop.test(c(4,16),c(40,250),correct=FALSE)


##################################################
#supplemental - manual calculation
##################################################

(obs<-matrix(c(4,16,36,234),nrow=2))
(exp<-chisq.test(obs,correct=FALSE)$expected)
(x2<-sum((obs-exp)^2/exp))
1-pchisq(x2,df=1)

##################################################
#supplemental - sample size effects
##################################################

prop.test(c(4,16),c(40,250),correct=FALSE) #note success vs trials
(obs<-matrix(c(4,16,36,234),nrow=2)) #note success vs failures
chisq.test(obs,correct=FALSE)
(obs<-matrix(c(40,160,360,2340),nrow=2)) #note success vs failures
chisq.test(obs,correct=FALSE)

##################################################


#3.7 Testing for independence of two variables in a contingency table

#without continuity correction
(owls<-matrix(c(16,9,4,11),nrow=2))
(out<-chisq.test(owls,correct=FALSE))

##################################################
#supplemental - manual calculation
##################################################

(exp<-out$expected)
(x2<-sum((owls-exp)^2/exp))
1-pchisq(x2,df=1)

##################################################

#with continuity correction
(out<-chisq.test(owls,correct=TRUE))

##################################################
#supplemental - manual calculation
##################################################

#demonstrate manual calculation of chi-square test with continuity correction
(exp<-out$expected)
(x2<-sum((abs(owls-exp)-.5)^2/exp))
1-pchisq(x2,df=1)

##################################################

#fisher's exact test
fisher.test(owls)

#calculation of odds ratio -- differs from MLE from fisher.test
(owls[1,1]*owls[2,2])/(owls[1,2]*owls[2,1])


###4. Exercise

#4.1 Difference between tree seedling counts after treatment
veg<-read.csv('vegdata.csv',header=TRUE)
clipped<-veg[veg$treatment=='clipped',]
clipped

clipped.long<-reshape(clipped,varying=list(5:6),v.names='count',
  timevar='species',times=c('birch','pine'),direction='long')
clipped.long$species<-as.factor(clipped.long$species)


#4.2 Correlation between invasive worm counts and midden counts
worms<-read.csv('worms.csv',header=TRUE)
worms
hist(worms$middens.count)
hist(worms$worms.count)
sunflowerplot(worms$middens.count,worms$worms.count)

#4.3 Brown creeper preference/avoidance of forest edges
birds<-read.csv('bird.sta.csv',header=TRUE)
hab<-read.csv('hab.sta.csv',header=TRUE)
birdhab<-merge(birds,hab,by=c('basin','sub','sta'))
bin<-data.trans(birdhab,method='power',var='AMCR:YRWA',exp=0,plot=FALSE)
temp1<-table(bin$s.edge,bin$BRCR)
temp2<-temp1[,c(2,1)]
temp2



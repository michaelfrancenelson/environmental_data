#    R code for: Chapter 8 in:
#    Analysing Ecological Data. (2007). Zuur, Ieno and Smith. Springer, 680p.
#    This file was produced by Nelly Chizhikova and Alain Zuur
#    (highstat@highstat.com)

#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful, 
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.




setwd("D:\\applicat\\HighlandStatistics\\Book\\R\\RChapter8\\")

par(mar = c(4.5, 4.5, 0.5, 0.5), cex.lab = 1.3, cex.axis = 1.3)


#      Figure 8.1 A. One regression line
RIKZ <- read.table(file = "RIKZ.txt", header = TRUE)
RIKZ$Richness <- rowSums(RIKZ[, 2:76] > 0)

par(mfrow=c(2,2),mar=c(4,4,2,1)+0.1)
tmp <- lm(Richness ~ NAP, data = RIKZ)
plot(RIKZ$NAP, RIKZ$Richness, xlab="NAP", ylab="Richness", type="n")
points(RIKZ$NAP,RIKZ$Richness,cex=1)
abline(tmp)
text(2,20,"A",cex=1.2)


tmp1<-lm(Richness~NAP*factor(Beach), data = RIKZ)
plot(RIKZ$NAP,RIKZ$Richness,xlab="NAP",ylab="Richness",type="n")
points(RIKZ$NAP,RIKZ$Richness,cex=1)
for (i in 1:9){
    J<-RIKZ$Beach==i
    x1<-RIKZ$NAP[J]
    y1<-tmp1$fitted[J]
    Ord<-order(x1)
    lines(x1[Ord],y1[Ord])
}
text(2,20,"B",cex=1.2)


tmp2<-lm(Richness~NAP+factor(Beach), data = RIKZ)
plot(RIKZ$NAP,RIKZ$Richness,xlab="NAP",ylab="Richness",type="n")
points(RIKZ$NAP,RIKZ$Richness,cex=1)
for (i in 1:9){
    J<-RIKZ$Beach==i
    x1<-RIKZ$NAP[J]
    y1<-tmp2$fitted[J]
    Ord<-order(x1)
    lines(x1[Ord],y1[Ord])
}
text(2,20,"C",cex=1.2)


tmp3<-lm(Richness~NAP+factor(Beach):NAP, data = RIKZ)
plot(RIKZ$NAP,RIKZ$Richness,xlab="NAP",ylab="Richness",type="n")
points(RIKZ$NAP,RIKZ$Richness,cex=1)
for (i in 1:9){
    J<-RIKZ$Beach==i
    x1<-RIKZ$NAP[J]
    y1<-tmp3$fitted[J]
    Ord<-order(x1)
    lines(x1[Ord],y1[Ord])
}
text(2,20,"D",cex=1.2)




#Output for model 5 with random intercept, page 128-129
RIKZ <- read.table(file = "RIKZ.txt", header = TRUE)
RIKZ$Richness <- rowSums(RIKZ[, 2:76] > 0)
library(nlme)
m5 <- lme(Richness ~ NAP, data = RIKZ,
                  random = ~ 1|Beach)
summary(m5)
anova(m5)



#      Output for model 6 with random slope and intercept, page 129
RIKZ <- read.table(file = "RIKZ.txt", header = TRUE)
RIKZ$Richness <- rowSums(RIKZ[, 2:76] > 0)
library(nlme)
m6<-lme(Richness~NAP ,random= ~ NAP | factor(Beach), data = RIKZ, method="REML")
summary(m6)
anova(m6)
#I am not sure why these results are slightly different from those in the book
#It could be due to different R versions (1.6 in the book, and 2.7 now)
plot(m6)



#      Model selection (comparing models 5 and 6), page 130
anova(m5, m6)



#Model 7, page 131
RIKZ <- read.table(file = "RIKZ.txt", header = TRUE)
RIKZ$Richness <- rowSums(RIKZ[, 2:76] > 0)
RIKZ$fBeach <- factor(RIKZ$Beach)
library(nlme)
lmc <- lmeControl(niterEM = 5000, msMaxIter = 1000)
m7 <- lme(Richness ~ NAP, data = RIKZ,
                  random = ~ 1+NAP|fBeach,
                  weights = varIdent(form = ~ 1|fBeach), method="REML",
                  control = lmc)
summary(m7)

#In the current version of R (2.7), this model does not convergence (in the
#older version it did). Odd.
#I decided to remove the random slope from model 7.

m7NEW <- lme(Richness ~ NAP, data = RIKZ,
                  random = ~ 1|fBeach,
                  weights = varIdent(form = ~ 1|fBeach), method="REML")
summary(m7NEW)


#comparing the models 6 and 7NEW, page 132

anova(m6, m7NEW)

#      Model df      AIC      BIC     logLik   Test  L.Ratio p-value
#m6        1  6 244.3839 254.9511 -116.19193
#m7NEW     2 12 212.1544 233.2888  -94.07719 1 vs 2 44.22947  <.0001



#Figure 8.2
par(mfrow=c(2,2),mar=c(5,4,1,1)+0.1)
plot(m6$fitted[,2],resid(m6,type="p"),xlab="Fitted values",ylab="Residuals")
qqnorm(resid(m6,type="p"),cex=1,main="")

plot(m7NEW$fitted[,2],resid(m7,type="p"),xlab="Fitted values",ylab="Residuals")
qqnorm(resid(m7NEW,type="p"),cex=1,main="")




#      Model 8, page 133
#NOTE THAT DUE TO CONVERGENCE PROBLEMS, I REMOVED THE RANDOM SLOPES
RIKZ <- read.table(file = "RIKZ.txt", header = TRUE)
RIKZ$Richness <- rowSums(RIKZ[, 2:76] > 0)
RIKZ$fBeach <- factor(RIKZ$Beach)
library(nlme)

lmc <- lmeControl(niterEM = 5000, msMaxIter = 1000)
m8NEW <- lme(Richness ~ NAP+factor(exposure), data = RIKZ,
                  random = ~ 1|fBeach,
                  weights = varIdent(form = ~ 1|fBeach),
                  method="ML",control = lmc)

#comparison with model 7
m7NEW <- lme(Richness ~ NAP, data = RIKZ,
                  random = ~ 1|fBeach,
                  weights = varIdent(form = ~ 1|fBeach),
                  method="ML")
anova(m7NEW, m8NEW)


################
# Section 8.5
# A full analysis and all R code for the bee data can be found
# in Chapter 22





#Section 8.6
RIKZ <- read.table(file = "RIKZ.txt", header = TRUE)
RIKZ$Richness <- rowSums(RIKZ[, 2:76] > 0)
RIKZ$fBeach <- factor(RIKZ$Beach)
library(nlme)
library(mgcv)

lmc <- lmeControl(niterEM = 5000, msMaxIter = 1000)
M10 <- gamm(Richness ~ s(NAP),random=list(fBeach=~1),
                  data = RIKZ)

M11 <- gamm(Richness ~ s(NAP),random=list(fBeach=~1),
                  weights = varIdent(form = ~ 1|fBeach),
                  data = RIKZ)

AIC(M10$lme,M11$lme)
#Note the small differences due to the different R version
plot(M11$gam)
summary(M11$lme)



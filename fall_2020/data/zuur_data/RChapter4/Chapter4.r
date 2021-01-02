#    R code for: Chapter 4 in:
#    Analysing Ecological Data. (2007). Zuur, Ieno and Smith. Springer, 680p.
#    This file was produced by Alain Zuur (highstat@highstat.com)
#    www.highstat.com

#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.


setwd("D:\\applicat\\HighlandStatistics\\Book\\R\\RChapter4\\")

#Figure 4.2
Argentina<-read.table(file="Argentina.txt",header=TRUE)
names(Argentina)
boxplot(Argentina$L.acuta, xlab="L. acuta")

#Figure 4.3
Argentina<-read.table(file="Argentina.txt",header=TRUE)
op<-par(mfrow=c(2,2), mar=c(4,3,3,2))
boxplot(Argentina$L.acuta,
        Argentina$H.similis,
        Argentina$U.uruguayensis,
        Argentina$N.succinea,
        names=c("La","Hs","Uu","Ns"))

Argentina$L.acuta.SQ <- sqrt(Argentina$L.acuta)
Argentina$H.similis.SQ <- sqrt(Argentina$H.similis)
Argentina$U.uruguayensis.SQ <- sqrt(Argentina$U.uruguayensis)
Argentina$N.succinea.SQ <- sqrt(Argentina$N.succinea)

boxplot(Argentina$L.acuta.SQ,
        Argentina$H.similis.SQ,
        Argentina$U.uruguayensis.SQ,
        Argentina$N.succinea.SQ,
        names=c("La","Hs","Uu","Ns"))

Argentina$fTransect <- factor(Argentina$Transect)
Argentina$fSeason <- factor(Argentina$Season)
boxplot(L.acuta.SQ ~ fTransect,
        names=c("a","b","c"),
        data=Argentina)

boxplot(L.acuta.SQ ~ fTransect * fSeason,
        names=c("1a","2a","1b","2b","1c","2c"),
        data=Argentina)

par(op)



#Figure 4.4
Argentina<-read.table(file="Argentina.txt",header=TRUE)
op<-par(mfrow=c(2,2), mar=c(4,3,3,2))
dotchart(Argentina$L.acuta, main = "L. acuta", pch=Argentina$Transect)
dotchart(Argentina$H.similis, main = "H. similis", pch=Argentina$Transect)
dotchart(Argentina$U.uruguayensis, main = "U. uruguayensis", pch=Argentina$Transect)
dotchart(Argentina$N.succinea, main = "N. succinea", pch=Argentina$Transect)
par(op)



#Figure 4.5
Squid<-read.table(file="Squid.txt",header=TRUE)
names(Squid)
hist(Squid$GSI)
hist(Squid$GSI[Squid$Sex==1], main="Males",xlab="")
hist(Squid$GSI[Squid$Sex==2], main="Females",xlab="")


#Figure 4.6
Argentina$L.acuta.SQ <- sqrt(Argentina$L.acuta)
Argentina$L.acuta.SQ4 <- Argentina$L.acuta^(0.25)
Argentina$L.acuta.Log <- log(Argentina$L.acuta+1)

op<-par(mfrow=c(2,2), mar=c(4,3,3,2))
qqnorm(Argentina$L.acuta,main="None (p=1)")
qqline(Argentina$L.acuta)
qqnorm(Argentina$L.acuta.SQ,main="Square root (p=0.5)")
qqline(Argentina$L.acuta.SQ)
qqnorm(Argentina$L.acuta.SQ4,main="Fourth root (p=0.25)")
qqline(Argentina$L.acuta.SQ4)
qqnorm(Argentina$L.acuta.Log,main="Logarirthmic (p=0)")
qqline(Argentina$L.acuta.Log)
par(op)


#Figure 4.7
Clams <- read.table(file="WedgeclamII.txt",header=TRUE)
names(Clams)
Clams$LogLENGTH<-log(Clams$LENGTH)
Clams$LogAFD<-log(Clams$AFD)
plot(x=Clams$LogLENGTH,y=Clams$LogAFD,xlab="Length",ylab="Biomass")
M1 <- lm(LogAFD~LogLENGTH, data = Clams)
abline(M1)


#Figure 4.8
Decapod <- read.table(file="DecapodNew.txt",header=TRUE)
names(Decapod)
Species <- Decapod[,2:13]
Richness <- rowSums(Species > 0)
Z<-cbind(Richness,Decapod[,c(14,16,17,18)])
pairs(Z)
#Use the labels option in the pairs funciton to get the
#same names as in Figure 4.8


#Figure 4.9
Decapod <- read.table(file="DecapodNew.txt",header=TRUE)
source("MyLibrary.R")
Decapod$ChoTransformed <- sqrt(Decapod$Ch0.10m)
Z <- cbind(Decapod$T1m,Decapod$T45.35m,
           Decapod$S1m,Decapod$S45.35m,
           Decapod$ChoTransformed)
pairs(Z,lower.panel=panel.cor,
       labels = c("T1m","T45.35m","S1m","S45.35m","ChloTrans"))
#Note: The panel.cor function in our library
#file MyLibrary.R uses only one digit for the correlation.
#In the book, we used two digits. You can easily
#change this.


#Figure 4.10
RIKZ <- read.table(file="RIKZ.txt",header=TRUE)
names(RIKZ)
RIKZ$Richness <- rowSums(RIKZ[,2:76] > 0)
RIKZ$fweek <- factor(RIKZ$week)
coplot(Richness~NAP | fweek, data = RIKZ)


#Figure 4.11
RIKZ <- read.table(file="RIKZ.txt",header=TRUE)
coplot(Richness~NAP | temperature, data = RIKZ,
       panel = panel.smooth)


#Figure 4.12
Squid<-read.table(file="Squid.txt",header=TRUE)
names(Squid)
library(lattice)
xyplot(GSI~MONTH | factor(Location), data = Squid, col = 1)


#Figure 4.13
ClamsIeno <- read.table(file="ClamsIeno.txt",header=TRUE)
names(ClamsIeno)
ClamsIeno$fBeach <- factor(ClamsIeno$Beach)
ClamsIeno$fMonth <- factor(ClamsIeno$Month)
ClamsIeno$fLevel <- factor(ClamsIeno$Level)
library(Design)
plot.design(Abundance ~ fBeach + fLevel + fMonth,
            data = ClamsIeno)


#Figure 4.14
#Note: This is a different clams data sets as above
ClamsIeno <- read.table(file="ClamsIeno.txt",header=TRUE)
names(ClamsIeno)
ClamsIeno$fBeach <- factor(ClamsIeno$Beach)
ClamsIeno$fMonth <- factor(ClamsIeno$Month)
ClamsIeno$fLevel <- factor(ClamsIeno$Level)
library(Design)
interaction.plot(ClamsIeno$fMonth,ClamsIeno$fBeach,
                 ClamsIeno$Abundance, xlab="Month",
                 ylab="")

win.graph()
interaction.plot(ClamsIeno$fMonth,ClamsIeno$fLevel,
                 ClamsIeno$Abundance, xlab="Month",
                 ylab="")
#The data argument does not seem to work here. You could define
#fLevel and fBeach outside the ClamsIeno object to get rid
#of the funny labeling in the legend.


#Figure 4.15
RIKZ <- read.table(file="RIKZ.txt",header=TRUE)
names(RIKZ)
RIKZ$Richness <- rowSums(RIKZ[,2:76] > 0)
plot(x = RIKZ$NAP, y=RIKZ$Richness, xlab="NAP",
     ylab="Richness", main = "Scatterplot")
M1.RIKZ <- lm(Richness ~ NAP, data = RIKZ)
abline(M1.RIKZ)


#Figure 4.18
#These are the wedgeclams again
Clams <- read.table(file="WedgeclamII.txt",header=TRUE)
names(Clams)
Clams$LogLENGTH<-log(Clams$LENGTH)
Clams$LogAFD<-log(Clams$AFD)

op<-par(mfrow=c(2,2), mar=c(5,4,4,2))
plot(x=Clams$LENGTH,y=Clams$AFD,xlab="Length",ylab="Biomass")
plot(x=Clams$LENGTH^2,y=Clams$AFD,xlab="Length^2",ylab="Biomass")
plot(x=Clams$LENGTH,y=Clams$LogAFD,xlab="Length",ylab="log(Biomass)")
plot(x=Clams$LogLENGTH,y=Clams$LogAFD,xlab="Length",ylab="Biomass")
par(op)


#Figure 4.19
#The code below seems to be correct, but it gives a slightly
#different graph as in the book. I guess I forgot to remove
#the outlier in the data, when we made the graph for the book.
#The data set below does not contain the outlier.
Clams <- read.table(file="WedgeclamII.txt",header=TRUE)
n<-length(Clams$AFD)
Ydot<-exp(sum(log(Clams$AFD),na.rm=T)/n)

myboxcox2<-function(Y1,X2,p,Ydot){
 if (p != 0) { Y1.t <- (-1+Y1^(p)) / (p*Ydot^(p-1)) }
 if (p == 0) { Y1.t <- Ydot * log(Y1) }
 tmp<-lm(Y1.t~X2)
 sum(tmp$residuals^2)
}


a1=-0.1
a2=0.5
E<-vector(length=length(seq(a1,a2,0.1)))
II<-seq(a1,a2,0.01)
k<-1
for (j in II){
    print(j)
    E[k]<-myboxcox2(Clams$AFD,Clams$LENGTH,j,Ydot)
    k<-k+1
}

plot(II,E,type="l",xlab="Power transformation p", ylab="SS(p)")

#Confidence bands
SS.E=min(E)
v=n-2
t=1.96
SS.star=SS.E*(1+(t)/v)
abline(SS.star,0,lty=2)


#Figure 4.20
SSTNA<-read.table(file="SSTNorthAmerica.txt",header=TRUE)
names(SSTNA)
SST1<-c(SSTNA$Observed_22_24,SSTNA$Observed_30_32,
       SSTNA$Observed_38_40,SSTNA$Observed_46_48)
Time<-rep(SSTNA$AxisYear,4)
n <- length(SSTNA$Observed_22_24)
ID <- rep(c("SST_22_24","SST_30_32",
            "SST_38_40","SST_46_48"), each = n)

library(lattice)
xyplot(SST1~Time|ID,type="l",col=1)


#Centre the data:
SST2<-c(scale(SSTNA$Observed_22_24,scale=FALSE),
        scale(SSTNA$Observed_30_32,scale=FALSE),
        scale(SSTNA$Observed_38_40,scale=FALSE),
        scale(SSTNA$Observed_46_48,scale=FALSE))

xyplot(SST2~Time|ID,type="l",col=1)


#Normalise the data:
SST3<-c(scale(SSTNA$Observed_22_24,scale=TRUE),
        scale(SSTNA$Observed_30_32,scale=TRUE),
        scale(SSTNA$Observed_38_40,scale=TRUE),
        scale(SSTNA$Observed_46_48,scale=TRUE))

xyplot(SST3~Time|ID,type="l",col=1)



#Figure 4.21
Squid<-read.table(file="Squid.txt",header=TRUE)
names(Squid)
plot(x = Squid$MONTH, y = Squid$GSI, xlab="Month", ylab = "GSI")
Squid$fLocation <- factor(Squid$Location)
Squid$fSex <- factor(Squid$Sex)
coplot(GSI~MONTH | fLocation * fSex, data = Squid)







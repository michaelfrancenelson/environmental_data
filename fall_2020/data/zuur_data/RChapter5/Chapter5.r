#    R code for: Chapter 5 in:
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

#    This file was produced by Nelly chizhikova and Alain Zuur 
#    (highstat@highstat.com)


setwd("D:\\applicat\\HighlandStatistics\\Book\\R\\RChapter5\\")

#Read RIKZ data from a file
RIKZ <- read.table(file = "RIKZ.txt",header = TRUE)
RIKZ$Richness <- rowSums(RIKZ[,2:76] > 0)



#     Figure 5.1 A
RIKZ <- read.table(file = "RIKZ.txt",header = TRUE)
RIKZ$Richness <- rowSums(RIKZ[,2:76] > 0)
par(mar = c(4.5,4.5,0.5,0.5), cex.lab = 1.5, cex.axis = 1.5)
plot(RIKZ$NAP,RIKZ$Richness, ylab = "Richness", xlab = "NAP")



#     Figure 5.1 B
RIKZ <- read.table(file = "RIKZ.txt",header = TRUE)
RIKZ$Richness <- rowSums(RIKZ[,2:76] > 0)
par(mar = c(4.5,4.5,0.5,0.5), cex.lab = 1.5, cex.axis = 1.5)
plot(RIKZ$NAP,RIKZ$Richness, ylab = "Richness", xlab = "NAP")
RIKZ_model.1<-lm(Richness ~ NAP, data = RIKZ)
abline(RIKZ_model.1)



#    Table 5.3 
RIKZ <- read.table(file = "RIKZ.txt",header = TRUE)
RIKZ$Richness <- rowSums(RIKZ[,2:76] > 0)
RIKZ_model.1<-lm(Richness ~ NAP, data = RIKZ)
anova(RIKZ_model.1)



#    Table 5.4 
RIKZ <- read.table(file = "RIKZ.txt",header = TRUE)
RIKZ$Richness <- rowSums(RIKZ[,2:76] > 0)
RIKZ_model.1<-lm(Richness ~ NAP, data = RIKZ)
summary(RIKZ_model.1)$coefficients


#Figures 5.6 and 5.7 were created in Matlab. However,
#in our book: "Mixed effects models and extensions with R", by
#Zuur, Ieno, Walker, Saveliev and SMith (2009) we
#use a similar graph for a Poisson GLM. The code
#is given below and you can easily adapt it for a
#linear regression, GLM or GAM:

library(scatterplot3d)
x <- seq(0, 100)
y <- exp(0.01+0.03*x)      #<-change this for a linear regression or GAM. Just drop the exp
y                           # For a GAM, use perhaps something like 0.01 + 0.03*x + 0.05*x^2
z <- 0*x                    #try different values of 0.01, 0.03, 0.05 and choose something that looks non-linear

ymeas=rpois(length(y),lambda=y)    #<- change this for a linear regression

plot(x,ymeas,type="p",xlab="Covariate",ylab="Observed values")
lines(x,y)

rr=scatterplot3d(x, y, z, highlight.3d=TRUE, col.axis="black",
      col.grid="black", pch=20,zlim=c(0,0.4),type="l",lwd=3,
      xlab="Covariate",ylab="Possible values",zlab="Probability")

MyX=c(2,15,30,50,75)
for (i in 1:5){
  xi=MyX[i]
  yi=exp(0.01+0.03*xi)
  #if ( xi > 10) {yseq=round(seq(yi-0.8*yi,yi+0.8*yi,step=0.01))}
  if (xi <=100) {yseq=round(seq(0,20,by=0.1))}
  zi=dpois(yseq, lambda=yi)           #<- Change this for a linear regression
  rb=cbind(xi,yseq,zi)
  rr$points3d(rb, col = 1,type="h",pch=30)
  }





#    Figure 5.8 
par(mar = c(4.5,4.5,0.5,0.5), cex.lab = 1.5, cex.axis = 1.5, mfrow = c(2,2))
ff <- y  ~  x
for (i in 1:4) {
    ff[2:3] <- lapply(paste(c("y","x"), i, sep = ""), as.name)

     plot(ff, data  = anscombe, xlim = c(3,19), ylim = c(3,13), 
          xlab = "x", ylab = "y", cex = 1.5)

     #Fit linear model to the data
     assign(paste("ans_model.",i,sep = ""), 
	        lmi <- lm(ff, data =  anscombe))

     #add regression line to the plot
     abline(get(paste("ans_model.",i,sep = "")))
}
par(mfrow = c(1,1))



#     Estimated regression parameters for Anscombe data
lapply(objects(pat = "ans_model.\[1-4]$"), 
       function(n) summary(get(n))$coefficients)

	   
	   
#     R^2 values for Anscombe data models
lapply(objects(pat = "ans_model.\[1-4]$"), 
       function(n) summary(get(n))$r.squared)


	   
#     Figure 5.9 
RIKZ <- read.table(file = "RIKZ.txt",header = TRUE)
RIKZ$Richness <- rowSums(RIKZ[,2:76] > 0)
par(mar = c(5,5,1.5,0.5), cex.lab = 1.5, cex.axis = 1.5, mfrow = c(2,2))
RIKZ_model.1<-lm(Richness ~ NAP, data = RIKZ)
plot(RIKZ_model.1, which = c(1:4), add.smooth = F, cex.id = 1)



#     Figure 5.10 
RIKZ <- read.table(file = "RIKZ.txt",header = TRUE)
RIKZ$Richness <- rowSums(RIKZ[,2:76] > 0)
RIKZ_model.1<-lm(Richness ~ NAP, data = RIKZ)
par(mar = c(5,5,0.5,0.5), cex.lab = 1.5, cex.axis = 1.5, mfrow = c(2,1))
plot(lm.influence(RIKZ_model.1)$coefficients[,1], 
     xlab = "Index of omitted observation", 
	 ylab = "Intercept")
plot(lm.influence(RIKZ_model.1)$coefficients[,2], 
     xlab = "Index of omitted observation", 
	 ylab = "NAP slope")
par(mfrow = c(1,1))



#     Figure 5.11
RIKZ <- read.table(file = "RIKZ.txt",header = TRUE)
RIKZ$Richness <- rowSums(RIKZ[,2:76] > 0)
RIKZ_model.1<-lm(Richness ~ NAP, data = RIKZ)
library(MASS)
par(mar = c(5,5,2,0.5), cex.lab = 1.5, cex.axis = 1.5)
plot(predict(RIKZ_model.1),stdres(RIKZ_model.1), 
     xlab = "Fitted values", ylab = "Residuals", 
	 main = "Standardized residuals")
plot(predict(RIKZ_model.1),studres(RIKZ_model.1), 
     xlab = "Fitted values", ylab = "Residuals", 
	 main = "Studentised residuals")


	 
#     Figure 5.12 
RIKZ <- read.table(file = "RIKZ.txt",header = TRUE)
RIKZ$Richness <- rowSums(RIKZ[,2:76] > 0)
RIKZ_model.1<-lm(Richness ~ NAP, data = RIKZ)
par(mar = c(4.5,4.5,0.5,0.5), cex.lab = 1.5, cex.axis = 1.5)
plot(lm.influence(RIKZ_model.1)$hat, 
     xlab = "Sample index", ylab = "Hat values", cex = 1.2)



#     Table 5.6 
RIKZ <- read.table(file = "RIKZ.txt",header = TRUE)
RIKZ$Richness <- rowSums(RIKZ[,2:76] > 0)
RIKZ_model.2<-lm(Richness ~ angle2+NAP+grainsize+humus+factor(week), 
                 data = RIKZ)
summary(RIKZ_model.2)$coefficients
summary(RIKZ_model.2)


#     Table, page 70
RIKZ <- read.table(file = "RIKZ.txt",header = TRUE)
RIKZ$Richness <- rowSums(RIKZ[,2:76] > 0)
RIKZ_model.2<-lm(Richness ~ angle2+NAP+grainsize+humus+factor(week), 
                 data = RIKZ)
anova(RIKZ_model.2)



#     Model selection, pages 71-72
#     Backward selection
RIKZ <- read.table(file = "RIKZ.txt",header = TRUE)
RIKZ$Richness <- rowSums(RIKZ[,2:76] > 0)
RIKZ_model.2<-lm(Richness ~ angle2+NAP+grainsize+humus+factor(week), 
                 data = RIKZ)
step(RIKZ_model.2, direction = "backward")
#     One variable is dropped in turn
drop1(RIKZ_model.2, test = "F")





#     Partial linear regression
#     partialling the effect of factor(Transect) out 
Argentina<-read.table(file = "Argentina.txt",header = TRUE)
library(vegan)
par(mar = c(4.5,4.5,0.5,0.5), cex.lab = 1.5, cex.axis = 1.5)
H<-diversity(Argentina[,2:5], index = "shannon")

#In the book, we actually used the Shannon Index with log10
#as base
Mydiversity<-function (x, index = "shannon", MARGIN = 1, base = exp(1))
{
    x <- as.matrix(x)
    INDICES <- c("shannon", "simpson", "invsimpson")
    index <- match.arg(index, INDICES)
    total <- apply(x, MARGIN, sum)
    x <- sweep(x, MARGIN, total, "/")
    if (index == "shannon")
        x <- -x * log10(x)
    else x <- x^2
    H <- apply(x, MARGIN, sum, na.rm = TRUE)
    if (index == "simpson")
        H <- 1 - H
    else if (index == "invsimpson")
        H <- 1/H
    return(H)
}


H<-diversity(Argentina[,2:5], index = "shannon")

H_model<-lm(H ~ factor(Transect), data = Argentina)
Mud_model<-lm(Mud ~ factor(Transect), data = Argentina)
partial_lm<-lm(residuals(H_model) ~ residuals(Mud_model))

#NAP slope and it's significance
summary(partial_lm)$coefficients

#     Figure 5.13
plot(residuals(Mud_model), residuals(H_model),
     xlab = "Mud", ylab = "Shannon-Weaver index")
abline(partial_lm)


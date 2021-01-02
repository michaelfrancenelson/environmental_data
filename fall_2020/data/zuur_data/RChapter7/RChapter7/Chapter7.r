#    R code for: Chapter 7 in:
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

#    This file was produced by Nelly Chizhikova and Alain Zuur
#    (highstat@highstat.com)



setwd("D:\\applicat\\HighlandStatistics\\Book\\R\\RChapter7\\")


par(mar = c(4.5, 4.5, 0.5, 0.5), cex.lab = 1.2, cex.axis = 1.2)


# Figure 7.1
RIKZ <- read.table(file = "RIKZ.txt", header = TRUE)
RIKZ$Richness <- rowSums(RIKZ[, 2:76] > 0)
plot(RIKZ$grainsize, RIKZ$Richness,
     ylab = "Richness", xlab = "Grainsize")




# Figure 7.3
#This graph is slithgly different compared to the one in
#the book. Not sure if the one in the book is actually correct.
RIKZ <- read.table(file = "RIKZ.txt", header = TRUE)
RIKZ$Richness <- rowSums(RIKZ[, 2:76] > 0)
par(mfrow = c(2, 2))
#A
plot(RIKZ$grainsize, RIKZ$Richness,
     ylab = "Richness", xlab = "Grainsize",
     main = "LOESS with span = 0.75")
M1 <- loess(RIKZ$Richness ~ RIKZ$grainsize, span = 0.75)
F1<-fitted(M1)
I1<-order(RIKZ$grainsize)
lines(RIKZ$grainsize[I1],F1[I1],lwd = 2)

#B
plot(RIKZ$grainsize, RIKZ$Richness,
     ylab = "Richness", xlab = "Grainsize",
     main = "LOESS with span = 0.3")
M2 <- loess(RIKZ$Richness ~ RIKZ$grainsize, span = 0.3)
F2<-fitted(M2)
I2<-order(RIKZ$grainsize)
lines(RIKZ$grainsize[I2],F2[I2],lwd = 2)

#C
plot(RIKZ$grainsize, RIKZ$Richness,
     ylab = "Richness", xlab = "Grainsize",
     main = "LOESS with span = 0.5")
M3 <- loess(RIKZ$Richness ~ RIKZ$grainsize, span = 0.5)
F3 <- fitted(M3)
I3 <- order(RIKZ$grainsize)
lines(RIKZ$grainsize[I3], F3[I3], lwd = 2)

#D
plot(RIKZ$grainsize, RIKZ$Richness,
      ylab = "Richness", xlab = "Grainsize",
      main = "Default smoothing spline")
library(mgcv)
M4 <- gam(Richness ~ s(grainsize, fx=TRUE, k=5), data = RIKZ)
F4 <- fitted(M4)
I4 <- order(RIKZ$grainsize)
lines(RIKZ$grainsize[I4], F4[I4], lwd = 2)

par(mfrow = c(1, 1))



#Figure 7.5
Bahama <- read.table("Bahama.txt", header = T)
Bahama2 <- Bahama[Bahama$Method == 2, ]
library(mgcv)
plot(Bahama2$CoralRichness, Bahama2$Parrot,
     xlab = "CoralRichness", ylab = "Parrot fish")
     
Parrot_admod <- gam(Parrot ~ s(CoralRichness, k = 5, fx = T),
                    data = Bahama2,
                    family = gaussian)
plot(Parrot_admod)

#Numerical output for the Parrot model, page 103
summary(Parrot_admod)




#Figure 7.6 A
Bahama <- read.table("Bahama.txt", header = T)
Bahama2 <- Bahama[Bahama$Method == 2, ]
plot(Bahama2$CoralRichness, Bahama2$Parrot,
     xlab = "CoralRichness", ylab = "Parrot fish")
Parrot_poly <- glm(Parrot ~ poly(CoralRichness, 3),
                   data = Bahama2,
                   family = gaussian)
                   
#Do the prediction
new_CoralRichness <- seq(0, 15, 1)
MyData <- data.frame(CoralRichness = new_CoralRichness)
MyPred <- predict(Parrot_poly, newdata = MyData)
lines(new_CoralRichness,MyPred)



#Figure 7.6B
plot(Bahama2$CoralRichness, Bahama2$Parrot,
     xlab = "CoralRichness", ylab = "Parrot fish")
Parrot_poly <- glm(Parrot ~ poly(CoralRichness, 3),
                   data = Bahama2,
                   family = gaussian,
                   subset = (Bahama2$CoralRichness < 8.5))

#Do the prediction
new_CoralRichness <- seq(0, 8, 1)
MyData <- data.frame(CoralRichness = new_CoralRichness)
MyPred <- predict(Parrot_poly, newdata = MyData)
lines(new_CoralRichness,MyPred)

Parrot_poly <- glm(Parrot ~ poly(CoralRichness, 3),
                   data = Bahama2,
                   family = gaussian,
                   subset = (Bahama2$CoralRichness > 8.5))

#Do the prediction
new_CoralRichness <- seq(9, 15, 1)
MyData <- data.frame(CoralRichness = new_CoralRichness)
MyPred <- predict(Parrot_poly, newdata = MyData)
lines(new_CoralRichness,MyPred)




#      Figure 7.7
RIKZ <- read.table(file = "RIKZ.txt", header = TRUE)
RIKZ$Richness <- rowSums(RIKZ[, 2:76] > 0)
RIKZ$fExposure <- factor(RIKZ$exposure)
library(mgcv)
RIKZ_admod <- gam(Richness ~ fExposure  +
                             s(grainsize, fx = TRUE, k = 5)  +
                             s(temperature, fx = TRUE, k = 5),
                             data = RIKZ,
                             family = gaussian)
plot(RIKZ_admod)

#Table 7.1
summary(RIKZ_admod)$p.table

#All numerical output, page 110
summary(RIKZ_admod)




#Figure 7.8
Bahama <- read.table("Bahama.txt", header = T)
Bahama2 <- Bahama[Bahama$Method == 2, ]
smooth_parrot <- data.frame(CoralRichness = rep(seq(3,15,1),times=6))
smooth_parrot$dftext <- rep(paste(1:6, "df", ""), each = 13)
smooth_parrot$df <- rep(2:7, each = 13)
library(mgcv)
for(i in 2:7) {
  gam_model <- gam(Parrot ~ s(CoralRichness, k = i, fx = T),
                 data = Bahama2,  family = gaussian)
  smooth_parrot$Parrot[smooth_parrot$df == i] <- predict(gam_model,
        newdata = data.frame(CoralRichness =
        smooth_parrot$CoralRichness[smooth_parrot$df == i]))
}

library(lattice)
xyplot(Parrot ~ CoralRichness|dftext, data = smooth_parrot,
       type = "l", xlab = "Coral richness", ylab = "Parrotfish",
       col=1)





#Figure 7.9
Bahama <- read.table("Bahama.txt", header = T)
Bahama2 <- Bahama[Bahama$Method == 2, ]
smooth_CV <- matrix(nrow = 9)
library(mgcv)
for(i in 3:11) {
      gam_model <- gam(Parrot ~ s(CoralRichness, k = i, fx = T),
                 data = Bahama2,
                 family = gaussian)
      smooth_CV[i-2] <- gam_model$gcv.ubre
}
plot(2:10, smooth_CV, type = "l",
     xlab = "Degrees of freedom", ylab = "CV")




#Figure 7.10 A
Squid <- read.table("Squid.txt", header = T)
library(mgcv)
GSI_admod.4 <- gam(GSI ~ s(MONTH, k = 5, fx = T) + factor(YEAR) +
                         factor(Location) + factor(Sex),
                         data = Squid,
                         family = gaussian(link = identity))
plot(GSI_admod.4)



#Figure 7.10 B
Squid <- read.table("Squid.txt", header = T)
library(mgcv)
GSI_admod <- gam(GSI ~ s(MONTH) + factor(YEAR) + factor(Location) +
                       factor(Sex), data = Squid,
                       family = gaussian(link = identity))
plot(GSI_admod)




#compare models with different degress of freedom, page 116
Squid <- read.table("Squid.txt", header = T)
library(mgcv)
GSI_admod.4 <- gam(GSI ~ s(MONTH, k = 5, fx = T) + factor(YEAR) +
                         factor(Location) + factor(Sex),
                         data = Squid,
                         family = gaussian(link = identity))
GSI_admod <- gam(GSI ~ s(MONTH) + factor(YEAR) +
                       factor(Location) + factor(Sex),
                       data = Squid,
                       family = gaussian(link = identity))
anova(GSI_admod, GSI_admod.4, test = "F")


summary(GSI_admod)




#       Figure 7.11
Squid <- read.table("Squid.txt", header = T)
library(mgcv)
GSI_admod <- gam(GSI ~ s(MONTH) + factor(YEAR) +
                       factor(Location) + factor(Sex),
                       data = Squid,
                       family = gaussian(link = identity))
#A
plot(predict(GSI_admod), Squid$GSI, xlab = "Fitted values",
     ylab = "Observed values")

#B
plot(predict(GSI_admod), residuals(GSI_admod),
     xlab = "Fitted values", ylab = "Residuals")

#C
par(mar = c(4.5, 4.5, 2.5, 0.5))
qqnorm(residuals(GSI_admod))
qqline(residuals(GSI_admod))
#Not sure what goes on with this figure in the book.

#D
par(mar = c(4.5, 4.5, 2.5, 0.5))
hist(residuals(GSI_admod), main = "Histogram of residuals",
     xlab = "Classes", ylab = "Frequency")

#E
par(mar = c(4.5, 4.5, 0.5, 0.5))
plot(GSI_admod$hat, xlab = "Observations", ylab = "Hat values")




#       Figure 7.12
Squid <- read.table("Squid.txt", header = T)
library(mgcv)
GSI_admod <- gam(GSI ~ s(MONTH) + factor(YEAR) +
                      factor(Location) + factor(Sex),
                      data = Squid,
                      family = gaussian(link = identity))
par(mfrow = c(2, 2))
# A
boxplot(residuals(GSI_admod) ~ Sex, data = Squid,
        names = c("Male", "Female"), xlab = "Sex",
        ylab = "Residuals")
#B
boxplot(residuals(GSI_admod) ~ YEAR, data = Squid,
         xlab = "Year", ylab = "Residuals")
#C
boxplot(residuals(GSI_admod) ~ Location, data = Squid,
         xlab = "Location", ylab = "Residuals")
#D ver. 2, like in the book
plot(residuals(GSI_admod) ~ MONTH, data = Squid,
         xlab = "Month", ylab = "Residuals")
par(mfrow = c(1, 1))





#       Figure 7.13
RIKZ <- read.table(file = "RIKZ.txt", header = TRUE)
RIKZ$Richness <- rowSums(RIKZ[, 2:76] > 0)
library(mgcv)
RIKZ_admod <- gam(Richness ~ factor(exposure)  +
                             s(temperature, fx = TRUE, k = 5),
                             data = RIKZ,
                             family = poisson(link = log))
plot(RIKZ_admod)
#model output, page 121
summary(RIKZ_admod)$p.table



#Quasi-Poisson model to inspect overdispersion, 
#page 121 ('scale' parameter in output)
RIKZ <- read.table(file = "RIKZ.txt", header = TRUE)
RIKZ$Richness <- rowSums(RIKZ[, 2:76] > 0)
library(mgcv)
RIKZ_admod <- gam(Richness ~ factor(exposure)  +
                             s(temperature, fx = TRUE, k = 5),
                             data = RIKZ,
                             family = quasipoisson(link = log))
summary(RIKZ_admod)





#      Figure 7.14
Squid <- read.table("Squid.txt", header = T)
library(mgcv)

Squid$IntGSI <- round(Squid$GSI)
#The text in the book should have mentioned that we used rounded density
#data in the Poisson GLM on page 122. Note that GSI is a density.
#In order to apply a Poisson GLM, you need to round it.
#A better approach is to model the counts with the ln(denominator) as an
#offset variable. However, the denominator that was used to calculate the
#GSI density values are not available.
#In a quasi-Poisson, some argue that only the mean-variance relationship
#is modelled, hence no Poisson distribution is assumed, and therefore there
#is no need for rounding the response variable. But there are not many
#references for this.

GSI_admod <- gam(IntGSI ~ s(MONTH) + factor(YEAR) +
                       factor(Location) + factor(Sex),
                       data = Squid,
                       family = poisson)
summary(GSI_admod)

GSI_admod <- gam(IntGSI ~ s(MONTH) + factor(YEAR) +
                       factor(Location) + factor(Sex),
                       data = Squid,
                       family = quasipoisson)
summary(GSI_admod)


plot(GSI_admod)



par(mfrow = c(2, 2))
# A
boxplot(residuals(GSI_admod) ~ Sex, data = Squid,
        names = c("Male", "Female"), xlab = "Sex",
        ylab = "Residuals")
#B
boxplot(residuals(GSI_admod) ~ YEAR, data = Squid,
         xlab = "Year", ylab = "Residuals")
#C
boxplot(residuals(GSI_admod) ~ Location, data = Squid,
         xlab = "Location", ylab = "Residuals")
#D
plot(residuals(GSI_admod) ~ MONTH, data = Squid,
         xlab = "Month", ylab = "Residuals")
par(mfrow = c(1, 1))


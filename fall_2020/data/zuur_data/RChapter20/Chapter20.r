#    R code for: Chapter 20 in:
#    Analysing Ecological Data. (2007). Zuur, Ieno and Smith. Springer, 680p.

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



setwd("D:\\applicat\\HighlandStatistics\\Book\\R\\RChapter20\\")

par(mar = c(4.5, 4.5, 0.5, 0.5), cex.lab = 1.3, cex.axis = 1.3)



#      Figure 20.2
Decapod <- read.table(file="DecapodNew.txt",header=TRUE)
Richness <- rowSums(Decapod[,2:13] > 0)
Decapod.dat<-cbind(Richness,Decapod[,c(19,20,14,15,16,17,18)])
source("MyLibrary.R")
#Note: The panel.cor function in our library file MyLibrary.
pairs(Decapod.dat,lower.panel=panel.cor, upper.panel=panel.smooth2)



#      Figure 20.3
#A
par(mar = c(4.5, 4.5, 2.5, 0.5))
dotchart(Decapod$Ch0.10m, xlab="Range", ylab="Samples", 
         main="Chlorophyll a")
par(mar = c(4.5, 4.5, 0.5, 0.5))

#B
par(mar = c(4.5, 4.5, 2.5, 0.5))
dotchart(sqrt(Decapod$Ch0.10m), xlab="Range", ylab="Samples", 
         main="Chlorophyll a transformed")
par(mar = c(4.5, 4.5, 0.5, 0.5))




#      linear regression results, page 379
Decapod$Richness <- rowSums(Decapod[,2:13] > 0)
Decapod <- Decapod[rowSums(is.na(Decapod) == T)  ==  0, ]
#This line of code removes the rows with missing values, something
#which we did not do in the book. Therefore, there will be small differences
#between results presented here, and the book.


Decapod$Ch0.10mTrans<-sqrt(Decapod$Ch0.10m)
decapod_model<-lm(Richness~T1m+S1m+Ch0.10mTrans+factor(Location)+factor(Year),
                  data=Decapod)

optimal_model<-step(decapod_model)
summary(optimal_model)
#I guess in the book the 1.55 for location should be 1.48.


#      Figure 20.4
M.optim<-lm(Richness~T1m+Ch0.10mTrans+factor(Location),
                  data=Decapod)

#      A
par(mar = c(5,5,1.5,0.5), mfrow = c(2,2))
plot(M.optim, which = c(1:4), add.smooth = F, cex.id = 1)
par(mar = c(4.5, 4.5, 0.5, 0.5), mfrow=c(1,1))

#      B
E <- residuals(M.optim)
plot(Decapod$T1m, E,xlab="Temperature", ylab="Residuals", cex=1.1)
tmp<-loess(E ~ T1m, data=Decapod, span=0.7)
P<-predict(tmp,se=TRUE)
I1<-order(Decapod$T1m)
lines(Decapod$T1m[I1], P$fit[I1], lwd=2)
lines(Decapod$T1m[I1], P$fit[I1]+2*P$se.fit[I1], lwd=1,lty=2)
lines(Decapod$T1m[I1], P$fit[I1]-2*P$se.fit[I1], lwd=1,lty=2)
abline(0,0,lty=2)


#C
plot(Decapod$Ch0.10mTrans, E,xlab="Ch0.10mTrans", ylab="Residuals", cex=1.1)
tmp<-loess(E ~ Ch0.10mTrans, data=Decapod, span=0.7)
P<-predict(tmp,se=TRUE)
I1<-order(Decapod$Ch0.10mTrans)
lines(Decapod$Ch0.10mTrans[I1], P$fit[I1], lwd=2)
lines(Decapod$Ch0.10mTrans[I1], P$fit[I1]+2*P$se.fit[I1], lwd=1,lty=2)
lines(Decapod$Ch0.10mTrans[I1], P$fit[I1]-2*P$se.fit[I1], lwd=1,lty=2)
abline(0,0,lty=2)


#      D
boxplot(E~factor(Decapod$Location),
        xlab="Location", ylab="Residuals")




#  Additive modelling, page 381. Selection of explanatory variables.
Decapod$Richness <- rowSums(Decapod[,2:13] > 0)
#Decapod <- Decapod[rowSums(is.na(Decapod) == T)  ==  0, ]
Decapod$Ch0.10mTrans<-sqrt(Decapod$Ch0.10m)
#Note: It is perhaps better to remove the rows with the missing values.


library(mgcv)
# Selection step 1. 
# Year should be removed from the model, it improves the model
dropmatrix<-data.frame(name=c("none", "T1m","S1m","Ch0.10mTrans","Location","Year"))
 
decapod_admod.1 <- gam(Richness ~ s(T1m) + 
                                  s(S1m) + 
                                  s(Ch0.10mTrans) + 
                                  factor(Location) + 
                                  factor(Year), 
                                  data=Decapod)
decapod_admod.2 <- gam(Richness ~ s(S1m) + 
                                  s(Ch0.10mTrans) + 
                                  factor(Location) + 
                                  factor(Year), 
                                  data=Decapod)
decapod_admod.3 <- gam(Richness ~ s(T1m) + 
                                  s(Ch0.10mTrans) + 
                                  factor(Location) + 
                                  factor(Year), 
                                  data=Decapod)
decapod_admod.4 <- gam(Richness ~ s(T1m) + 
                                  s(S1m) +  
                                  factor(Location) + 
                                  factor(Year), 
                                  data=Decapod)
decapod_admod.5 <- gam(Richness ~ s(T1m) + 
                                  s(S1m) + 
                                  s(Ch0.10mTrans) + 
                                  factor(Year), 
                                  data=Decapod)
decapod_admod.6 <- gam(Richness ~ s(T1m) + 
                                  s(S1m) + 
                                  s(Ch0.10mTrans) + 
                                  factor(Location), 
                                  data=Decapod)

dropmatrix$AIC<-lapply(objects(pat = "decapod_admod.\[1-6]$"), function(n) AIC(get(n)))
dropmatrix


 # Selection step 2
dropmatrix<-data.frame(name=c("none", "T1m","S1m","Ch0.10mTrans","Location"))
decapod_admod.1 <- gam(Richness ~ s(T1m) + 
                                  s(S1m) + 
                                  s(Ch0.10mTrans) + 
                                  factor(Location), 
                                  data=Decapod)
decapod_admod.2 <- gam(Richness ~ s(S1m) + 
                                  s(Ch0.10mTrans) + 
                                  factor(Location), 
                                  data=Decapod)
decapod_admod.3 <- gam(Richness ~ s(T1m) + 
                                  s(Ch0.10mTrans) + 
                                  factor(Location), 
                                  data=Decapod)
decapod_admod.4 <- gam(Richness ~ s(T1m) + 
                                  s(S1m) +  
                                  factor(Location), 
                                  data=Decapod)
decapod_admod.5 <- gam(Richness ~ s(T1m) + 
                                  s(S1m) + 
                                  s(Ch0.10mTrans), 
                                  data=Decapod)

dropmatrix$AIC<-lapply(objects(pat = "decapod_admod.\[1-5]$"), function(n) AIC(get(n)))
dropmatrix
#Differences may be due to different R versions.


#S1m should be removed, because of high p-value
anova(decapod_admod.1) 

#output of final model, page 383
decapod_admod.fin <- gam(Richness ~ s(T1m) + 
                                    s(Ch0.10mTrans) + 
                                    factor(Location), 
                                    data=Decapod)
summary(decapod_admod.fin) 



#Family: gaussian
#Link function: identity
#
#Formula:
#Richness ~ s(T1m) + s(Ch0.10mTrans) + factor(Location)
#
#Parametric coefficients:
#                  Estimate Std. Error t value Pr(>|t|)
#(Intercept)         3.1616     0.3352   9.432 3.29e-11 ***
#factor(Location)2   1.7868     0.5346   3.343  0.00196 **
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#Approximate significance of smooth terms:
#                  edf Est.rank     F  p-value
#s(T1m)          2.142        5 5.617 0.000647 ***
#s(Ch0.10mTrans) 1.325        3 9.937 6.74e-05 ***
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
#R-sq.(adj) =  0.697   Deviance explained =   73%
#GCV score = 2.9907   Scale est. = 2.5919    n = 41




#     Figure 20.5
Decapod <- read.table(file="DecapodNew.txt",header=TRUE)
Decapod <- Decapod[rowSums(is.na(Decapod) == T)  ==  0, ]
Decapod$Richness <- rowSums(Decapod[,2:13] > 0)
Decapod$Ch0.10mTrans<-sqrt(Decapod$Ch0.10m)
library(mgcv)
decapod_admod.fin <- gam(Richness ~ s(T1m) + 
                                    s(Ch0.10mTrans) + 
                                    factor(Location), 
                                    data=Decapod)
#A
plot(decapod_admod.fin, select=1)

#B
plot(decapod_admod.fin, select=2)




#     Figure 20.6
#      A

E <- residuals(decapod_admod.fin)
plot(Decapod$T1m, E,xlab="Temperature", ylab="Residuals", cex=1.1)
tmp<-loess(E ~ T1m, data=Decapod, span=0.65)
P<-predict(tmp,se=TRUE)
I1<-order(Decapod$T1m)
lines(Decapod$T1m[I1], P$fit[I1], lwd=2)
lines(Decapod$T1m[I1], P$fit[I1]+2*P$se.fit[I1], lwd=1,lty=2)
lines(Decapod$T1m[I1], P$fit[I1]-2*P$se.fit[I1], lwd=1,lty=2)
abline(0,0,lty=2)
#Differences with the graphs in the book are due to the missing values;
#they were not removed.

#C
plot(Decapod$Ch0.10mTrans, E,xlab="Ch0.10mTrans", ylab="Residuals", cex=1.1)
tmp<-loess(E ~ Ch0.10mTrans, data=Decapod, span=0.7)
P<-predict(tmp,se=TRUE)
I1<-order(Decapod$Ch0.10mTrans)
lines(Decapod$Ch0.10mTrans[I1], P$fit[I1], lwd=2)
lines(Decapod$Ch0.10mTrans[I1], P$fit[I1]+2*P$se.fit[I1], lwd=1,lty=2)
lines(Decapod$Ch0.10mTrans[I1], P$fit[I1]-2*P$se.fit[I1], lwd=1,lty=2)
abline(0,0,lty=2)


#      D
boxplot(E~factor(Decapod$Location),
        xlab="Location", ylab="Residuals")










#comparison of linear regression model and additive model
Decapod <- read.table(file="DecapodNew.txt",header=TRUE)
Decapod <- Decapod[rowSums(is.na(Decapod) == T)  ==  0, ]
Decapod$Richness <- rowSums(Decapod[,2:13] > 0)
Decapod$Ch0.10mTrans<-sqrt(Decapod$Ch0.10m)
decapod_model <- lm(Richness ~ T1m+Ch0.10m+factor(Location), 
                  data=Decapod)
library(mgcv)
decapod_admod.fin <- gam(Richness ~ s(T1m) + 
                                    s(Ch0.10mTrans) + 
                                    factor(Location), 
                                    data=Decapod)
anova.lm(decapod_admod.fin, decapod_model) 
#or 
anova(decapod_model,decapod_admod.fin) 



 
 
#      Figures 20.7 and 20.8. 
#      "How many samples to take?"  - a bootstrap test.
Decapod <- read.table(file = "DecapodNew.txt", header = TRUE)
Decapod <- Decapod[rowSums(is.na(Decapod) == T)  ==  0, ]
Richness <- rowSums(Decapod[, 2:13] > 0)
Ch0.10mTrans <- sqrt(Decapod$Ch0.10m)
Decapod.dat <- cbind(Richness, Ch0.10mTrans, Decapod[, c(20, 14)])


library(gam)

full_model <- gam(Richness ~ s(T1m, df = 2) + 
                           s(Ch0.10mTrans, df = 2) + 
                           as.factor(Location), 
                           data = Decapod.dat)

Temp.full <- full_model$smooth[, 1]
Chlo.full <- full_model$smooth[, 2]
Loc.full <- full_model$coefficients[4]

N <- nrow(Decapod.dat)
MySelect <- rep(T, N)

subset_out <- c(4, 10, 14, 20)
subset_title <- c("10%", "25%", "35%", "50%")

NBOOT <- 20

#This code generate 3 graphics window. Drag the window with
#your mouse to see other two.

dev.off()
windows()
par(mfrow = c(2, 2), mar = c(4.5, 4.5, 2.5, 0.5), cex.lab = 1.5, cex.axis = 1.5)


windows()
par(mfrow = c(2, 2), mar = c(4.5, 4.5, 2.5, 0.5), cex.lab = 1.5, cex.axis = 1.5)

windows()
par(mfrow = c(2, 2), mar = c(4.5, 4.5, 2.5, 0.5), cex.lab = 1.5, cex.axis = 1.5)

for (s in 1:length(subset_out)) {

     TM <- matrix(nrow = N, ncol = NBOOT)
     CH <- matrix(nrow = N, ncol = NBOOT)
     LOC <- matrix(nrow = N, ncol = NBOOT)

     N.OUT <- subset_out[s]
     MyTitle <- subset_title[s]

     for (i in 1:NBOOT){
          MySelect <- seq(from = 1, to = N, times = 1)
          Yout <- sample(MySelect)[1:N.OUT]
          MySelect[Yout] <- F

          subset_model <- gam(Richness ~ s(T1m, df = 2) + 
                              s(Ch0.10mTrans, df = 2) + 
                              as.factor(Location), 
                              data = Decapod.dat, 
                              subset = MySelect)

         subset_model.predict <- predict.gam(subset_model, 
                                             newdata = Decapod.dat, 
                                             subset = MySelect, 
                                             type = "terms")
         TM[, i] <- subset_model.predict[, 1]
         CH[, i] <- subset_model.predict[, 2]
         LOC[, i] <- subset_model.predict[, 3]
     }

      Pred.full <- predict.gam(full_model, type = "terms", se = T)

     #TEMPERATURE
       Pred.Temp <- Pred.full$fit[, 1]
       II <- order(Decapod.dat$T1m)
       plot(sort(Decapod.dat$T1m), Pred.Temp[II], type = "l", 
            lwd = 5, col = "red", ylim = c(-2, 3), main = MyTitle, 
            xlab = "Temperature", ylab = "Smoother")
       SE.T1 <- Pred.full$se.fit[, 1][II]
       lines(sort(Decapod.dat$T1m), Pred.Temp[II]+2*SE.T1, 
             lty = 2, lwd = 3)
       lines(sort(Decapod.dat$T1m), Pred.Temp[II]-2*SE.T1, 
             lty = 2, lwd = 3)

       for (i in 1:NBOOT){
            lines(sort(Decapod.dat$T1m), TM[II, i])
       }


    dev.set(dev.next())

      #CHLOROHYLL
        Pred.Chlor <- Pred.full$fit[, 2]
        II <- order(Ch0.10mTrans)
        plot(sort(Ch0.10mTrans), Pred.Chlor[II], 
             type = "l", col = "red", lwd = 5, ylim = c(-3, 4), 
             main = MyTitle, xlab = "Chlorophyll", ylab = "Smoother")
        SE.CH <- Pred.full$se.fit[, 2][II]

        lines(sort(Ch0.10mTrans), Pred.Chlor[II]+2*SE.CH, lty = 2, lwd = 3)
        lines(sort(Ch0.10mTrans), Pred.Chlor[II]-2*SE.CH, lty = 2, lwd = 3)

        for (i in 1:NBOOT){
             lines(sort(Ch0.10mTrans), CH[II, i])
        }


    dev.set(dev.next())

      #LOCATION
        Pred.LOC <- Pred.full$fit[, 3]
        II <- order(Decapod.dat$Location)
        plot(sort(Decapod.dat$Location), Pred.LOC[II], type = "l", 
             lwd = 3, ylim = c(-2, 2), main = MyTitle, xlab = "Location", 
             ylab = "Factor")
        SE.LOC <- Pred.full$se.fit[, 3][II]

        points(sort(Decapod.dat$Location), Pred.LOC[II]+2*SE.LOC, 
               lty = 2, lwd = 5)
        points(sort(Decapod.dat$Location), Pred.LOC[II]-2*SE.LOC, 
               lty = 2, lwd = 5)

        for (i in 1:NBOOT){
             points(sort(Decapod.dat$Location), LOC[II, i], lwd = 1)
        }


   dev.set(dev.next())

} #s cycle end


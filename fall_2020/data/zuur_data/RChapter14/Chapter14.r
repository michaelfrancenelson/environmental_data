#    R code for: Chapter 14 in:
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



setwd("D:\\applicat\\HighlandStatistics\\Book\\R\\RChapter14\\")

library(MASS) 

par(mar = c(4.5, 4.5, 0.5, 0.5), cex.lab = 1, cex.axis = 1)




Y<-read.table("SparrowDA.txt", header = T)
names(Y)
attach(Y)


#      Count of ovservation per observer
#      numbering of observers goes from 0 to 9
table(observer)
# Count of observations is between 9 and 297
# so we have serious problem with unequal group sizes.
# Using the 9:1 rule we need to drop the observers: No. 0 and No. 9
Y<-Y[(observer!= 0)&(observer!= 9), ]
detach(Y)
attach(Y)




#     Figure 14.2
boxplot(flatwing ~ observer, data = Y, outline = T, width = table(observer)^0.5, 
        xlab = "flatwing")




#      Other boxplots
par(mfrow = c(3, 2))
boxplot(wingcrd ~ observer, data = Y, outline = T, 
        width = table(observer)^0.5, xlab = "wingcrd")
boxplot(tarsus ~ observer, data = Y, outline = T, 
        width = table(observer)^0.5, xlab = "tarsus")
boxplot(head ~ observer, data = Y, outline = T, 
        width = table(observer)^0.5, xlab = "head")
boxplot(culmen ~ observer, data = Y, outline = T, 
        width = table(observer)^0.5, xlab = "culmen")
boxplot(nalospi ~ observer, data = Y, outline = T, 
        width = table(observer)^0.5, xlab = "nalospi")
boxplot(wt ~ observer, data = Y, outline = T, 
        width = table(observer)^0.5, xlab = "wt")
par(mfrow = c(1, 1))




#     Cleveland plots
ord<-order(observer)
dotchart(Y$flatwing[ord], lcolor = 0, color = observer[ord], 
         pch = 19, xlab = "flatwing")
dotchart(Y$wingcrd[ord], lcolor = 0, color = observer[ord], 
         pch = 19, xlab = "wingcrd")
dotchart(Y$tarsus[ord], lcolor = 0, color = observer[ord], 
         pch = 19, xlab = "tarsus")
dotchart(Y$head[ord], lcolor = 0, color = observer[ord], 
         pch = 19, xlab = "head")
dotchart(Y$culmen[ord], lcolor = 0, color = observer[ord], 
         pch = 19, xlab = "culmen")
dotchart(Y$nalospi[ord], lcolor = 0, color = observer[ord], 
         pch = 19, xlab = "nalospi")
dotchart(Y$wt[ord], lcolor = 0, color = observer[ord], 
         pch = 19, xlab = "wt")




#       Correlation and pairplot of variables
source("Mylibrary.r")
plot(Y[, 1:7], lower.panel = panel.cor)
# One of wingcrd and flatwing should be removed
# because of high correlation (0.99)
# we decided to drop wingcrd  (first column).

 

 
#       De-seasonalisation
Y$wtnew<-residuals(lm(wt~factor(Month), data=Y))



 

#       Discriminant function 
discrim1<-lda(observer ~ flatwing + tarsus + head + culmen + nalospi + wtnew, data = Y)
discrim1
# Coefficients of linear discriminants for LD1 
# give coefficients of first dicriminant function (14.3)




#Normalisation of data
detach(Y)
Y$flatwing<-(Y$flatwing-mean(Y$flatwing))/sd(Y$flatwing)
Y$tarsus<-(Y$tarsus-mean(Y$tarsus))/sd(Y$tarsus)
Y$head<-(Y$head-mean(Y$head))/sd(Y$head)
Y$culmen<-(Y$culmen-mean(Y$culmen))/sd(Y$culmen)
Y$nalospi<-(Y$nalospi-mean(Y$nalospi))/sd(Y$nalospi)
Y$wtnew<-(Y$wtnew-mean(Y$wtnew))/sd(Y$wtnew)
attach(Y)

discrim2<-lda(observer ~ flatwing + tarsus + head + culmen + nalospi + wtnew, data = Y)
discrim2
# Coefficients of linear discriminants for LD1: 
# give coefficients of first dicriminant function (14.4)

#Just in case you need the normalised data in ascii format for another package:
Z <- cbind(Y$flatwing, Y$tarsus, Y$head, Y$culmen, Y$nalospi, Y$wtnew, Y$observer)
colnames(Z)<- c("flatwing", "tarsus", "head", "culmen", "nalospi", "wtnew","Observer")
write.table(Z, file = "c:/sparrowsnorm.txt")

#     Figure 14.3
plot(predict(discrim2)$x[, 1], predict(discrim2)$x[, 2], type = "n", 
     xlab = "Discriminant function 1", ylab = "Discriminant function 2")
text(predict(discrim2)$x[, 1], predict(discrim2)$x[, 2], labels = observer)




#      Figure 14.4
group_average.1<-tapply(predict(discrim2)$x[, 1], observer, mean)
group_average.2<-tapply(predict(discrim2)$x[, 2], observer, mean)
plot(predict(discrim2)$x[, 1], predict(discrim2)$x[, 2], type = "n", 
     xlab = "Discriminant function 1", ylab = "Doscriminant function 2")

#points(group_average.1, group_average.2, pch = 17, cex = 2)
text(group_average.1, group_average.2, 1:8,cex = 1)

Radius <- 2.15

#NG = number of groups
NG <- 8
for (i in 1:NG) {
     x3 <- vector(length=72)
     y3<- vector(length=72)
     t1 <- 0
     for (j in 1:72) {
         x3[j] <- Radius * sin(t1)+ group_average.1[i]
         y3[j] <- Radius * cos(t1)+ group_average.2[i]
         t1 <- t1 + 3.1416/36
     }
     lines(x3,y3)
     }

abline(0,0)
abline(h=0,v=0)



#The software package Brodgar was used to generate Figure 14.4. You will
#need to do some manual programming in the code above for your own data
#(e.g. change the number of groups, and the group labels in the text command).
#Brodgar has tools for automatic backwards selection based on the Mahalanobis distance.
#To do this in R would require some tedious programming


#     Figure 14.4
# Instead of calculation of 90% tolerance interval 
# one can use this rough estimation

#You need to install the ade4 package: Click Packages, select server, Install package
library(ade4)

plot(predict(discrim2)$x[, 1], predict(discrim2)$x[, 2], type = "n", 
     xlab = "Discriminant function 1", ylab = "Discriminant function 2")
s.class(cbind(predict(discrim2)$x[, 1], predict(discrim2)$x[, 2]), 
        fac = as.factor(observer), axesell = F, cpoint = 0, cstar = 0, 
        cellipse = 2, grid = F, col = rainbow(8), add.plot = T)




#      Figure 14.5
correlation<-cor(x = Y[, c(2:6,11)], y = predict(discrim2)$x)
plot(c(-0.8, 0.8), c(-0.8, 0.8), type = "n", 
     xlab = "Discriminant function 1", ylab = "Discriminant function 2")
lines(c(-1, 1), c(0, 0), lty = 2)
lines(c(0, 0), c(-1, 1), lty = 2)
library(ade4)
s.arrow(correlation[, 1:2], grid = F, add.plot = T, addaxes = T, clabel = 1.5)
correlation
# Correlation coefficients between DA axis 
# and each of the original variables



#Note
#The numerical output in the book was taken from Brodgar (www.brodgar.com),
#which runs the discriminant analysis routine DSCRM from the FORTRAN IMSL library.
#All Brodgar output was compared with the examples in Huberty (1994), and results
#are identical. It is therefore confusing that R gives slightly different output for
#the predict function and for some of the test statistics, see below.
#I guess that differences are due to specific options in the predict.lda
#function in R such as "prior" and "method".


#Group effect
summary(manova(as.matrix(Y[, c(2:6,11)]) ~ observer), "Wilks")
summary(manova(as.matrix(Y[, c(2:6,11)]) ~ observer), "Pillai")
summary(manova(as.matrix(Y[, c(2:6,11)]) ~ observer), "Hotelling-Lawley")
#Note the difference with the book (and Brodgar output, see note above)



#Classification table (page 257)
table(observer, predict(discrim2)$class)
# |                -> classified as
# V real observer
#Note the difference with the book (and Brodgar output, see note above)


#The percentage of correctly classified samples per groups
for (i in 1:length(unique(observer))) {
cat(i, round(sum(observer == i&predict(discrim2)$class == i)/
                sum(observer == i)*100, 2), "\n")
}

#Note the difference with the book (and Brodgar output, see note above)


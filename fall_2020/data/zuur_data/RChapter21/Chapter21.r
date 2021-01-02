#    R code for: Chapter 21 in:
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



setwd("D:\\applicat\\HighlandStatistics\\Book\\R\\RChapter21")


par(mar = c(4.5, 4.5, 0.5, 0.5), cex.lab = 1.3, cex.axis = 1.3)
Solea <- read.table("Solea.txt", header = T)


#     Figure 21.2
source("MyLibrary.R")
plot(Solea[, 9:12], lower.panel = panel.cor, 
     upper.panel = panel.smooth2)

 
 
#     Figure 21.3
source("MyLibrary.R")
plot(Solea[, c(13, 5, 6, 7, 8, 9, 12)], lower.panel = panel.cor, 
     upper.panel = panel.smooth2)
 
 
 
#     Figure 21.4
data_solea <- data.frame(Solea_solea = Solea$Solea_solea,
                         season = as.factor(Solea$season), 
                         month = as.factor(Solea$month), 
                         area = as.factor(Solea$Area))
plot.design(Solea_solea ~ season + month + area, 
            data = data_solea, axes = T, xtick = T)
 
 
 
#     Figure 21.5
coplot(Solea_solea ~ salinity|month, data = Solea, panel = panel.smooth2,
       number = 4, xlab = c("Salinity", "Month"))

 
 
#     Figures 21.6 Classification tree
library(rpart)

f1 <- formula(as.factor(Solea_solea) ~ depth +
           temperature + salinity + transparency +
           gravel + mud + factor(month) + Area)
           
solea_tree <- rpart(f1, data = Solea, method = "class",
                    minsplit=5, cp = 0.001)
                    
par(xpd = NA, mar = c(1.5, 1.5, 1.5, 1.5))
plot(solea_tree,uniform=TRUE, margin=0.1)
text(solea_tree, use.n = T, cex = 1.0)
par(xpd = F, mar = c(4.5, 4.5, 0.5, 0.5))


# Figures 21.7. Cross-validation
par(mar = c(4.5, 4.5, 4.5, 0.5))
plotcp(solea_tree)
par(mar = c(4.5, 4.5, 0.5, 0.5))



#     Table 21.2
library(mgcv)
Solea.gam1 <- gam(Solea_solea ~ s(depth),data = Solea, family = binomial)
Solea.gam2 <- gam(Solea_solea ~ s(salinity),data = Solea, family = binomial)
Solea.gam3 <- gam(Solea_solea ~ s(temperature),data = Solea, family = binomial)
Solea.gam4 <- gam(Solea_solea ~ s(transparency),data = Solea, family = binomial)
Solea.gam5 <- gam(Solea_solea ~ s(gravel),data = Solea, family = binomial)
Solea.gam6 <- gam(Solea_solea ~ s(mud),data = Solea, family = binomial)

AIC(Solea.gam1,Solea.gam2,Solea.gam3,Solea.gam4,Solea.gam5,Solea.gam6)
#use the summary command to get the edf.

#Figure 21.8
plot(Solea.gam2)



##################################
#     Generalised linear modelling
solea_glm <- glm(Solea_solea ~ temperature +
                               salinity + 
                               gravel +
                               factor(month),
                data = Solea, family = binomial)

#Table 21.3
summary(solea_glm)

#Table 21.4
drop1(solea_glm,test="Chi")

#figure 21.9
par(mfrow=c(2,2))
termplot(solea_glm,se=T,ask=F,rug=T,
         col.term = 1,col.se = "black",
         terms=1,partial.resid=T)

termplot(solea_glm,se=T,ask=F,rug=T,
         col.term = 1,col.se = "black",
         terms=2,partial.resid=T)


termplot(solea_glm,se=T,ask=F,rug=T,
         col.term = 1,col.se = "black",
         terms=3,partial.resid=T)

termplot(solea_glm,se=T,ask=F,rug=T,
         col.term = 1,col.se = "black",
         terms=4,partial.resid=T)




#############################################
#Refit with GAM on page 399
#Note that the differences between the output here, and in the book are due
#to using the gam function from the gam package (in the book), and different
#R versions
Solea.gam <- gam(Solea_solea ~ s(salinity)+s(temperature)+s(gravel)+factor(month),
                   data = Solea, family = binomial)
plot(Solea.gam)
summary(Solea.gam)



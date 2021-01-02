#    R code for: Chapter 9 in:
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



setwd("D:\\applicat\\HighlandStatistics\\Book\\R\\RChapter9\\")

par(mar = c(4.5, 4.5, 0.5, 0.5), cex.lab = 1.3, cex.axis = 1.3)



#       Output of regression tree, page 147
Bahama <- read.table("Bahama.txt", header = T)
library(rpart)
parrot_tree <- rpart(Parrot ~ CoralTotal + 
                              as.factor(Month)  + 
                              as.factor(Station) + 
                              as.factor(Method), 
                              data = Bahama, 
                              control = rpart.control(cp = 0.02))
parrot_tree



#      Figure 9.3
parrot_tree <- rpart(Parrot ~ CoralTotal +
                              as.factor(Month)  + 
                              as.factor(Station) + 
                              as.factor(Method), 
                              data = Bahama, 
                              control = rpart.control(cp = 0.02))
par(xpd = NA, mar = c(2.5, 5, 2.5, 5))
plot(parrot_tree)
text(parrot_tree, cex = 1.5, use.n = T)
par(xpd = F, mar = c(4.5, 4.5, 0.5, 0.5))




#      Figure 9.4 and Table 9.2
parrot_tree <- rpart(Parrot ~ CoralTotal +
                              as.factor(Month)  + 
                              as.factor(Station) + 
                              as.factor(Method), 
                              data = Bahama, 
                              control = rpart.control(cp = 0.001))
par(mar = c(4.5, 4.5, 2.5, 0.5))
plotcp(parrot_tree)
printcp(parrot_tree)
par(mar = c(4.5, 4.5, 0.5, 0.5))



#       Figure 9.5 and model output, page 154-156
Ditch <- read.table("Ditch.txt", header = T)
library(rpart)
ditch_tree <- rpart(as.factor(Site) ~ Depth + pH + Conductivity  + 
                              BOD + Ammoniacal_Nitrogen + 
                              Total_Oxidised_Nitrogen  + 
                              Suspended_Solids + Chloride + 
                              Sulphate + Total_Calcium + 
                              Total_Zinc + Total_Cadmium + 
                              Total_Lead + Total_Nickel + 
                              Total_Phosphate, 
                              data = Ditch, method = "class", 
                              control = rpart.control(cp = 0.001),
                              minsplit=15)
par(mar = c(4.5, 4.5, 2.5, 0.5))
plotcp(ditch_tree)
printcp(ditch_tree)
ditch_tree
par(mar = c(2.5, 5, 2.5, 5))



#     Figure 9.6

ditch_tree <- rpart(as.factor(Site) ~ Depth + pH + Conductivity  +
                              BOD + Ammoniacal_Nitrogen + 
                              Total_Oxidised_Nitrogen  + 
                              Suspended_Solids + Chloride + 
                              Sulphate + Total_Calcium + 
                              Total_Zinc + Total_Cadmium + 
                              Total_Lead + Total_Nickel + 
                              Total_Phosphate, 
                              data = Ditch, method = "class", 
                              control = rpart.control(cp = 0.01))
par(xpd = NA, mar = c(2.5, 5, 2.5, 5))
plot(ditch_tree)
text(ditch_tree, use.n = T, cex = 1.5)
par(xpd = F, mar = c(4.5, 4.5, 0.5, 0.5))



#     Figure 9.7
dotchart(Ditch$Total_Calcium,  pch = Ditch$Site,
         xlab = "Range", ylab = "Sample",
         main = "Total Calcium")




#       Estimate regression parameters of optimal multinomial model  
Ditch <- read.table("Ditch.txt", header = T)
Ditch <- Ditch[rowSums(is.na(Ditch) == T)  ==  0, ]
library(nnet)
library(MASS)


LogDitch<-log10(Ditch[,4:18]+1)
LogDitch$fSite<-factor(Ditch$Site)
LogDitch$fYear<-factor(Ditch$Year)
LogDitch$fMonth<-factor(Ditch$Month)



f1<-formula(fSite~1+Depth+pH+Conductivity+BOD+Ammoniacal_Nitrogen+
                 Total_Oxidised_Nitrogen+Suspended_Solids+
                 Chloride+Sulphate+Total_Calcium+Total_Zinc+
                 Total_Cadmium+Total_Lead+Total_Nickel+
                 Total_Phosphate+fYear+fMonth)
tmp<-multinom(f1,data=LogDitch)
dropterm(tmp)

#Remove month, and repeat the process.
#........
#........


#page 160
f1<-formula(fSite~1+Depth+Ammoniacal_Nitrogen+
                 Total_Oxidised_Nitrogen+Total_Calcium+Total_Zinc)
M.optim<-multinom(f1,data=LogDitch)
summary(M.optim,Wald=T,cor=F)

#Table 9.3
dropterm(M.optim, test = "Chisq")
#It seems that the row for Depth in the book is wrong

#fSite ~ 1 + Depth + Ammoniacal_Nitrogen + Total_Oxidised_Nitrogen +
#    Total_Calcium + Total_Zinc
#                        Df     AIC     LRT   Pr(Chi)
#<none>                      77.158
#Depth                    4  87.906  18.748 0.0008808 ***
#Ammoniacal_Nitrogen      4  93.918  24.761 5.621e-05 ***
#Total_Oxidised_Nitrogen  4  92.753  23.596 9.624e-05 ***
#Total_Calcium            4 125.579  56.421 1.636e-11 ***
#Total_Zinc               4  89.064  19.906 0.0005212 ***
#---
#Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#>

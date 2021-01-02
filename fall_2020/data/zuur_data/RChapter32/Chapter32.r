#    R code for: Chapter 32 in:
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

#All results in the book chapter were carried outr with Brodgar
#and are identical to CANOCO. We had some problems with the partial
#CCA in R; we think that this is due to its factor(Block) use. We therefore
#used dummy variables B1, B2 and B3.


setwd("D:\\applicat\\HighlandStatistics\\Book\\R\\RChapter33\\")


#############################
#
#     Data exploration. 


mplants <- read.table("MexicanPlants.txt", header = T, 
                      sep = "\t", row.names = 1)
families <- mplants[, 2:33]
expl_variables <- mplants[, 37:46]
names(families)
names(expl_variables)



#      Figure 32.3
plot(c(0, ncol(families)), c(0, nrow(families)), type = "n", 
     xlab = "Families", ylab = "Observations")

for (i in 1:ncol(families)) {
     for (j in 1:nrow(families)) {
          if (families[j, i] == 0) points(x = i, y = j, pch = "-",cex=0.7)
     }
}
#      There is a large number of observations equal to zero.
#      This indicates that PCA and RDA are not appropriate for 
#      this data.
#      Instead of applying NMDS or RDA combined with the Chord 
#      transformation it was decided to use the average of 10 
#      replicates per pasture.





#      Averaging replicates per pastures.
averaged_mplants <- apply(mplants, MARGIN = 2, FUN = function (x) {
                          tapply(x, INDEX = mplants$Pasture, FUN = mean)})
averaged_mplants <- as.data.frame(averaged_mplants)
averaged_mplants <- averaged_mplants[order(averaged_mplants$Block), ]
families <- averaged_mplants[, 2:33]
expl_variables <- averaged_mplants[, 37:46]

names(families)
names(expl_variables)




#      Cleveland dotplots for families data
par(mfrow = c(2, 3))
par(ask = T)
for (i in 1:ncol(families)) {
     dotchart(families[, i], groups = averaged_mplants$Block, 
                            labels = averaged_mplants$Block, 
                            pch = averaged_mplants$Block, 
                            col = averaged_mplants$Block, 
                            main = colnames(families)[i], 
                            ylab = "Observations by blocks")
     if(sum(families[, i]>0)<5) {
        text(0, 13.5, "less than five observations", pos = 4)
     }
}
par(mfrow = c(1, 1))
par(ask = F)





#     Some families were measured at less than five pastures:
#     bo, had, hsch, hse, sa.
#     Therefore these families were omitted from the analysis:
index<-(names(families)!="bo")&
       (names(families)!="had")&
       (names(families)!="hsch")&
       (names(families)!="hse")&
       (names(families)!="sa")

new_families <- families[, c(index)]
names(new_families)
names(expl_variables)
#In the book, we did not omit ara...for some obscure reason




#     Cleveland dotplots for explanatory variables
par(mfrow = c(2, 3))
par(ask = T)
for (i in 1:ncol(expl_variables)) {
     dotchart(expl_variables[, i], groups = averaged_mplants$Block, 
                                  labels = averaged_mplants$Block, 
                                  pch = averaged_mplants$Block, 
                                  col = averaged_mplants$Block, 
                                  main = colnames(expl_variables)[i], 
                                  ylab = "Observations by blocks")
}
par(mfrow = c(1, 1))
par(ask = F)

#      Bared soil had one very large observation, so bared soil 
#      was log transformed:
expl_variables$BAREDSOIL <- log(averaged_mplants$BAREDSOIL + 1)

#      Weeding frequency had 16 pastures with the same value, so
#      this explanatory variable were omitted from the analysis:
names(expl_variables)
index<-names(expl_variables)!="WEEDINGFRECUENCYIND"
new_expl_variables <- expl_variables[, index]
names(new_expl_variables)






#      Figure 32.4
#      Pairplots of continuous explanatory variables 
#      to inspect collinearity (so block and plague are omitted)
source("MyLibrary.r")
plot(new_expl_variables[, c(2:5, 7:9)], lower.panel = panel.cor, 
     upper.panel = panel.smooth2)




#      Minimum and maximum vegetation height have a cross-correlation 
#      of 0.82. This is strong linear relationship. So it was decided
#      to drop minimum vegetation height.
names(expl_variables)
index<-(names(expl_variables)!="MINVEGHEIGHT")&
       (names(expl_variables)!="WEEDINGFRECUENCYIND")
new_expl_variables <- expl_variables[, index]
names(new_expl_variables)




#      Boxplots of explanatory variables conditional on "block"
par(mfrow = c(2, 2))
par(ask = T)
for (i in 2:ncol(new_expl_variables)) {
boxplot(as.matrix(new_expl_variables[i]) ~ new_expl_variables$Block, 
        xlab = "Block", ylab = colnames(new_expl_variables)[i])
}
par(mfrow = c(1, 1))
par(ask = F)
#      Boxplots give the impression that there is a block effect in 
#      some of explanatory variables.





#       Writing the data into file 
new_mplants <- cbind(Pasture = averaged_mplants$Pasture, 
                   new_families, new_expl_variables)
                   
names(new_mplants)
write.table(new_mplants, file = "avg_MexicanPlants.txt", 
            sep = "\t", quote = F)








#################################################
#
#      Canonical correspondence analysis results.



setwd("D:\\applicat\\HighlandStatistics\\Book\\R\\RChapter33\\")


#mplants <- read.table("avg_MexicanPlants.txt", header = T, sep = "\t",
#                      row.names = 1)
                      
mplants<-read.table(file="MexicanPlantsTotals.txt", header=TRUE)

dim(mplants)
names(mplants)
families <- mplants[, 1:27]
expl_variables <- mplants[, 28:37]
names(families)
names(expl_variables)



#      CCA
library(vegan)
mplants_CCA <- cca(families ~ B1+B2+B3 + as.factor(PLAGUE) +
                          ALTITUDE + AGE + CATTLEINTENSITY + 
                          FIELDSLOPE + BAREDSOIL + MAXVEGHEIGHT, 
                          data = expl_variables)




#      Figure 32.5
plot(mplants_CCA, scaling = 1, type = "n",
     xlab = "axis 1", ylab = "axis 2")
text(mplants_CCA, scaling = 1, display = "cn", head.arrow = 0,
     lwd = 2, col = "red")
points(mplants_CCA, scaling = 1, display = "cn", head.arrow = 0,
       lwd = 2, pch = 15, cex = 1)
text(mplants_CCA, scaling = 1, dis = "species", col = "green4")
text(mplants_CCA, scaling = 1, dis = "sites", col = "blue2",
       pch = 19, cex = 1)

#Axes may be reversed due to cca algorithm in vegan package




#      Total inertia (variation) in the family data:
round(mplants_CCA$tot.chi, 2)

#      The inertia explained by all explanatory variables:
round(sum(mplants_CCA$CCA$eig), 2)

#      The inertia explained by all explanatory variables, %:
cat(round((sum(mplants_CCA$CCA$eig)/mplants_CCA$tot.chi)*100), 
    "%", "\n")

#      Part the first two canonical axes in total inertia 
#      explained by all explanatory variables:
cat(round(sum(mplants_CCA$CCA$eig[1:2])/sum(mplants_CCA$CCA$eig)*100), 
    "%", "\n")

#      Part the first two canonical axes in total inertia 
#      of the family data:
cat(round(sum(mplants_CCA$CCA$eig[1:2])/mplants_CCA$tot.chi*100), 
    "%", "\n")




#Table 32.2
#See our comments in Chapter 27 how to do a forward selection with
#CCA and RDA in R. Please email us if you figure out how to do
#it fully automatically.





#     Table 32.3
#     VIF values for all explanatory variables. 
corvif(expl_variables)






############################################
#
#      Partial CCA and variance partitioning

mplants<-read.table(file="MexicanPlantsTotals.txt", header=TRUE)
dim(mplants)
names(mplants)
families <- mplants[, 1:27]
expl_variables <- mplants[, 28:37]
names(families)
names(expl_variables)




library(vegan)
mplants_CCA.1 <- cca(families ~ B1+B2+B3 + as.factor(PLAGUE) +
                                ALTITUDE + AGE + CATTLEINTENSITY + 
                                FIELDSLOPE + BAREDSOIL + MAXVEGHEIGHT, 
                     data = expl_variables)


#Alternative, use BLOCK as a factor:
expl_variables$Block<-vector(length=20)
expl_variables$Block[1:20]<-4
expl_variables$Block[expl_variables$B1==1]<-1
expl_variables$Block[expl_variables$B2==1]<-2
expl_variables$Block[expl_variables$B3==1]<-3

mplants_CCA.1A <- cca(families ~ factor(Block) + as.factor(PLAGUE) +
                                ALTITUDE + AGE + CATTLEINTENSITY +
                                FIELDSLOPE + BAREDSOIL + MAXVEGHEIGHT,
                     data = expl_variables)
#Gives same results

mplants_CCA.2 <- cca(families ~ as.factor(Block), 
                     data = expl_variables)
                     
mplants_CCA.3 <- cca(families ~ as.factor(PLAGUE) + 
                                ALTITUDE + AGE + CATTLEINTENSITY + 
                                FIELDSLOPE + BAREDSOIL + MAXVEGHEIGHT, 
                     data = expl_variables)
                     
#mplants_CCA.4 <- cca(families ~ as.factor(Block) +
#                                Condition(as.factor(PLAGUE)+
#                                ALTITUDE+ AGE+ CATTLEINTENSITY+
#                                FIELDSLOPE+ BAREDSOIL+ MAXVEGHEIGHT),
#                     data = expl_variables)
#Gave odd results, must have made a mistake somewhere

Z<-cbind(expl_variables$PLAGUE, expl_variables$ALTITUDE,
         expl_variables$AGE,expl_variables$CATTLEINTENSITY,
         expl_variables$FIELDSLOPE, expl_variables$BAREDSOIL,
         expl_variables$MAXVEGHEIGHT)
         
Y<-cbind(expl_variables$B1,expl_variables$B2,expl_variables$B3)
mplants_CCA.4 <- cca(X=families,Y=Y,Z=Z,
                     data = expl_variables)

#This is identical as in the book
summary.cca(mplants_CCA.4)
#mplants_CCA.5 <- cca(families ~ Condition(as.factor(Block)) +
#                                as.factor(PLAGUE) + ALTITUDE +
#                                AGE + CATTLEINTENSITY +
#                                FIELDSLOPE + BAREDSOIL + MAXVEGHEIGHT,
#                    data = expl_variables)
##Gave odd results, must have made a mistake somewhere

mplants_CCA.5 <- cca(X=families,Y=Z, Z=Y,
                    data = expl_variables)



#     Table 32.4
#     Results of the partial CCA
Expl_intertia.1 <- sum(mplants_CCA.1$CCA$eig)
Expl_intertia.2 <- sum(mplants_CCA.2$CCA$eig)
Expl_intertia.3 <- sum(mplants_CCA.3$CCA$eig)
Expl_intertia.4 <- sum(mplants_CCA.4$CCA$eig)
Expl_intertia.5 <- sum(mplants_CCA.5$CCA$eig)

Expl_percent.1 <- sum(mplants_CCA.1$CCA$eig)/mplants_CCA.1$tot.chi*100
Expl_percent.2 <- sum(mplants_CCA.2$CCA$eig)/mplants_CCA.2$tot.chi*100
Expl_percent.3 <- sum(mplants_CCA.3$CCA$eig)/mplants_CCA.3$tot.chi*100
Expl_percent.4 <- sum(mplants_CCA.4$CCA$eig)/mplants_CCA.4$tot.chi*100
Expl_percent.5 <- sum(mplants_CCA.5$CCA$eig)/mplants_CCA.5$tot.chi*100

cat(
" Block_and_others            Explained_intertia", round(Expl_intertia.1, 2),
                                 "\t", round(Expl_percent.1), "%\n", 
 "Blocks                      Explained_intertia", round(Expl_intertia.2, 2),
                                 "\t", round(Expl_percent.2), "%\n", 
 "Others                      Explained_intertia", round(Expl_intertia.3, 2),
                                 "\t", round(Expl_percent.3), "%\n", 
 "Block with oth. as covar.   Explained_intertia", round(Expl_intertia.4, 2),
                                 "\t", round(Expl_percent.4), "%\n", 
 "Others with Block as covar. Explained_intertia", round(Expl_intertia.5, 2),
                                 "\t", round(Expl_percent.5), "%\n")


#Note: the expelained interia in step 5 should be 0.14 in the book



#       Table 32.5
#       Variance decomposition table showing the effect 
#       of block and the other variables
cat(
" A Pure others 5  ", "\t", round(Expl_intertia.5, 2), "\t", 
                            round(Expl_percent.5), "%\n", 
 "B Pure Block  4  ", "\t", round(Expl_intertia.4, 2), "\t", 
                            round(Expl_percent.4), "%\n", 
 "C Shared     3-5 ", "\t", round(Expl_intertia.3-Expl_intertia.5, 2), 
                      "\t", round((Expl_intertia.3-Expl_intertia.5)/
                                   mplants_CCA.1$tot.chi*100), "%\n", 
 "D Residual       ", "\t", round(mplants_CCA.1$tot.chi-Expl_intertia.1, 2),
                      "\t", round((mplants_CCA.1$tot.chi-Expl_intertia.1)/
                                   mplants_CCA.1$tot.chi*100), "%\n", 
 "Total            ", "\t", 1, "\t", "100 %", "\n")

#Because of the mistake in Table 32.4, some of the values in the
#book are wrong




################################
#
#      African star grass



mplants<-read.table(file="MexicanPlantsTotals.txt", header=TRUE)
dim(mplants)
names(mplants)
families <- mplants[, 1:27]
expl_variables <- mplants[, 28:37]
names(families)
names(expl_variables)






#      Figure 32.6
dotchart(families$grcyn, groups = mplants$Block, 
                            labels = mplants$Block, 
                            pch = mplants$Block, 
                            col = mplants$Block, 
                            main="grcyn",
                            xlab = "Range")




#      Exclude grcyn family from the data
names(families)
names(expl_variables)
index<-names(families)!="grcyn"
new_families <- families[, index]
names(new_families)




library(vegan)
mplants_CCA.1 <- cca(new_families ~ B1+B2+B3 + as.factor(PLAGUE) +
                                    ALTITUDE + AGE + CATTLEINTENSITY + 
                                    FIELDSLOPE + BAREDSOIL + MAXVEGHEIGHT, 
                     data = expl_variables)
mplants_CCA.2 <- cca(new_families ~ B1+B2+B3,
                     data = expl_variables)
mplants_CCA.3 <- cca(new_families ~ as.factor(PLAGUE) + 
                                    ALTITUDE + AGE + CATTLEINTENSITY + 
                                    FIELDSLOPE + BAREDSOIL + MAXVEGHEIGHT, 
                     data = expl_variables)
                     
                     
                     

                     
Z<-cbind(expl_variables$PLAGUE, expl_variables$ALTITUDE,
         expl_variables$AGE,expl_variables$CATTLEINTENSITY,
         expl_variables$FIELDSLOPE, expl_variables$BAREDSOIL,
         expl_variables$MAXVEGHEIGHT)

Y<-cbind(expl_variables$B1,expl_variables$B2,expl_variables$B3)
mplants_CCA.4 <- cca(X=new_families,Y=Y,Z=Z,
                     data = expl_variables)
                     

mplants_CCA.5 <- cca(X=new_families,Y=Z, Z=Y,
                    data = expl_variables)





#      The triplot (grcyn family removed)
plot(mplants_CCA.1, scaling = 1, type = "n",
     xlab = "axis 1", ylab = "axis 2")
text(mplants_CCA.1, scaling = 1, display = "cn", head.arrow = 0,
     lwd = 2, col = "red")
points(mplants_CCA.1, scaling = 1, display = "cn", head.arrow = 0,
       lwd = 2, pch = 15, cex = 2)
text(mplants_CCA.1, scaling = 1, dis = "species", col = "green4")
text(mplants_CCA.1, scaling = 1, dis = "sites", col = "blue2",
       pch = 19, cex = 1.5)
#      Axes may be reversed due to cca algorithm in vegan package






#     Table 32.6
#     Results of the partial CCA (grcyn family removed)
Expl_intertia.1 <- sum(mplants_CCA.1$CCA$eig)
Expl_intertia.2 <- sum(mplants_CCA.2$CCA$eig)
Expl_intertia.3 <- sum(mplants_CCA.3$CCA$eig)
Expl_intertia.4 <- sum(mplants_CCA.4$CCA$eig)
Expl_intertia.5 <- sum(mplants_CCA.5$CCA$eig)

Expl_percent.1 <- sum(mplants_CCA.1$CCA$eig)/mplants_CCA.1$tot.chi*100
Expl_percent.2 <- sum(mplants_CCA.2$CCA$eig)/mplants_CCA.2$tot.chi*100
Expl_percent.3 <- sum(mplants_CCA.3$CCA$eig)/mplants_CCA.3$tot.chi*100
Expl_percent.4 <- sum(mplants_CCA.4$CCA$eig)/mplants_CCA.4$tot.chi*100
Expl_percent.5 <- sum(mplants_CCA.5$CCA$eig)/mplants_CCA.5$tot.chi*100

cat(
" Block_and_others            Explained_intertia", round(Expl_intertia.1, 2),
                                 "\t", round(Expl_percent.1), "%\n", 
 "Blocks                      Explained_intertia", round(Expl_intertia.2, 2),
                                 "\t", round(Expl_percent.2), "%\n", 
 "Others                      Explained_intertia", round(Expl_intertia.3, 2),
                                 "\t", round(Expl_percent.3), "%\n", 
 "Block with oth. as covar.   Explained_intertia", round(Expl_intertia.4, 2),
                                 "\t", round(Expl_percent.4), "%\n", 
 "Others with Block as covar. Explained_intertia", round(Expl_intertia.5, 2),
                                 "\t", round(Expl_percent.5), "%\n")





#       Table 32.7
#       Variance decomposition table showing the effect 
#       of block and the other variables (grcyn family removed)
cat(
" A Pure others 5  ", "\t", round(Expl_intertia.5, 2), "\t", 
                            round(Expl_percent.5), "%\n", 
 "B Pure Block  4  ", "\t", round(Expl_intertia.4, 2), "\t", 
                            round(Expl_percent.4), "%\n", 
 "C Shared     3-5 ", "\t", round(Expl_intertia.3-Expl_intertia.5, 2), 
                      "\t", round((Expl_intertia.3-Expl_intertia.5)/
                                   mplants_CCA.1$tot.chi*100), "%\n", 
 "D Residual       ", "\t", round(mplants_CCA.1$tot.chi-Expl_intertia.1, 2),
                      "\t", round((mplants_CCA.1$tot.chi-Expl_intertia.1)/
                                   mplants_CCA.1$tot.chi*100), "%\n", 
 "Total            ", "\t", 1, "\t", "100 %", "\n")


#Differences are due to rounding




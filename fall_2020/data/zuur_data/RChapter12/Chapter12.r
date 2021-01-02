#    R code for: Chapter 12 in:
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



setwd("D:\\applicat\\HighlandStatistics\\Book\\R\\RChapter12")

library(MASS) 

par(mar = c(4.5, 4.5, 0.5, 0.5), cex.lab = 1.5, cex.axis = 1.5)


################################
#
#        Chapter 12.2


RIKZ <- read.table(file = "RIKZ.txt",header = TRUE)
RIKZ$Richness <- rowSums(RIKZ[,2:76] > 0)
RIKZ_norm<-data.frame(NAP_norm=(RIKZ$NAP-mean(RIKZ$NAP))/sd(RIKZ$NAP),
           Rich_norm=(RIKZ$Richness-mean(RIKZ$Richness))/sd(RIKZ$Richness))




#      Figure 12.1
par(mfrow=c(2,2))
#      A
plot(RIKZ$NAP,RIKZ$Richness, ylab = "Richness", xlab = "NAP", cex=1.5)

#      B
plot(RIKZ_norm, xlim=c(-3.5, 3.5), ylim=c(-3.5,3.5), cex=1.5,
     xlab="Normalised NAP", ylab="Normalised Richness")
lines(c(-4,4),c(-4,4), lty=2)
lines(c(-4,4),c(4,-4), lty=2)

#      C
RIKZ_PCA<-princomp(RIKZ_norm)
plot(RIKZ_PCA$scores, xlim=c(-3.5, 3.5), ylim=c(-3.5,3.5), cex=1.5,
     xlab="Axis 1", ylab="Axis2")

#      D
RIKZ_PCA<-princomp(RIKZ_norm)
plot(RIKZ_PCA$scores[,1],  rep(1, times=nrow(RIKZ_PCA$scores)), axes=F, 
     xlab="Axis 1", ylab=" ")
axis(1)
par(mfrow=c(1,1))




#     PCA output (page 195) 
#     For percent of information see "proportion of variance".
#     First axis is denoted as "Comp.1"
summary(RIKZ_PCA)

#Importance of components:
#                          Comp.1    Comp.2
#Standard deviation     1.2388718 0.6486541
#Proportion of Variance 0.7848426 0.2151574
#Cumulative Proportion  0.7848426 1.0000000

#The 76% in the book should probably be 78%. (page 196, fourth line)



#    Matrix C which gives coefficients for equation of new axes
#    (page 195-196)
RIKZ_PCA<-princomp(RIKZ_norm)
RIKZ_PCA$loadings

#      coefficients for first axis (first column in C matrix)
RIKZ_PCA$loadings[,1]

#     coefficients for second axis (second column in C matrix)
RIKZ_PCA$loadings[,2]





###################################
#
#     Sections 12.4-12.5. Example of PCA

sparrows<-read.table("Sparrows2.txt", header=T)


#     Table 12.1
round(cor(sparrows),2)





#     Loadings for the first two axes
#     (the variables are normalized)
spar_PCA<-princomp(sparrows[,1:7], cor=TRUE)
#     first axis
round(spar_PCA$loadings[,1],2)
#     second axis
round(spar_PCA$loadings[,2],2)


#Or a bit more clumsy:
#     Normalisation of variables
#    (scale into zero mean and unit variance).
#    this data transformation is done by
#    "standardize" option of decostand function
library(vegan)
sparrows_norm<-decostand(sparrows[,1:7], "standardize")
spar_PCA<-princomp(sparrows_norm)





#     Figure 12.2
spar_PCA<-princomp(sparrows_norm)
plot(spar_PCA$scores[,1:2], xlab="axis 1", ylab="axis 2")



#     Table 12.2
CumProp <- 0
for (i in 1:4) {
cat(paste("Axis", i, sep = ""), "\t", 
    "EigScaled", round(spar_PCA$sdev[i]^2/sum(spar_PCA$sdev^2), 3), "\t",
    "Eig", round(spar_PCA$sdev[i]^2, 3), "\t",  
    "CumProp%", round(CumProp <- CumProp+spar_PCA$sdev[i]^2/ 
                     sum(spar_PCA$sdev^2)*100, 2), "\n")
}





#      Broken stick  model approach (page 199, formula 12.3)
p<-ncol(sparrows_norm)
L<-matrix(ncol=p)
for (i in 1:p) {
L[i]<-round(1/p*sum(1/seq(from=i, to=p)),2)
}
#      Broken stick values
L
#      Based on the broken stick model only the first axis 
#      is of interest, because it's scaled eigenvalue (0.64) is 
#      greater than corresponding brocken stick value (0.37)





#      Figure 12.3
plot(spar_PCA$sdev^2/sum(spar_PCA$sdev^2), type="h", lwd=3, 
     xlab="Axes", ylab="Scaled Eigenvalues")

#or as barplot of unscaled eigenvalues
screeplot(spar_PCA, main=NULL)





#     Figure 12.4. The correlation biplot.
biplot(spar_PCA, scale=1, cex=c(0.4, 1), arrow.len=0, main="Correlation biplot")
abline(h=0, lty=2)
abline(v=0, lty=2)

#Figures 12.4 and 12.5 were produced with Brodgar. Just like CANOCO,
#Brodgar uses a slighly different way to present the loadings (it
#scales them between -1 and 1).

win.graph()
#     Figure 12.5. The distance biplot.
biplot(spar_PCA, scale=0, cex=c(0.4, 1), arrow.len=0, main="Distance biplot")
abline(h=0, lty=2)
abline(v=0, lty=2)

#You may want to check the help file of biplot.princomp.
#Seems the scale is just the opposite of the alpha in the book.
#R help file text:
# scale 	The variables are scaled by lambda ^ scale and the
#         observations are scaled by lambda ^ (1-scale) where
#         lambda are the singular values as computed by princomp.
#         Normally 0 <= scale <= 1, and a warning will be issued
#         if the specified scale is outside this range.

#Yes...page 201: G is for the rows (observations). So...alpha = 1-scale






#######################################
#
#        Chapter 12.6. General remarks.
library(vegan)

BirdData<-read.table("BirdsDayHour.txt", header=T)
Birds <- BirdData[, 4:73]


#      Put variables (cells) in columns
#      Put observations (day-in-hour) in rows
Birds.day.by.cell <- t(Birds)


#Remove cells with less than 10 observations
I1 <- colSums(Birds.day.by.cell)
Birds.day.by.cell.M10 <- Birds.day.by.cell[,I1>=10]


#The data were cubic root transformed
Birds.CR <-Birds.day.by.cell.M10 ^ (0.33333333)


#      PCA, covariance matrix used.
#      (rda function used without second matrix on 
#       explanatory variables gives PCA ordination)
M1 <- rda(Birds.CR, scale=FALSE)

#Or use:
summary(prcomp(Birds.CR))


#      Figure 12.6
#      PCA correlation biplot
plot(M1, scaling=2, type="n")
#      The labels 1, 2, 3 indicate day of observation
label<-substr(row.names(Birds.CR),2,2)
text(scores(M1, display="sites", scaling=2), labels=label)
#      Triangles represent the variales (counts on grid cells)
points(M1, scaling=2, display="species", pch=17)





#      Eigenvalues (see page 205)
CumProp <- 0
for (i in 1:4) {
cat(paste("Axis", i, sep = ""), "\t", 
    "EigScaled", round(M1$CA$eig[i]/M1$CA$tot.chi, 3), "\t",
    "Eig", round(M1$CA$eig[i], 3), "\t",
    "CumProp%", round(CumProp <- CumProp+M1$CA$eig[i]/
                     M1$CA$tot.chi*100, 2), "\n")
}








######################################
#
#     12.7 Hord and Hellingr transformation

library(vegan)


#      Chord transformation (make site's sum of squares equal to one)
bird_dat_chord<-decostand(Birds.CR, "norm", MARGIN=1)



#      PCA, covariance matrix used.
#      (rda function used without second matrix on 
#       explanatory variables gives PCA ordination)
bird_chord_PCA<-rda(bird_dat_chord, scale=F)




#      Figure 12.9   PCA correlation biplot
#      PCA distance biplot
bird_chord_PCA<-rda(bird_dat_chord, scale=F)
plot(bird_chord_PCA, scaling=2, type="n")

#      The labels 1, 2, 3 indicate day of observation
#      Triangles represent the variales (counts on grid cells)
label<-substr(row.names(Birds.CR),2,2)
text(scores(bird_chord_PCA, display="sites", scaling=2), labels=label)
points(bird_chord_PCA, scaling=2, display="species", pch=17)




#      PCA distance biplot
plot(bird_chord_PCA, scaling=1, type="n")
#      The labels 1, 2, 3 indicate day of observation
label<-substr(row.names(Birds.CR),2,2)
text(scores(bird_chord_PCA, display="sites", scaling=1), labels=label)
#      Triangles represent the variales (counts on grid cells)
points(bird_chord_PCA, scaling=1, display="species", pch=17)





######################################
#
#     12.8 Explanatory variables

library(vegan)


RIKZ <- read.table("RIKZGroups.txt", header = TRUE)
Species <- RIKZ[,2:5]


#Data were square root transformed
Species.sq <- sqrt(Species)
ExplVar <- RIKZ[, 7:17]

#ExplVar <- RIKZ[, c("angle1","exposure","salinity",
#                    "temperature","NAP","penetrability",
#                    "grainsize","humus","chalk",
#                    "sorting1")]
##I think I am using a slightly different set of covariates in the book
#Hence, there will be small differences



#       Figure 12.10 A
RIKZ_PCA_A<-rda(Species.sq, scale=T)
plot(RIKZ_PCA_A, scaling=2, type="n")
segments(x0=0,
         y0=0, 
         x1=scores(RIKZ_PCA_A, display="species", scaling=2)[,1],
         y1=scores(RIKZ_PCA_A, display="species", scaling=2)[,2])
text(RIKZ_PCA_A, display="sp", scaling=2, col=2)
points(RIKZ_PCA_A, display="wa", scaling=2, pch=19)


#Eigenvalues (see page 208)
CumProp <- 0
for (i in 1:4) {
cat(paste("Axis", i, sep = ""), "\t", 
    "EigScaled", round(RIKZ_PCA_A$CA$eig[i]/RIKZ_PCA_A$CA$tot.chi, 3), "\t",
    "Eig", round(RIKZ_PCA_A$CA$eig[i], 3), "\t",  
    "CumProp%", round(CumProp <- CumProp+RIKZ_PCA_A$CA$eig[i]/ 
                     RIKZ_PCA_A$CA$tot.chi*100, 2), "\n")
}





#Figure 12.10 B
RIKZ_PCA_B<-rda(cbind(Species.sq, ExplVar), scale=T)
plot(RIKZ_PCA_B, scaling=2, type="n")
segments(x0=0,
         y0=0, 
         x1=scores(RIKZ_PCA_B, display="species", scaling=2)[,1],
         y1=scores(RIKZ_PCA_B, display="species", scaling=2)[,2])
text(RIKZ_PCA_B, display="sp", scaling=2, col=2)
points(RIKZ_PCA_B, display="wa", scaling=2, pch=19)


#      Eigenvalues (see page 209)
CumProp <- 0
for (i in 1:4) {
cat(paste("Axis", i, sep = ""), "\t", 
    "EigScaled", round(RIKZ_PCA_B$CA$eig[i]/RIKZ_PCA_B$CA$tot.chi, 3), "\t",
    "Eig", round(RIKZ_PCA_B$CA$eig[i], 3), "\t",  
    "CumProp%", round(CumProp <- CumProp+RIKZ_PCA_B$CA$eig[i]/ 
                     RIKZ_PCA_B$CA$tot.chi*100, 2), "\n")
}

#The eigenvalue of the second axis is smaller than in the book.
#I think I didn't use angle1 and angle 2 in the book.





#       Figure 12.10 C
RIKZ_PCA_A<-rda(Species.sq, scale=T)
#      Correlation of explanatory variables with ordination axes
crosscor<-cor(ExplVar, scores(RIKZ_PCA_A, display="wa")[,1:2])

plot(RIKZ_PCA_A, scaling=2, type="n")
segments(x0=0,
         y0=0, 
         x1=scores(RIKZ_PCA_A, display="species", scaling=2)[,1],
         y1=scores(RIKZ_PCA_A, display="species", scaling=2)[,2])
text(RIKZ_PCA_A, display="sp", scaling=2, col=2)
segments(x0=0,
         y0=0, 
         x1=crosscor[,1],
         y1=crosscor[,2])
text(crosscor, labels=row.names(crosscor), col=3)
points(RIKZ_PCA_A, display="wa", scaling=2, pch=19)




#       Figure 12.10 D (it's Figure C zoomed in)
RIKZ_PCA_A<-rda(Species.sq, scale=T)
#      Correlation of explanatory variables with ordination axes
crosscor<-cor(ExplVar, scores(RIKZ_PCA_A, display="wa")[,1:2])
crosscor

plot(RIKZ_PCA_A, scaling=2, type="n", xlim=c(-1.5, 1.5), ylim=c(-1.5, 1.5))
segments(x0=0,
         y0=0, 
         x1=scores(RIKZ_PCA_A, display="species", scaling=2)[,1],
         y1=scores(RIKZ_PCA_A, display="species", scaling=2)[,2])
text(RIKZ_PCA_A, display="sp", scaling=2, col=2)
segments(x0=0,
         y0=0, 
         x1=crosscor[,1],
         y1=crosscor[,2])
text(crosscor, labels=row.names(crosscor), col=3)
points(RIKZ_PCA_A, display="wa", scaling=2, pch=19)




######################################
#
#     12.9 Redundancy analysis

library(vegan)

I1 <- rowSums(Species)  #Could be used to drop sites with a total
                        #of 0. Just in case you want to apply one
                        #of these db-RDA transformation
                        
ExplVar <- RIKZ[, c("angle1","exposure","salinity",
                    "temperature","NAP","penetrability",
                    "grainsize","humus","chalk",
                    "sorting1")]


RIKZ_RDA<-rda(Species.sq, ExplVar, scale=T)



#     Figure 12.11
plot(RIKZ_RDA, scaling=2, type="n")
segments(x0=0,
         y0=0, 
         x1=scores(RIKZ_RDA, display="species", scaling=2)[,1],
         y1=scores(RIKZ_RDA, display="species", scaling=2)[,2])
text(RIKZ_RDA, display="sp", scaling=2, col=2)
text(RIKZ_RDA, display="bp", scaling=2,
     row.names(scores(RIKZ_RDA, display="bp")), col=3)
points(RIKZ_RDA, display="lc", scaling=2, pch=19)



#      Table 12.3
CumProp <- 0
CumProp2 <-0
for (i in 1:4) {
cat(paste("Ax", i, sep = ""), "\t", 
    "Eig", round(RIKZ_RDA$CCA$eig[i]/RIKZ_RDA$tot.chi, 2), "\t",
    "Eig%", round(RIKZ_RDA$CCA$eig[i]/RIKZ_RDA$tot.chi*100), "\t",  
    "Cum", round(CumProp <- CumProp+RIKZ_RDA$CCA$eig[i]/ 
                     RIKZ_RDA$tot.chi*100), "\t",
    "CanonSum%", round(RIKZ_RDA$CCA$eig[i]/RIKZ_RDA$CCA$tot.chi*100), "\t",
    "CumCanSum%", round(CumProp2 <- CumProp2+RIKZ_RDA$CCA$eig[i]/ 
                     RIKZ_RDA$CCA$tot.chi*100), "\n")
}
#      If the total variance is set to 1, then 
#      the sum of all canonical eigenvalues is equal to
round(RIKZ_RDA$CCA$tot.chi/RIKZ_RDA$tot.chi,2)

#Differences with the book are due to rounding





######################################
#
#     Nominal variables (page 214)


ExplVar <- RIKZ[, c("week","angle1","exposure","salinity",
                    "temperature","NAP","penetrability",
                    "grainsize","humus","chalk",
                    "sorting1")]

RIKZ_RDA<-rda(Species.sq~as.factor(week)+angle1+exposure+salinity+
                      temperature+NAP+penetrability+grainsize+humus+
                      chalk+sorting1,
              data=ExplVar,scale=T)
              
              
              

plot(RIKZ_RDA, scaling=2)
#Sorry.....but R is making a mess of these figures.
#Better use Brodgar..:-)




#I'm not sure whether vegan does a forward selection
#as described in Table 12.5 and 12.6. Certainly
step(RIKZ_RDA)
#is wrong. It doesn't have an AIC.

#For the permutation test, see the help file of:
anova.cca



#For the partial analysis, you can use:

X1 <- RIKZ[, c("angle1","salinity",
                    "temperature","NAP","penetrability",
                    "grainsize","humus","chalk",
                    "sorting1")]
X2 <- RIKZ[,"exposure"]


RIKZ_RDA1<-rda(Species.sq, X1, X2, scale=T)
RIKZ_RDA2<-rda(Species.sq, X2, X1, scale=T)

summary(RIKZ_RDA1)
summary(RIKZ_RDA2)


#Partitioning of correlations:
#              Inertia Proportion
#Total          4.0000     1.0000
#Conditioned    0.7138     0.1784
#Constrained    0.7865     0.1966
#Unconstrained  2.4997     0.6249
#

#Partitioning of correlations:
#              Inertia Proportion
#Total          4.0000    1.00000
#Conditioned    1.2030    0.30075
#Constrained    0.2973    0.07433
#Unconstrained  2.4997    0.62492

#If you look at the proportions, then the numbers are the same as
#Tables 12.7 and 12.8







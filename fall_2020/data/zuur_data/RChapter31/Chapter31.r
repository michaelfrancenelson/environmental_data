#    R code for: Chapter 31 in:
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



setwd("D:\\applicat\\HighlandStatistics\\Book\\R\\RChapter31\\")

savanna <- read.table("savanna.txt", header = T, sep = "\t", row.names = 1)






#       Figure 31.4 
#       Plot species
par(mar = c(4.5, 4.5, 2.5, 0.5), cex.lab = 1.2, cex.axis = 1.2, cex = 1)
par(ask = T)
for (i in 1:16) {
     dotchart(savanna[, i], main = colnames(savanna)[i], 
     ylab = "Sample", xlab = "Range") 
}
par(ask = F)





#     Cleveland dotplots for specis showed that a data 
#     transformation is needed, as there are various extreme 
#     observations.
#     It was decided to apply a square root transformation 
#     on the species.
species <- sqrt(savanna[, 1:16])
#     Square root transformation improved the linear relationship 
#     between species and untransformed satellite variables
cor(species, savanna[, 18:29])
max(cor(species, savanna[, 18:29]))
min(cor(species, savanna[, 18:29]))





#       Plot regeneration explanatory variables 
#       (based on vegetation parameters)
par(mar = c(4.5, 4.5, 2.5, 0.5), cex.lab = 1.5, cex.axis = 1.5, cex = 1.2)
par(ask = T)
for (i in 30:32) {
     dotchart(savanna[, i], main = colnames(savanna)[i], 
     ylab = "Sample", xlab = "Range", groups = savanna$strata, 
     color = savanna$strata + 1, pch = rev(savanna$strata) + 1)
     axis(2, at = c(5, 24), labels = c("1", "0"))
}
par(ask = F)





#       The dotplots for regeneration explanatory variables 
#       showed that a transformation of these variables is required 
#       because they have a few samples with considerable larger 
#       values than the rest.
#       It was decided to apply log(X + 1) transformation 
regeneration <- log(savanna[, 30:32] + 1)





#       Figure 31.5 
#       Plot satellite explanatory variables
par(mar = c(4.5, 4.5, 2.5, 0.5), cex.lab = 1.5, cex.axis = 1.5, cex = 1.2)
par(ask = T)
for (i in 18:29) {
     dotchart(savanna[, i], main = colnames(savanna)[i], 
     ylab = "Sample", xlab = "Range", groups = savanna$strata, 
     color = savanna$strata + 1, pch = rev(savanna$strata) + 1)
     axis(2, at = c(5, 24), labels = c("1", "0"))
}
par(ask = F)
#      Cleveland dotplots indicated that there are a few variables
#      with strata effect (most obvious is greenness and SAVI).
#      Nevertheless it was decided not to remove strata effect, 
#      because it may be important. 





#      Figure 31.6
source("MyLibrary.r")
pairs(savanna[, 18:29], upper.panel = panel.smooth2, lower.panel = panel.cor)
#      The correlations here are not absolute as it was in the book;
#      you can change this default in library "MyLibrary.r".
#      Scatterplot between sattelite variables indicates serious 
#      collinearity. 
#      Leave for a while band1, band2, brightness, greeness, 
#      wetness, ratio72, savi.
satellite <- savanna[, c(17:19, 24:28)]
pairs(satellite[, 2:8], upper.panel = panel.smooth2, lower.panel = panel.cor)





#      Figure 31.7. Correlation biplot
satellite_PCA <- princomp(satellite, cor = T)
#      to flip picture
satellite_PCA$loadings[, 2] <- satellite_PCA$loadings[, 2]*(-1)
satellite_PCA$scores[, 2] <- satellite_PCA$scores[, 2]*(-1)
par(mar = c(4.5, 4.5, 2.5, 2.5))
biplot(satellite_PCA, scale = 0, arrow.len = 0, cex = c(1, 1.5))
abline(h = 0, lty = 2)
abline(v = 0, lty = 2)

#Note: we multiplied the loadings and scores to get the same picture as
#in the book. But this is not required or recommended.





#      Eigenvalues corresopnding to principal component axes and 
#      cumulative variation of the data explained by axes
#      (eigenvalues are scaled to have sum of one)
CumVar <- 0
for (i in 1:8) {
cat(paste("Axis", i, sep = ""), "\t", 
    "scaled_eig", 
    round((satellite_PCA$sdev[i]^2)/sum(satellite_PCA$sdev^2), 2), "\t",  
    "CumVar", 
    round(CumVar <- CumVar + 
                    satellite_PCA$sdev[i]^2/sum(satellite_PCA$sdev^2), 2), 
    "\n")
}

#      As PCA showed some collinearity,  
#      it was decided to leave for analysis these variables:
#      strata, band1, brightness, greenness and wetness
satellite <- savanna[ , c(17, 18, 24, 25, 26)]






#      An alternative way to select set of explanatory variables is
#      to use VIF values (Chapter 26)
library(faraway)
sort(vif(savanna[, 18:29]))





##################################################
#
#       Multivatiate analysis
#       Speies vs. satellite based expl. variables

savanna <- read.table("savanna.txt", header = T, sep = "\t", row.names = 1)
species <- sqrt(savanna[, 1:16])
satellite <- savanna[ , c(17, 18, 24, 25, 26)]




#       Which method to use: RDA or CCA?
library(vegan)
decorana(species)
#       Axis length shows that we can use RDA





#      Figure 31.8 
library(vegan)
savanna_RDA <- rda(species, satellite, scale = F)
plot(savanna_RDA, scaling = 2)






# Eigenvalues corresponding to constrained and unconstrained axes 
# (eigenvalues are scaled to have sum of one)

total <- savanna_RDA$tot.chi #sum of eigenvalues (total variation)
constr <- savanna_RDA$CCA$eig #eigenvalues of constrained axes
unconstr <- savanna_RDA$CA$eig  #eigenvalues of unconstrained axes

TVProp <- 0
CVProp <- 0
for (i in 1:length(constr)) {
cat(paste("Constr_ax_", i, sep = ""), "\t", 
    "Scaled_eig", round(constr[i]/sum(constr)*100, 2), "%", "\t",  
    "CnstrProp", round(CVProp <- CVProp + constr[i]/sum(constr)*100), "%", "\t",
    "TotalProp", round(TVProp <- TVProp + constr[i]/total*100), "%", 
    "\n")
}
#      All five explanatory variables (the four satellite variables 
#      and strata) explain 35% of the variation in the species data
#      (TotalProp). 
#      The first two constrained axes explain 28% of the 
#      total variation in the species data (see TotalProp).
#      This is 53.6% on axis 1 and 17.9 on axis 2 (Scaled_eig), 
#      this makes in sum 81% (CnstrProp). 






#     Table 31.4. Permutation test.
library(vegan)
savanna_RDA <- rda(species, satellite, scale = F)
library(packfor)
satellite.sel <- forward.sel(species, satellite, nperm = 1000, alpha = 1)
satellite.sel[, c(1, 6, 7)]
#     The results indicate that brightness is significantly 
#     related to the species data and there are a weak strata effect.





#     Additive model. Figure 31.9
library(mgcv)
par(mfrow = c(2, 4))
result <- matrix(ncol = 3, nrow = ncol(species))
result <- as.data.frame(result)
colnames(result)[1] <- "SpecName"
colnames(result)[2] <- "Brightness_pval"
colnames(result)[3] <- "Strata_pval"
for (i in 1:ncol(species)) {
admodel <- gam(species[, i] ~ s(brightness, k = 5) + factor(strata), 
               data = satellite)
result[i, 1] <- colnames(species)[i]
result[i, 2] <- round(anova(admodel)$s.table[4], 3)
result[i, 3] <- round(anova(admodel)$p.table[2, 4], 3)
if (result[i, 2]<0.05) plot(admodel, main = colnames(species)[i])
}
par(mfrow = c(1, 1))
result
#    Results indicated that brightness was significantly related 
#    to seven species: Bomcos, Corpin, Danoli, Pteeri, Sclbir, Termac, 
#    Xerstu. Strata was significantly related to Proafr and Termac.






##################################################
#
#       Multivatiate analysis
#       Speies vs. onsite explanatory variables

savanna <- read.table("savanna.txt", header = T, sep = "\t", row.names = 1)
species <- sqrt(savanna[, 1:16])
regeneration <- log(savanna[, 30:32] + 1)





#      Figure 31.10 
library(vegan)
savanna_RDA.2 <- rda(species, regeneration, scale = F)
plot(savanna_RDA.2, scaling = 2)
#  The figure may be flipped
#Note that it may be better to rescale the scores and loadings so that
#the figure looks better (as in the book). Brodgar follows CANOCO.
#To do this yourself: extract all loadings and scores of the first two axes,
#rescale each of them so that they fit in the -1 -1 range. You should not
#change the relation between the first and second axes! For example,
#the scores of the first two axes can be multiplied (or divided) by a certain
#number, the loadings of the first two axes by another number, etc.




# Eigenvalues corresponding to constrained and unconstrained axes 
# (eigenvalues are scaled to have sum of one)

total <- savanna_RDA.2$tot.chi #sum of eigenvalues (total variation)
constr <- savanna_RDA.2$CCA$eig #eigenvalues of constrained axes
unconstr <- savanna_RDA.2$CA$eig  #eigenvalues of unconstrained axes

TVProp <- 0
CVProp <- 0
for (i in 1:length(constr)) {
cat(paste("Constr_ax_", i, sep = ""), "\t", 
    "Scaled_eig", round(constr[i]/sum(constr)*100, 2), "%", "\t",  
    "CnstrProp", round(CVProp <- CVProp + constr[i]/sum(constr)*100), "%", "\t",
    "TotalProp", round(TVProp <- TVProp + constr[i]/total*100), "%", 
    "\n")
}
#      All five explanatory variables (the four satellite variables 
#      and strata) explain 24% of the variation in the species data
#      (TotalProp). 
#      The first two constrained axes explain 21% of the 
#      total variation in the species data (see TotalProp).
#      This is 73.51% on axis 1 and 15.15 on axis 2 (Scaled_eig), 
#      this makes in sum 89% (CnstrProp). 






#     Permutation test.
library(vegan)
savanna_RDA.2 <- rda(species, regeneration, scale = F)
library(packfor)
regeneration.sel <- forward.sel(species, regeneration, nperm = 1000, alpha = 1)
regeneration.sel[, c(1, 6, 7)]
#     The results indicate that Cl2 is significantly related to the 
#     species data. Other explanatory variables were not significant 
#     at the 5% level





#     Additive models. 
library(mgcv)
par(mfrow = c(1, 3))
result.2 <- matrix(ncol = 2, nrow = ncol(species))
result.2 <- as.data.frame(result.2)
colnames(result.2)[1] <- "SpecName"
colnames(result.2)[2] <- "Cl2_pval"
for (i in 1:ncol(species)) {
admodel.2 <- gam(species[, i] ~ s(Cl2, k = 5), data = regeneration)
result.2[i, 1] <- colnames(species)[i]
result.2[i, 2] <- round(anova(admodel.2)$s.table[4], 3)
if (result.2[i, 2]<0.05) plot(admodel.2, main = colnames(species)[i])
}
par(mfrow = c(1, 1))
result.2
#    Results indicated that Cl2 was significantly related 
#    to the species: Comnig, Danoli, Pteeri.






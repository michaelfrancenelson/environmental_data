#    R code for: Chapter 13 in:
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





setwd("D:\\applicat\\HighlandStatistics\\Book\\R\\RChapter13\\")

#      Figure 13.1 A
plot(c(10, 30), c(0, 100), type = "n", xlab = "Temperature", ylab = "Y")
art_example<-read.table("artificial_example.txt", header = T, sep = "\t")
points(art_example, pch = 19)





#      Figure 13.1 B
#      Gaussian response curve
c<-100
u<-20
t<-2
x_new<-seq(from = 10, to = 30, by = 0.2)
y_new<-c*exp(-((x_new-u)^2)/(2*t^2))
plot(c(10, 30), c(0, 100), type = "n", xlab = "Temperature", ylab = "Y")
lines(x_new, y_new)
art_example<-read.table("artificial_example.txt", header = T, sep = "\t")
points(art_example, pch = 19)





#      Fitting gaussian curve to the example data
art_example<-read.table("artificial_example.txt", header = T, sep = "\t")
gaus_model<-glm(log(y) ~ poly(x, 2), data = art_example, family = gaussian)

plot(c(10, 30), c(0, 100), type = "n", xlab = "Temperature", ylab = "Y")
x_new<-seq(from = 10, to = 30, by = 0.2)
y_new<-c*exp(-((x_new-u)^2)/(2*t^2))
lines(x_new, exp(predict(gaus_model, newdata = data.frame(x = x_new))),
      col = "red")
points(art_example, pch = 19)
lines(x_new, y_new)

b1<-gaus_model$coefficients[1]
b2<-gaus_model$coefficients[2]
b3<-gaus_model$coefficients[3]

t_new<-1/sqrt(-2*b3)
u_new<-(-b2)/(2*b3)
c_new<-exp(b1-(b2^2)/(4*b3))

cat(
" Tolerance t        ", t_new, "\n",
 "Optimum u         ", u_new, "\n",
 "Maximum value c   ", c_new, "\n")






#       Figure 13.5
#       axes may be reversed due to the algorithm
#       of the cca-function in vegan package
mplants<-read.table("avg_MexicanPlants.txt", header = T, sep = "\t")
#In the book, we used a data file with a slightly different
#row order. This means that the row labels are slightly different
#(e.g.site 3 in the book is site 5 here)
#But results are identical.

index<-rep(T, times = 15)
index<-names(mplants) == "grcyn"|names(mplants) == "com"|
       names(mplants) == "vi"|names(mplants) == "co"|
       names(mplants) == "le"|names(mplants) == "ma"|
       names(mplants) == "as"|names(mplants) == "grresto"|
       names(mplants) == "eu"|names(mplants) == "ru"|
       names(mplants) == "ac"|names(mplants) == "cy"|
       names(mplants) == "la"|names(mplants) == "vit"|
       names(mplants) == "so"
species<-mplants[, index]
library(vegan)
mplants_CA<-cca(species)
plot(mplants_CA, scaling = 1, cex = 2, main = "Scaling = 1")

#      the total variation
round(mplants_CA$CA$tot.chi, 2)

#       the first two eigenvalues
round(mplants_CA$CA$eig[1:2], 2)






#       Figure 13.6
#       axes may be reversed due to the algorithm
#       of the cca-function in vegan package
mplants<-read.table("avg_MexicanPlants.txt", header = T, sep = "\t")
index<-rep(T, times = 15)
index<-names(mplants) == "grcyn"|names(mplants) == "com"|
       names(mplants) == "vi"|names(mplants) == "co"|
       names(mplants) == "le"|names(mplants) == "ma"|
       names(mplants) == "as"|names(mplants) == "grresto"|
       names(mplants) == "eu"|names(mplants) == "ru"|
       names(mplants) == "ac"|names(mplants) == "cy"|
       names(mplants) == "la"|names(mplants) == "vit"|
       names(mplants) == "so"
species<-mplants[, index]


library(vegan)
mplants_CA<-cca(species)
plot(mplants_CA, scaling = 2, main = "Scaling = 2")

#      the total variation
round(mplants_CA$CA$tot.chi, 2)

#       the first two eigenvalues
round(mplants_CA$CA$eig[1:2], 2)






#       Figure 13.6
#       axes may be reversed due to algorithm
#       of the cca-function in vegan package
mplants<-read.table("avg_MexicanPlants.txt", header = T, sep = "\t")
index<-rep(T, times = 15)
index<-names(mplants) == "grcyn"|names(mplants) == "com"|
       names(mplants) == "vi"|names(mplants) == "co"|
       names(mplants) == "le"|names(mplants) == "ma"|
       names(mplants) == "as"|names(mplants) == "grresto"|
       names(mplants) == "eu"|names(mplants) == "ru"|
       names(mplants) == "ac"|names(mplants) == "cy"|
       names(mplants) == "la"|names(mplants) == "vit"|
       names(mplants) == "so"
species<-mplants[, index]
library(vegan)
mplants_CA<-cca(species)
plot(mplants_CA, scaling = 3, main = "Scaling = 3")

#      the total variation
round(mplants_CA$CA$tot.chi, 2)

#       the first two eigenvalues
round(mplants_CA$CA$eig[1:2], 2)

#       the first two axes explain
cat(round(sum(mplants_CA$CA$eig[1:2])/mplants_CA$CA$tot.chi*100), "%", "\n")
#       of the variation






#       Figure 13.8
#       axes may be reversed due to the algorithm
#       of the cca-function in vegan package
#In the book, we used a data file with a slightly different
#row order. This means that the row labels are slightly different
#(e.g.site 3 in the book is site 5 here)
#But results are identical.

mplants<-read.table("avg_MexicanPlants.txt", header = T, sep = "\t")
index_spec<-names(mplants) == "grcyn"|names(mplants) == "com"|
            names(mplants) == "vi"|names(mplants) == "co"|
            names(mplants) == "le"|names(mplants) == "ma"|
            names(mplants) == "as"|names(mplants) == "grresto"|
            names(mplants) == "eu"|names(mplants) == "ru"|
            names(mplants) == "ac"|names(mplants) == "cy"|
            names(mplants) == "la"|names(mplants) == "vit"|
            names(mplants) == "so"
species<-mplants[, index_spec]
index_env<-names(mplants) == "MAXVEGHEIGHT"|names(mplants) == "AGE"|
           names(mplants) == "ALTITUDE"|names(mplants) == "BAREDSOIL"
env_var<-mplants[, index_env]
library(vegan)
mplants_CCA<-cca(species, env_var)
plot(mplants_CCA, scaling = 2)

#      the total variation in the data is:
round(mplants_CCA$tot.chi, 2)

#       the sum of all canonical eigenvalues:
round(mplants_CCA$CCA$tot.chi, 2)

#       all four explanatory variables explain
cat(round(mplants_CCA$CCA$tot.chi/mplants_CCA$tot.chi*100), "% of data", "\n")
#      of the total variation in the data

#       the first two (canonical) eigenvalues are:
round(mplants_CCA$CCA$eig[1:2], 2)

#       so the first two canonical axes explain:
cat(round(sum(mplants_CCA$CCA$eig[1:2])/mplants_CCA$CCA$tot.chi*100), "%", "\n")
#       of the variation that can be explained
#       with the four environmental variables

#       but this is (the first two canonical axes explain):
cat(round(sum(mplants_CCA$CCA$eig[1:2])/mplants_CCA$tot.chi*100), "%", "\n")
#       of the total variation in the data





######################################
#
#        13.6 Problems with CA and CCA



#       Figure 13.10
#       axes may be reversed due to the algorithm
#       of the cca-function in vegan package
RIKZ<-read.table("RIKZ.txt", header = T, sep = "\t")
RIKZ_species<-RIKZ[rowSums(RIKZ[, 2:76] == 0)<75, 2:76]
library(vegan)
RIKZ_CA<-cca(RIKZ_species)
plot(RIKZ_CA, scaling = 2, type = "n", xlab = "axis 1", ylab = "axis 2")
text(RIKZ_CA, scaling = 2, display = "species", cex = 2)
text(RIKZ_CA, scaling = 2, display = "site", cex = 1, col = "red")




#       Figure 31.11
#       axes may be reversed due to the algorithm
#       of the cca-function in vegan package
mex_plants<-read.table("MexicanPlants.txt", header = T, sep = "\t")
mex_plants<-mex_plants[rowSums(is.na(mex_plants)) == 0, ]
mex_species<-mex_plants[, 3:34]
mex_env_var<-mex_plants[, 38:47]
library(vegan)
mex_CCA<-cca(mex_species ~ factor(Block) + ALTITUDE + AGE +
                          CATTLEINTENSITY + FIELDSLOPE +
                          WEEDINGFRECUENCYIND + factor(PLAGUE) +
                          BAREDSOIL + MINVEGHEIGHT + MAXVEGHEIGHT,
            data = mex_env_var)
plot(mex_CCA, scaling = 1, type = "n", xlab = "axis 1", ylab = "axis 2")
points(mex_CCA, display = "site", scaling = 1, pch = 19, cex = 1.2)
text(mex_CCA, display = "species", scaling = 1)
points(mex_CCA, display = "bp", scaling = 1, pch = 19, cex = 1.2)
text(mex_CCA, display = "bp", scaling = 1, pch = 19, cex = 1.2)

#Note: Brodgar follows CANOCO, and places the loadings and scores between
#-1 and 1. R makes a mess of these triplots.

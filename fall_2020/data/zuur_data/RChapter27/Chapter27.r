#    R code for: Chapter 27 in:
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



setwd("D:\\applicat\\HighlandStatistics\\Book\\R\\RChapter27\\")




#############################
#
#     The variables. 



#      Table 27.2
RIKZ <- read.table("RIKZ.txt", header = T, sep = "\t", row.names = 1)
expl_variables <- RIKZ[, 76:87]

significance <- matrix(ncol = ncol(expl_variables), 
                       nrow = ncol(expl_variables))
for (i in 1:(ncol(expl_variables)-1)){
    for (j in (i + 1):ncol(expl_variables)) {
         significance[j, i] <- cor.test(x = expl_variables[, i], 
                                        y = expl_variables[, j])$p.value
    }
}
significance <- as.dist(round(significance, 2))

cor_matr <- as.dist(round(cor(expl_variables), 2), upper = T)
cor_matr[significance>0.05] <- NA
cor_matr





##################################################
#
#     Analysing the data using univariate methods. 


RIKZ <- read.table("RIKZascii.txt", header = TRUE)
#RIKZ <- RIKZ[rowSums(RIKZ[, 1:75])>0, ]
species <- RIKZ[, 2:76]
expl_variables <- RIKZ[, 77:88]




#      Figure 27.1
p <- species/rowSums(species)
shannon <- (-1)*rowSums(p*log10(p), na.rm = T)
shannon <- as.numeric(shannon)
par(mar = c(4.5, 4.5, 0.5, 0.5), cex.lab = 1.5)
dotchart(shannon, groups = factor(RIKZ$Beach), xlab = "Range")

#Or use:
#diversity(species, index = "shannon", MARGIN = 1, base = exp(1))
#This is the Shannon index with natural log.



#      Figure 27.2. Pairplot of some of the explanatory variables
source("MyLibrary.r")
plot(expl_variables[, c(2, 5:12)], lower.panel = panel.cor)
#      The variables angle1, humus, shalk and sorting all have an 
#      observation with a large value. This might cause problems 
#      in some statistical analyses, so a square root transformation 
#      was applied on these variables

names(expl_variables)
# [1] "week"          "angle1"        "angle2"        "exposure"
# [5] "salinity"      "temperature"   "NAP"           "penetrability"
# [9] "grainsize"     "humus"         "chalk"         "sorting1"


expl_variables[, c(2, 10:12)] <- sqrt(expl_variables[, c(2, 10:12)])





#     Pearson correlations between all variables all were smaller
#     than 0.8 in absolute sense. However some correlations were 
#     between 0.75 and 0.8. This indicates a certain degree of 
#     collinearity
plot(expl_variables, lower.panel = panel.cor)





#      Table 27.3. Angle1, humus, chalk and sorting are square root
#                  transformed

#      Corss-correlation
table_27.3 <- data.frame(Cross_correl = round(cor(expl_variables, shannon), 2))

#      Between-Beach
ex_aver <- apply(expl_variables, MARGIN = 2, FUN = function(x) 
                 tapply(x, INDEX = RIKZ$Beach, FUN = mean))
sh_aver <- tapply(shannon, INDEX = RIKZ$Beach, FUN = mean)
table_27.3$Between_Beach <- round(cor(ex_aver, as.numeric(sh_aver)), 2)

#      Within-Beach
ex_with <- apply(expl_variables, MARGIN = 2, FUN = function(x) 
                 residuals(lm(x~RIKZ$Beach)))
sh_with <- residuals(lm(shannon~RIKZ$Beach, data = RIKZ))
table_27.3$Within_beach <- round(cor(ex_with, as.numeric(sh_with)), 2)

#      For some of the variables within-beach correlation
#      could not be calculated because these explanatory variables 
#      had the same value at all five stations on a beach
is.na(table_27.3$Within_beach[c(1, 3, 4, 5, 6, 7)]) <- T
table_27.3

#I believe the Agle1 correlation is wrong in the book. Must be a typo




#################################################
#
#      Additive modelling


#       Table 27.4 Forward selection.
library(MASS)
library(mgcv)

shannon_GAM.lo <- gam(shannon~1, data = expl_variables)

AIC(update(shannon_GAM.lo,.~. + s(angle1, fx=TRUE, k=5)))
AIC(update(shannon_GAM.lo,.~. + s(angle2, fx=TRUE, k=5)))
AIC(update(shannon_GAM.lo,.~. + s(salinity, fx=TRUE, k=5)))
AIC(update(shannon_GAM.lo,.~. + s(temperature, fx=TRUE, k=5)))
AIC(update(shannon_GAM.lo,.~. + s(NAP, fx=TRUE, k=5)))
AIC(update(shannon_GAM.lo,.~. + s(penetrability, fx=TRUE, k=5)))
AIC(update(shannon_GAM.lo,.~. + s(grainsize, fx=TRUE, k=5)))
AIC(update(shannon_GAM.lo,.~. + s(humus, fx=TRUE, k=5)))
AIC(update(shannon_GAM.lo,.~. + s(chalk, fx=TRUE, k=5)))
AIC(update(shannon_GAM.lo,.~. + s(sorting1, fx=TRUE, k=5)))

#NAP + ...
AIC(update(shannon_GAM.lo,.~. + s(NAP, fx=TRUE, k=5)+ factor(week)))
AIC(update(shannon_GAM.lo,.~. + s(NAP, fx=TRUE, k=5)+ factor(exposure)))
AIC(update(shannon_GAM.lo,.~. + s(NAP, fx=TRUE, k=5)+ s(angle1, fx=TRUE, k=5)))
AIC(update(shannon_GAM.lo,.~. + s(NAP, fx=TRUE, k=5)+ s(angle2, fx=TRUE, k=5)))
AIC(update(shannon_GAM.lo,.~. + s(NAP, fx=TRUE, k=5)+ s(salinity, fx=TRUE, k=5)))
AIC(update(shannon_GAM.lo,.~. + s(NAP, fx=TRUE, k=5)+ s(temperature, fx=TRUE, k=5)))
AIC(update(shannon_GAM.lo,.~. + s(NAP, fx=TRUE, k=5)+ s(penetrability, fx=TRUE, k=5)))
AIC(update(shannon_GAM.lo,.~. + s(NAP, fx=TRUE, k=5)+ s(grainsize, fx=TRUE, k=5)))
AIC(update(shannon_GAM.lo,.~. + s(NAP, fx=TRUE, k=5)+ s(humus, fx=TRUE, k=5)))
AIC(update(shannon_GAM.lo,.~. + s(NAP, fx=TRUE, k=5)+ s(chalk, fx=TRUE, k=5)))
AIC(update(shannon_GAM.lo,.~. + s(NAP, fx=TRUE, k=5)+ s(sorting1, fx=TRUE, k=5)))


#NAP + factor(week) + ...
AIC(update(shannon_GAM.lo,.~. + s(NAP, fx=TRUE, k=5)+ factor(week)+ s(angle1, fx=TRUE, k=5)))
AIC(update(shannon_GAM.lo,.~. + s(NAP, fx=TRUE, k=5)+ factor(week)+  s(angle2, fx=TRUE, k=5)))
AIC(update(shannon_GAM.lo,.~. + s(NAP, fx=TRUE, k=5)+ factor(week)+  factor(exposure)))
AIC(update(shannon_GAM.lo,.~. + s(NAP, fx=TRUE, k=5)+ factor(week)+  s(salinity, fx=TRUE, k=5)))
AIC(update(shannon_GAM.lo,.~. + s(NAP, fx=TRUE, k=5)+ factor(week)+  s(temperature, fx=TRUE, k=5)))
AIC(update(shannon_GAM.lo,.~. + s(NAP, fx=TRUE, k=5)+ factor(week)+  s(penetrability, fx=TRUE, k=5)))
AIC(update(shannon_GAM.lo,.~. + s(NAP, fx=TRUE, k=5)+ factor(week)+  s(grainsize, fx=TRUE, k=5)))
AIC(update(shannon_GAM.lo,.~. + s(NAP, fx=TRUE, k=5)+ factor(week)+  s(humus, fx=TRUE, k=5)))
AIC(update(shannon_GAM.lo,.~. + s(NAP, fx=TRUE, k=5)+ factor(week)+  s(chalk, fx=TRUE, k=5)))
AIC(update(shannon_GAM.lo,.~. + s(NAP, fx=TRUE, k=5)+ factor(week)+  s(sorting1, fx=TRUE, k=5)))

#Seems the book has a little typo for chalk.


shannon_GAM <- gam(shannon~1 + s(NAP, fx=TRUE, k=5) + factor(exposure),
                    data = expl_variables)

summary(shannon_GAM)
#Note that the R output has changed due to different mgcv versions
plot(shannon_GAM)
anova(shannon_GAM)






#######################
#
#      Regression tree


#       Figure 27.4

library(rpart)
shannon_tree <- rpart(shannon~factor(week) + factor(exposure) + angle1 + 
                            angle2 + salinity + temperature + NAP + 
                            penetrability + grainsize + humus + 
                            chalk + sorting1, data = expl_variables, cp = 0.05)
plot(shannon_tree, margin = 0.1)
text(shannon_tree, use.n = T)





###################################################
#
#      Analysing the data using multivariate methods
#      Transformation for Chord distance and square-root transformation


#We need to remove all rows with a total of zero.
SiteTotal <- rowSums(species, na.rm = TRUE)
species2 <- sqrt(species[SiteTotal>0 ,])
expl_variables2 <-expl_variables[SiteTotal>0 ,]

#Chord transformation
species_trans <- species2/sqrt(rowSums(species2^2))





#      Figure 27.5
library(vegan)
RIKZ_RDA <- rda(species_trans~angle1 + angle2 + penetrability + temperature + 
              salinity + humus + chalk + grainsize + sorting1 + NAP + 
              as.factor(week) + as.factor(exposure), 
              data = expl_variables2, scale = TRUE)
              
summary.cca(RIKZ_RDA)

plot(RIKZ_RDA, scaling = 1, type = "n", 
     xlab = "axis 1", ylab = "axis 2")
text(RIKZ_RDA, scaling = 1, display = "cn", head.arrow = 0, 
     lwd = 2, col = "red")
points(RIKZ_RDA, scaling = 1, display = "cn", head.arrow = 0, 
       lwd = 2, pch = 15, cex = 2)
text(RIKZ_RDA, scaling = 1, dis = "species", col = "green4")
points(RIKZ_RDA, scaling = 1, dis = "sites", col = "blue2", 
       pch = 19, cex = 1.5)
#The ordination axes can be reversed



# Eigenvalues corresponding to constrained axes 
# (eigenvalues are scaled to have sum of one)
total <- RIKZ_RDA$tot.chi #sum of eigenvalues (total variation)
constr <- RIKZ_RDA$CCA$eig #eigenvalues of constrained axes
unconstr <- RIKZ_RDA$CA$eig  #eigenvalues of unconstrained axes

TVProp <- 0
CVProp <- 0
for (i in 1:2) {
cat(paste("Constr_ax_", i, sep = ""), "\t", 
    "Scaled_eig", round(constr[i]/sum(constr)*100, 2), "%", "\t",  
    "CnstrProp", round(CVProp <- CVProp + constr[i]/sum(constr)*100), "%", "\t",
    "TotalProp", round(TVProp <- TVProp + constr[i]/total*100), "%", 
    "\n")
}
#      The first two constrained axes explain 45% of the total 
#      sum of all canonical eigenvalues (CnstrProp)
#      The first two constrained axes explain 18% of the 
#      total variation in the species data (see TotalProp).
#      
#      the total sum of all canonical eigenvalues as ratio:
round(sum(constr)/total, 2)





#     Construct new explanatory variables W1, W2, W3, W4, 
#     Expo8, Expo10 and Expo11 - nominal variables representing 
#     week and exposure levels in order to inspec their collinearity:
exposure <- expl_variables2$exposure
Expo8 <- rep(0, length = length(exposure))
Expo10 <- rep(0, length = length(exposure))
Expo11 <- rep(0, length = length(exposure))
Expo8[exposure == 8] <- 1
Expo10[exposure == 10] <- 1
Expo11[exposure == 11] <- 1

week <- expl_variables2$week
W1 <- rep(0, length = length(week))
W2 <- rep(0, length = length(week))
W3 <- rep(0, length = length(week))
W4 <- rep(0, length = length(week))
W1[week == 1] <- 1
W2[week == 2] <- 1
W3[week == 3] <- 1
W4[week == 4] <- 1

######################################################
#     Table 27.5
#     Permutation test. 
#Not sure whether R has exactly the same facilities as CANOCO/Brodgar
#You may have to do some research yourself, see the step and anova commands
#in the vegan package.
#The closest we can get is:


RIKZ_RDA1 <- rda(species_trans~W4, data = expl_variables2, scale = TRUE)
RIKZ_RDA2 <- rda(species_trans~salinity, data = expl_variables2, scale = TRUE)
RIKZ_RDA3 <- rda(species_trans~NAP, data = expl_variables2, scale = TRUE)
RIKZ_RDA4 <- rda(species_trans~humus, data = expl_variables2, scale = TRUE)
#etc

#and each time run:
anova(RIKZ_RDA1, by="terms", permu=9999)
anova(RIKZ_RDA2, by="terms", permu=9999)
anova(RIKZ_RDA3, by="terms", permu=9999)
anova(RIKZ_RDA4, by="terms", permu=9999)
#etc

#Pick the best one and continue with:
RIKZ_RDA1 <- rda(species_trans~W4 + salinity, data = expl_variables2, scale = TRUE)
RIKZ_RDA2 <- rda(species_trans~W4 + NAP, data = expl_variables2, scale = TRUE)
RIKZ_RDA3 <- rda(species_trans~w4 + humus, data = expl_variables2, scale = TRUE)
#etc
anova(RIKZ_RDA1, by="terms", permu=9999)
anova(RIKZ_RDA2, by="terms", permu=9999)
anova(RIKZ_RDA3, by="terms", permu=9999)
# etc etc
#
#You can also have a look at the step function, see:
#http://cc.oulu.fi/~jarioksa/softhelp/vegan/html/deviance.cca.html
#and
#http://cc.oulu.fi/~jarioksa/softhelp/vegan/html/anova.cca.html
#Please email us if you figure out how to do this faster

#The alternative is to use the packfor from the
#http://www.bio.umontreal.ca/legendre/indexEn.html

library(packfor)
names(expl_variables)

expl_variables.nom <- cbind(expl_variables2[, c(2, 3, 5:12)],
                          W2, W4, Expo10, Expo11)
regeneration.sel <- forward.sel(species_trans, 
                                expl_variables.nom, nperm = 1000, alpha = 1)
regeneration.sel[, c(1, 6, 7)]

#But results are not the same as CANOCO/Brodgar/R
##############################################################




#      Figure 27.6
#      Select explanatory variables: week, salinity, NAP, humus, 
#      temperature and exposure. Refit the RDA. 
expl_variables.3 <- cbind(expl_variables2[, c(1, 4, 5, 6, 7, 10)])
names(expl_variables.3)

RIKZ_RDA.2 <- rda(species_trans~temperature + salinity + humus + NAP + 
              as.factor(week) + as.factor(exposure), 
              data = expl_variables.3, scale = T)



plot(RIKZ_RDA.2, scaling = 1, type = "n",
     xlab = "axis 1", ylab = "axis 2")
text(RIKZ_RDA.2, scaling = 1, display = "cn", head.arrow = 0,
     lwd = 2, col = "red")
points(RIKZ_RDA.2, scaling = 1, display = "cn", head.arrow = 0,
       lwd = 2, pch = 15, cex = 2)
text(RIKZ_RDA.2, scaling = 1, dis = "species", col = "green4")
points(RIKZ_RDA.2, scaling = 1, dis = "sites", col = "blue2",
       pch = 19, cex = 1.5)
#      In some R versions the ordination axes can be reversed






# Eigenvalues corresponding to constrained axes 
# (eigenvalues are scaled to have sum of 1)
total <- RIKZ_RDA.2$tot.chi #sum of eigenvalues (total variation)
constr <- RIKZ_RDA.2$CCA$eig #eigenvalues of constrained axes
unconstr <- RIKZ_RDA.2$CA$eig  #eigenvalues of unconstrained axes





#     Table 27.6
#                                  Eigenvalue                axis_1    axis_2
cat("Eigenvalue", "\t", "\t", "\t", "\t", "\t", "\t", "\t", 
                  round(constr[1]/total, 2), "\t", 
                  round(constr[2]/total, 2), "\n")
cat("Eigenvalue as % of total variation", "\t", "\t", "\t", "\t", 
                  round(constr[1]/total*100), "\t",  
                  round(constr[2]/total*100), "\n")
cat("Eigenvalue as cumulative % of total variation", "\t", "\t", "\t", 
                  round(constr[1]/total*100), "\t", 
                  round(sum(constr[1:2])/total*100), "\n")
cat("Eigenvalue as % sum of all canonical eigenvalues", "\t", "\t", 
                  round(constr[1]/sum(constr)*100), "\t", 
                  round(constr[2]/sum(constr)*100), "\n")
cat("Eigenvalue as cumulative % sum of all canonical eigenvalues", "\t", 
                  round(constr[1]/sum(constr)*100), "\t", 
                  round(sum(constr[1:2])/sum(constr)*100), "\n")
 

 
 
 

#      Figure 27.7
#      Generate letters to denote beaches
site_letters <- rep("", times = nrow(species))
site_letters <- LETTERS[RIKZ$Beach]
site_letters[site_letters == "I"] <- "J"

expl_variables.3 <- cbind(expl_variables2[, c(1, 4, 5, 6, 7, 10)])
RIKZ_RDA.2 <- rda(species_trans~temperature + salinity + humus + NAP + 
              as.factor(week) + as.factor(exposure), 
              data = expl_variables.3, scale = T)
              

plot(RIKZ_RDA.2, scaling = 1, type = "n",
     xlab = "axis 1", ylab = "axis 2")
text(RIKZ_RDA.2, scaling = 1, display = "cn", head.arrow = 0,
     lwd = 2, col = "red")
points(RIKZ_RDA.2, scaling = 1, display = "cn", head.arrow = 0,
       lwd = 2, pch = 15, cex = 2)
text(RIKZ_RDA.2, scaling = 1, dis = "species", col = "green4")
points(RIKZ_RDA.2, scaling = 1, dis = "sites", col = "blue2",
       cex = 1.5, pch = site_letters)
#      In some R versions the ordination axes can be reversed




###################################
#
#      Variance partitioning in RDA

#RIKZ <- read.table("RIKZ.txt", header = T, sep = "\t", row.names = 1)
#RIKZ <- RIKZ[rowSums(RIKZ[, 1:75])>0, ]
#species <- RIKZ[, 1:75]
#expl_variables <- cbind(RIKZ[, c(76:88)])
#expl_variables[, c(2, 10:12)] <- sqrt(expl_variables[, c(2, 10:12)])



RIKZ_RDA.A <- rda(species_trans ~ as.factor(exposure), 
                  data = expl_variables2, scale = T)
RIKZ_RDA.B <- rda(species_trans ~ angle1 + angle2 + penetrability + 
                  temperature + salinity + humus + chalk + 
                  grainsize + sorting1 + NAP + as.factor(week), 
                  data = expl_variables2, scale = T)
RIKZ_RDA.C <- rda(species_trans ~ as.factor(exposure) + Condition(angle1, 
                  angle2, penetrability, temperature, salinity, humus, 
                  chalk, grainsize, sorting1, NAP, as.factor(week)), 
                  data = expl_variables2, scale = T)
RIKZ_RDA.D <- rda(species_trans ~ angle1 + angle2 + penetrability + 
                  temperature + salinity + humus + chalk + grainsize + 
                  sorting1 + NAP + as.factor(week) + 
                  Condition(as.factor(exposure)), 
                  data = expl_variables2, scale = T)
RIKZ_RDA.E <- rda(species_trans ~ angle1 + angle2 + penetrability + 
                  temperature + salinity + humus + chalk + grainsize + 
                  sorting1 + NAP + as.factor(week) + as.factor(exposure), 
                  data = expl_variables2, scale = T)





#     Table 27.7
#     Sum of all canonical eigenvalues. 
#     The total variation is scaled to 1 
A <- sum(RIKZ_RDA.A$CCA$eig)/RIKZ_RDA.A$tot.chi
B <- sum(RIKZ_RDA.B$CCA$eig)/RIKZ_RDA.B$tot.chi
C <- sum(RIKZ_RDA.C$CCA$eig)/RIKZ_RDA.C$tot.chi
D <- sum(RIKZ_RDA.D$CCA$eig)/RIKZ_RDA.D$tot.chi
E <- sum(RIKZ_RDA.E$CCA$eig)/RIKZ_RDA.E$tot.chi

round(A, 2)
round(B, 2)
round(C, 2)
round(D, 2)
round(E, 2)

#Note the typo in the book


#       Table 27.8
cat("Pure Exposure", "\t", round(C, 2), "\t", round(C*100), "%", "\n")
cat("Pure 'Others'", "\t", round(D, 2), "\t", round(D*100), "%", "\n")
cat("Shared", "\t", "\t", round(A-C, 2), "\t", round((A-C)*100), "%", "\n")
cat("Residual", "\t", round(1-E, 2), "\t", round((1-E)*100), "%", "\n")
cat("Total", "\t", "\t", 1, "\t", "100 %", "\n")

#Note the typo in the book


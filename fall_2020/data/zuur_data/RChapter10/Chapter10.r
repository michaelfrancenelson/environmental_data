#    R code for: Chapter 10 in:
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



setwd("D:\\applicat\\HighlandStatistics\\Book\\R\\RChapter10\\")

par(mar = c(4.5, 4.5, 0.5, 0.5), cex.lab = 1.1, cex.axis = 1.1)




Argentina <- read.table("Argentina.txt", header = T)



#      Table 10.1
transect <- as.data.frame(matrix(ncol = 4, nrow = 6))
names(transect) <- names(Argentina)[2:5]
for (i in 1:4) {
transect[, i] <- as.numeric(tapply(Argentina[, i + 1], 
              INDEX = list(Argentina$Transect, Argentina$Season), FUN = sum))
}
transect



#     Table 10.2
source("Ch10Library.r")

left <- 1; right <- 2
joint_table.1_2 <- cbind(c(joint_pres(transect, left, right), 
                     right_pres(transect, left, right)), 
                   c(left_pres(transect, left, right), 
                     joint_abs(transect, left, right)))

left <- 1; right <- 3
joint_table.1_3 <- cbind(c(joint_pres(transect, left, right), 
                     right_pres(transect, left, right)), 
                   c(left_pres(transect, left, right), 
                     joint_abs(transect, left, right)))

left <- 2; right <- 3
joint_table.2_3 <- cbind(c(joint_pres(transect, left, right), 
                     right_pres(transect, left, right)), 
                   c(left_pres(transect, left, right), 
                     joint_abs(transect, left, right)))

joint_table.1_2
joint_table.1_3
joint_table.2_3



###############################
#
#     Association between sites

#     Table 10.3: Left
#     SIMPLE MATCHING COEFFICIENT for presence-absence data
source("Ch10Library.r")
table_10.3 <- association(transect, "match")
table_10.3
#     The smaller the value, the more similar are the two sites.



#     Table 10.3: Right
#     COEFFICIENT of ROGERS AND TANIMOTO for presence-absence data.
#     The smaller the value, the more similar are the two sites.
source("Ch10Library.r")
table_10.3 <- association(transect, "rogers")
table_10.3




#     Table 10.4: Left
#     JACCARD COEFFICIENT for presence-absence data.
#     The smaller the value, the more similar are the two sites.
#     You need to install the vegan package from the R website.
library(vegan)
table_10.4 <- vegdist(decostand(transect, "pa"), "jaccard", upper = T)
round(table_10.4, 2)

#     or
#     Steinhaus coefficient is synonym for 1-(Jaccard dissimilarity index)
#     You need to install the package labdsv
library(labdsv)
table_10.4 <- dsvdis(transect, "steinhaus", upper = T)
round(table_10.4, 2)



#     Table 10.4: Right
#     SORENSEN COEFFICIENT for presence-absence data.
#     The smaller the value, the more similar are the two sites.
library(labdsv)
table_10.4 <- dsvdis(transect, "sorensen", upper = T)
round(table_10.4, 2)

#      or 
library(vegan)
table_10.4 <- vegdist(decostand(transect, "pa"), "bray", upper = T)
round(table_10.4, 2)



#     Table 10.5: Left
#     SIMILARITY RATIO for quantitative data.
#     The smaller the value, the more similar are the two sites.
source("Ch10Library.r")
table_10.5 <- association(transect, "simratio")
table_10.5



#     Table 10.5: Right
#     BRAY-CURTIS COEFFICIENT for quantitative data.
#     The smaller the value, the more similar are the two sites.
library(vegan)
table_10.5 <- vegdist(transect, "bray", upper = T)
round(table_10.5, 2)

#or
library(labdsv)
table_10.5 <- dsvdis(transect, "bray", upper = T)
round(table_10.5, 2)



#     Table 10.7 
#     EUCLIDEAN DISTANCES for quantitative data.
#     The smaller the value, the more similar are the two sites.
library(vegan)
table_10.7 <- vegdist(transect, "euclidean", upper = T)
round(table_10.7, 2)

#or
table_10.7 <- dist(transect, "euclidean", upper = T)
round(table_10.7, 2)



#     Table 10.8: Left
#     ORCHIAI COEFFICIENT for quantitative data.
#     The smaller the value, the more similar are the two sites.
library(labdsv)
table_10.8 <- dsvdis(transect, "ochiai", upper = T)
round(table_10.8, 2)



#     Table 10.8: Right
#     CHORD DISTANCE for quantitative data.
#     The smaller the value, the more similar are the two sites.
library(vegan)
table_10.8 <- vegdist(decostand(transect, "norm"), "euclidean", upper = T)
round(table_10.8, 2)




#     Table 10.9: Left
#     MANHATTAN DISTANCES for quantitative data.
#     The smaller the value, the more similar are the two sites.
library(vegan)
table_10.9 <- vegdist(transect, "manhattan", upper = T)
table_10.9

#or
table_10.9 <- dist(transect, "manhattan", upper = T)
table_10.9



#     Table 10.9: Right
#     SIMILARITY RATIO for quantitative data.
#     The smaller the value, the more similar are the two sites.
source("Ch10Library.r")
table_10.9 <- association(transect, "whittaker")
table_10.9




###############################
#
#     Association between species

#     Table 10.10: Left
#     Covariance coefficients
table_10.10 <- cov(transect)
round(table_10.10)



#     Table 10.10: Right
#     Correlation coefficients
table_10.10 <- cor(transect, method = "pearson")
round(table_10.10, 2)



#     Spearman rank correlation coefficients
round(cor(transect, method = "spearman"), 2)




#     Table 10.13
#     Observed, expected and contribution of each cell to 
#     Chi-square statistic
parasite <- read.table("parasite.txt", header = T, row.names = 1)
expected <- parasite
contribution <- parasite
for (i in 1:nrow(parasite)) {
for (j in 1:ncol(parasite)) {
expected[i, j] <- round(sum(parasite[i, ])*sum(parasite[, j])/
                        sum(parasite), 2)
contribution[i, j] <- round(((parasite[i, j]-expected[i, j])^2)/
                              expected[i, j], 2)
}
}
parasite
expected
contribution




#     Table 10.15
#     CHI-SQUARE DISTANCES among species
#     The smaller the values, the more similar are the species
library(labdsv)
table_10.15 <- dsvdis(t(transect), "chisq", upper = T)
round(table_10.15, 1)



#     Table 10.16
#     JACCARD INDEX for species
#     The smaller the values, the more similar are the species
source("Ch10Library.r")
table_10.16 <- association(t(transect), "jaccard")
table_10.16




###############################
#
#     Mantel test (spatial auto-correlation)


birds <- read.table("BirdsDayHour.txt", header = T)
#Due to copyright reasons a slightly modified data set is provided; 30 rows
#were omitted. Therefore, results between the R output and the results in the book
#are slightly different. Both analyses (book and below) are correct, though.



#In the code below, it may be advisable to use 9999 permutations
library(vegan)

#Chord
F1 <- vegdist(decostand(birds[, 2:71], "norm"), "euclidean", upper = T)
F2 <- vegdist(birds[, 72:73], "euclidean", upper = T)
chord_mantel <- mantel(F1, F2, permutations = 100)
chord_mantel


#Euclidean
F1 <- vegdist(birds[, 2:71], "euclidean", upper = T)
F2 <- vegdist(birds[, 72:73], "euclidean", upper = T)
chord_mantel <- mantel(F1, F2, permutations = 100)
chord_mantel



#Jaccard
F1 <- vegdist(decostand(birds[, 2:71], "pa"), "jaccard", upper = T)
F2 <- vegdist(birds[, 72:73], "euclidean", upper = T)
jaccard_mantel <- mantel(F1, F2, permutations = 100)
jaccard_mantel


#Takes a while!
source("Ch10Library.r")
F1 <- association(birds[, 2:71], "whittaker")
F2 <- vegdist(birds[, 72:73], "euclidean", upper = T)
whittaker_mantel <- mantel(F1, F2, permutations = 100)
whittaker_mantel



#     Table 10.17
cat(" Chord ", "\t", round(chord_mantel$statistic, 3), "\t", 
                   chord_mantel$signif, "\n", 
    "Jaccard", "\t", round(jaccard_mantel$statistic, 3), "\t", 
                     jaccard_mantel$signif, "\n", 
    "Euclidean", "\t", round(euclidean_mantel$statistic, 3), "\t", 
                       euclidean_mantel$signif, "\n", 
    "Whittaker", "\t", round(whittaker_mantel$statistic, 3), "\t", 
                       whittaker_mantel$signif, "\n")



					   
					   
###############################################
#
#     ANOSIM (testing differences between days)
#Due to copyright reasons a slightly modified data set is provided (a couple
#of rows were omitted); 30 rows were omitted to avoid any potential copyright trouble.
#Therefore, results between the R output and the book
#will be slightly different. Both analyses (book and below) are correct, though.


#In the code below, it may be advisable to use 10000 permutations


birds <- read.table("BirdsDayHour.txt", header = T)

dat <- t(birds[, 2:71])
day <- c(rep(1, 23), rep(2, 24), rep(3, 23))


library(vegan)
F <- vegdist(decostand(dat, "norm"), "euclidean", upper = T)
chord_anosim <- anosim(F, day, permutations = 100)
chord_anosim



library(vegan)
F <- vegdist(decostand(dat, "pa"), "jaccard", upper = T)
jaccard_anosim <- anosim(F, day, permutations = 100)
jaccard_anosim



library(vegan)
F <- vegdist(dat, "euclidean", upper = T)
euclidean_anosim <- anosim(F, day, permutations = 100)
euclidean_anosim



source("Ch10Library.r")
F <- association(dat, "whittaker")
whittaker_anosim <- anosim(F, day, permutations = 100)
whittaker_anosim




#     Summary Table of overall analysis
cat(" Chord ", "\t", round(chord_anosim$statistic, 3), "\t", 
                   chord_anosim$signif, "\n", 
    "Jaccard", "\t", round(jaccard_anosim$statistic, 3), "\t", 
                     jaccard_anosim$signif, "\n", 
    "Euclidean", "\t", round(euclidean_anosim$statistic, 3), "\t", 
                       euclidean_anosim$signif, "\n", 
    "Whittaker", "\t", round(whittaker_anosim$statistic, 3), "\t", 
                       whittaker_anosim$signif, "\n")




#     Pairwise ANOSIM comparisons
#     function pairwise.anosim uses 
#     the default number of permutations (1000) 
#     which differs from the book number (9999)
#     you can change this default in library "Ch10Library.r"
source("Ch10Library.r")
pairwise.anosim(dat, day, "euclidean")
pairwise.anosim(dat, day, "jaccard")
pairwise.anosim(dat, day, "chord")
pairwise.anosim(dat, day, "whittaker")



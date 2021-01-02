#    R code for: Chapter 29 in:
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



setwd("D:\\applicat\\HighlandStatistics\\Book\\R\\RChapter29\\")




Dolphins <- read.table("ScottishDolphins.txt", header = T, sep = "\t")





#      Figure 29.2
boxplot(as.data.frame(apply(Dolphins, MARGIN = 2, FUN = function(x) 
                            {(x-mean(x))/sd(x)})))





#      Covariance-based PCA (no transformation of the data)
cov_PCA <- princomp(Dolphins, cor = F)

#First two PC represent 88% of the total data variation
#see cumulative proportion:
summary(cov_PCA)
#Screeplot:
screeplot(cov_PCA, npcs = 10, main = "Eigenvalues", type = "l", cex = 2)




# Interpretation of PCs.  
# Loadings for PCs are ordered according to absolute value
# First PC:
ord <- order(abs(cov_PCA$loadings[, 1]), decreasing = T)
cov_PCA$loadings[ord, 1]

# Second PC:
ord2 <- order(abs(cov_PCA$loadings[, 2]), decreasing = T)
cov_PCA$loadings[ord2, 2]





#      Correlation-based PCA (the data are normalised)
cor_PCA <- princomp(Dolphins, cor = T)




#       Table 29.1
CumProp <- 0
for (i in 1:10) {
cat(paste("Axis", i, sep = ""), "\t", 
    "eig", round(cor_PCA$sdev[i]^2, 3), "\t",  
    "CumProp", round(CumProp <- CumProp+cor_PCA$sdev[i]^2/ 
                     sum(cor_PCA$sdev^2), 2), "\n")
}




#        Figure 29.3
screeplot(cor_PCA, npcs = 10, main = "Eigenvalues", type = "l", cex = 2)




# First normalisation Sum_z(c^2) = 1. First PC loadings. 
cor_PCA$loadings[, 1]
# Second normalisation Sum_z(c^2) = Var(Z). First PC loadings. 
cor_PCA$loadings[, 1]*cor_PCA$sdev[1]
# Third normalisation Var(c_z) = 1. First PC loadings. 
cor_PCA$loadings[, 1]/cor_PCA$sdev[1]




#       Table 29.2
round(cor_PCA$loadings[, 1:2], 2)




#      Figure 29.4
biplot(cor_PCA, choices = 1:2, scale = 0, main = "Correlation, scaling = 0", 
       cex = c(0.5, 1), col = 1, arrow.len = 0)




#The SCoTLASS calculations were carried out by Dr. Trendafilov. Please
#contact him directly for software code.


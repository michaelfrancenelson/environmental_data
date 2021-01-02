#    R code for: Chapter 30 in:
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



setwd("D:\\applicat\\HighlandStatistics\\Book\\R\\RChapter30")




Turtles <- read.table("turtles.txt", header = T, sep = "\t")



#      Figure 30.2
boxplot(Turtles[, 8:31])



#      Figure 30.3
source("MyLibrary.r")
pairs(Turtles[, 8:17], lower.panel = panel.cor, 
      upper.panel = panel.smooth2)




#      Correlation-based PCA 
corPCA <- princomp(Turtles[, 8:31], cor = T)




#      Figure 30.4
biplot(corPCA, choices = 1:2, cex = c(0.5, 1), col = 1, 
       arrow.len = 0, xlab = "axis 1", ylab = "axis 2")

#Note: In the book we used the software pacage Brodgar. Its graphs look different,
#but the results are the same


############################################################
#
#    Option 1. Using variable D2 as a scalar measure of size
#    FILTER OUT D2

D2_measure <- Turtles$D2
D <- cbind(Turtles[, c(8, 10:31)])
partialled <- matrix(nrow = 123, ncol = 23)
dimnames(partialled)[[2]] <- names(D)
for (i in 1:23){
    Z <- D[, i]
    tmp <- lm(Z~D2_measure)
    partialled[, i] <- residuals(tmp)
}



#Principal component analysis
partialPCA <- princomp(partialled, cor = T)



#Eigenvalues corresopnding to principal component axes and 
#cumulative variation of the data explained by axes
#(eigenvalues are scaled to have sum of one)
CumVar <- 0
for (i in 1:23) {
cat(paste("Axis", i, sep = ""), "\t", 
    "scaled_eig", 
    round((partialPCA$sdev[i]^2)/sum(partialPCA$sdev^2), 3), 
    "\t", "CumVar", 
    round(CumVar <- CumVar + partialPCA$sdev[i]^2/sum(partialPCA$sdev^2), 2), 
    "\n")
}




#      Figure 30.5
biplot(partialPCA, choices = 1:2, cex = c(0.5, 1), col = 1, arrow.len = 0, 
       xlab = "axis 1", ylab = "axis 2")

 
 
 
#################################################################
#
#    Option 2. Removing isometric size with a pre-standartisation
#    


D <- cbind(Turtles[, 8:31])
prestd <- as.data.frame(matrix(nrow = 123, ncol = 24))
names(prestd) <- names(D)
for (i in 1:123){
     prestd[i, ] <- D[i, ]/sqrt(sum(D[i, ]^2))
}



#Principal component analysis
prestdPCA <- princomp(prestd, cor = T)



#Eigenvalues corresopnding to principal component axes and 
#cumulative variation of the data explained by axes
#(eigenvalues are scaled to have sum of one)
CumVar <- 0
for (i in 1:24) {
cat(paste("Axis", i, sep = ""), "\t", 
    "scaled_eig", 
    round((prestdPCA$sdev[i]^2)/sum(prestdPCA$sdev^2), 3), "\t",  
    "CumVar", 
    round(CumVar <- CumVar + prestdPCA$sdev[i]^2/sum(prestdPCA$sdev^2), 2), 
    "\n")
}



#     PLot 30.6 A
plot(prestdPCA$loadings[, 1:2], type = "n", 
     xlab = "axis 1", ylab = "axis 2")
arrows(x0 = rep(0, 24), 
       y0 = rep(0, 24), 
       x1 = prestdPCA$loadings[, 1], 
	   y1 = prestdPCA$loadings[, 2], 
	   col = 1, length = 0)

text(prestdPCA$loadings[, 1:2], names(D))



#     PLot 30.6 B
plot(prestdPCA$scores[, 1:2], type = "n", 
     xlab = "axis 1", ylab = "axis 2")
text(prestdPCA$scores[, 1:2], labels = Turtles$SuperFamily)



#     PLot 30.6 C
plot(prestdPCA$scores[, 1:2], type = "n", 
     xlab = "axis 1", ylab = "axis 2")
text(prestdPCA$scores[, 1:2], labels = Turtles$Environment3)




############################################################
#
#    Option 3. Removing isometric size with double-centering
#    

D <- cbind(Turtles[, 8:31])
dbcent <- as.data.frame(matrix(nrow = 123, ncol = 24))
names(dbcent) <- names(D)
for (i in 1:123){
    dbcent[i, ] <- (D[i, ] - mean(as.numeric(D[i, ])))
}



#Principal component analysis
dbcentPCA <- princomp(dbcent, cor = F)




#Eigenvalues corresopnding to principal component axes and 
#cumulative variation of the data explained by axes
#(eigenvalues are scaled to have sum of one)
CumVar <- 0
for (i in 1:24) {
cat(paste("Axis", i, sep = ""), "\t", 
    "scaled_eig", 
    round((dbcentPCA$sdev[i]^2)/sum(dbcentPCA$sdev^2), 3), "\t",  
    "CumVar", 
    round(CumVar <- CumVar + dbcentPCA$sdev[i]^2/sum(dbcentPCA$sdev^2), 3), 
    "\n")
}



#     PLot 30.7
biplot(dbcentPCA, choices = 1:2, cex = c(0.7, 1), col = 1, arrow.len = 0, 
       xlabs = Turtles$Environment3, xlab = "axis 1", ylab = "axis 2")



#The calculations presented in Section 30.7 were carried out by the first author
#of the chapter. Please contact him directly for advice how to run these analysis.
#See also his book on "Morphometric analysis in R" (2008). J. Claude


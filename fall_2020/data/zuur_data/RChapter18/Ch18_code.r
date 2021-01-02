#    R code for: Chapter 18 in:
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



setwd("D:\\applicat\\HighlandStatistics\\Book\\R\\RChapter18\\")
par(ask=T)



#      Figure 18.1
irreg_lattice <- read.table("CentPoint.txt", header = TRUE)
plot(irreg_lattice, axes = F, xlab = "", ylab = "", pch = 19, cex = 2)
text(irreg_lattice, labels = row.names(irreg_lattice), pos = 4, cex = 2)




#     Table 18.2
irreg_lattice <- read.table("CentPoint.txt", header = TRUE)
library(spatstat)
w <- as.owin(list(xrange = c(4.6, 21.1), yrange = c(84.5, 93.6)))
point_pattern <- as.ppp(as.matrix(irreg_lattice), w)
dist_matrix <- pairdist(point_pattern)
dist_matrix <- as.dist(round(dist_matrix, 1), upper = T)
dist_matrix








#       Figure 18.4
#       need to change the display window size 
#       to fit the axis to margins
par(mfrow = c(2, 2))

#A
plot_1_6 <- read.table("Plot_1_6.txt", header = TRUE)
library(spatstat)
w <- as.owin(list(xrange = c(3, 25), 
                  yrange = c(3, 25)))
point_pattern.1_6 <- ppp(x = plot_1_6$X, y = plot_1_6$Y, 
                         window = w, marks = plot_1_6$height)
plot(point_pattern.1_6, maxsize = 0.5, main = "Figure 18.4 A")
axis(1, at = seq(5, 25, 5)); axis(2, at = seq(5, 25, 5))


#B
plot_22_7 <- read.table("Plot_22_7.txt", header = TRUE)
library(spatstat)
w <- as.owin(list(xrange = c(3, 25), 
                  yrange = c(3, 25)))
point_pattern.22_7 <- ppp(x = plot_22_7$X, y = plot_22_7$Y, 
                          window = w, marks = plot_22_7$height)
plot(point_pattern.22_7, maxsize = 0.5, main = "Figure 18.4 B")
axis(1, at = seq(5, 25, 5)); axis(2, at = seq(5, 25, 5))


#C
plot_1_6 <- read.table("Plot_1_6.txt", header = TRUE)
library(spdep)
v <- voronoi.mosaic(plot_1_6$X, plot_1_6$Y)
plot(plot_1_6[, 1:2], pch = 20, main = "Figure 18.4 C", 
     xlab = "", ylab = "", xlim = c(3, 25), ylim = c(3, 25))
plot(v, add = TRUE, do.points = F)


#D
plot_22_7 <- read.table("Plot_22_7.txt", header = TRUE)
library(spdep)
v <- voronoi.mosaic(plot_22_7[, 1], plot_22_7[, 2])
plot(plot_22_7[, 1:2], pch = 20, main = "Figure 18.4 D", 
     xlab = "", ylab = "", xlim = c(3, 25), ylim = c(3, 25))
plot(v, add = TRUE, do.points = F)
par(mfrow = c(1, 1))







#     Figure 18.5 A
par(mfrow = c(1, 1))
plot_1_6 <- read.table("Plot_1_6.txt", header = TRUE)
distance_band <- c(2.5, 5, 7.5, 10, 12.5, 15)
dist_ring_count <- length(distance_band) - 1
I <- data.frame(distance = 0, 
              z.score = 0, 
              Index = 0, 
              expectation = 0, 
              variance = 0, 
              p.value = 0)
library(spdep)
for(i in 1:dist_ring_count) {
  nb <- dnearneigh(as.matrix(plot_1_6[, 1:2]), distance_band[i], 
                                               distance_band[i + 1])
  W <- nb2listw(nb, style = "B")
  M <- moran.test(plot_1_6$height, W, alternative = "two.sided")
  I[i, 1] <- 0.5*(distance_band[i + 1] + distance_band[i])
  I[i, 2] <- M$statistic
  I[i, 3] <- M$estimate[1]
  I[i, 4] <- M$estimate[2]
  I[i, 5] <- M$estimate[3]
  I[i, 6] <- M$p.value
}
I[, c(1, 6)]
plot(I$distance, I$z.score, ylim = c( - 4, 4), 
     xlab = "Distance", ylab = "Moran's I", main = "Figure 18.5 A")
lines(I$distance, I$z.score)
#    Expectation. see that I$expectation == - 1/(count of trees - 1)
abline(h = - 1/(nrow(plot_1_6) - 1))
abline(h = + 1.96, lty = 2)
abline(h = - 1.96, lty = 2)







#     Figure 18.5 B
par(mfrow = c(1, 1))
plot_22_7 <- read.table("Plot_22_7.txt", header = TRUE)
distance_band <- c(2.5, 5, 7.5, 10, 12.5, 15)
dist_ring_count <- length(distance_band) - 1
I <- data.frame(distance = 0, 
              z.score = 0, 
              Index = 0, 
              expectation = 0, 
              variance = 0, 
              p.value = 0)
library(spdep)
for(i in 1:dist_ring_count) {
  nb <- dnearneigh(as.matrix(plot_22_7[, 1:2]), distance_band[i], 
                                                distance_band[i + 1])
  W <- nb2listw(nb, style = "B")
  M <- moran.test(plot_22_7$height, W, alternative = "two.sided")
  I[i, 1] <- 0.5*(distance_band[i + 1] + distance_band[i])
  I[i, 2] <- M$statistic
  I[i, 3] <- M$estimate[1]
  I[i, 4] <- M$estimate[2]
  I[i, 5] <- M$estimate[3]
  I[i, 6] <- M$p.value
}
I[, c(1, 6)]
plot(I$distance, I$z.score, ylim = c( - 4, 4), 
     xlab = "Distance", ylab = "Moran's I", main = "Figure 18.5 B")
lines(I$distance, I$z.score)
#    Expectation. See that I$expectation ==  - 1/(count of trees - 1)
abline(h = - 1/(nrow(plot_22_7) - 1))
abline(h = + 1.96, lty = 2)
abline(h = - 1.96, lty = 2)





############################################
#
#       Example of linear regression applied 
#       on the tree height data (p. 332)

plot_1_6 <- read.table("Plot_1_6.txt", header = TRUE)
model_LM <- lm(height ~ diameter, data = plot_1_6)
summary(model_LM)
AIC(model_LM)



#     Moran's I test on the residuals of the fitted linear model
library(spdep)
nb <- tri2nb(as.matrix(plot_1_6[, 1:2]))
W <- nb2listw(nb, style = "W")
model_LM.moran <- lm.morantest(model_LM, W, alternative = "greater", 
                               resfun = weighted.residuals)

#     Moran's I
cat("Moran's I", model_LM.moran$estimate[1], "\n")

#     p-value
cat("p-value", model_LM.moran$p.value, "\n")
#    this means that there is evidence of spatial 
#    auto - correlation in the residuals




############################################
#
#       Example of a SAR model for 
#       the tree height data (p. 334)
plot_1_6 <- read.table("Plot_1_6.txt", header = TRUE)

library(spdep)
nb <- tri2nb(as.matrix(plot_1_6[, 1:2]))
W <- nb2listw(nb, style = "W")
model_SAR <- lagsarlm(height ~ diameter, data = plot_1_6, W, 
                      method = "eigen", quiet = TRUE)
summary(model_SAR)

#     residual standard error
sqrt(model_SAR$SSE/nrow(plot_1_6))

#     To examine auto - correlation of residuals:
summary(model_SAR)$Wald1




#################################
#
#      Example of a SMA morel for 
#      a tree height data (p. 335)

plot_1_6 <- read.table("Plot_1_6.txt", header = TRUE)

library(spdep)
nb <- tri2nb(as.matrix(plot_1_6[, 1:2]))
W <- nb2listw(nb, style = "W")
model_SMA <- errorsarlm(height ~ diameter, data = plot_1_6, W, 
                        method = "eigen", quiet = TRUE)
summary(model_SMA)

#     residual standard error
sqrt(model_SMA$SSE/nrow(plot_1_6))

#     To examine auto - correlation of residuals:
summary(model_SMA)$Wald1





######################################
#
#    linear resgression (18.8, p. 336)


plot_1_6 <- read.table("Plot_1_6.txt", header = TRUE)
species_LM <- lm(height ~ diameter + as.factor(code_spec), 
                 data = plot_1_6)
summary(species_LM)
AIC(species_LM)

#     Moran's I test on the residuals of the fitted linear model
library(spdep)
nb <- tri2nb(as.matrix(plot_1_6[, 1:2]))
W <- nb2listw(nb, style = "W")
species_LM.moran <- lm.morantest(species_LM, W, alternative = "greater",
                                 resfun = weighted.residuals)

#     Moran's I
cat("Moran's I", species_LM.moran$estimate[1], "\n")

#     p-value
cat("p-value", species_LM.moran$p.value, "\n")
#    this means that there is no evidence of spatial 
#    auto - correlation in the residuals





##########################
#
#     Model (18.9, p. 336)


plot_1_6 <- read.table("Plot_1_6.txt", header = TRUE)
library(spdep)
species_SMA <- errorsarlm(height ~ diameter + as.factor(code_spec), 
                          data = plot_1_6, W, method = "eigen", 
                          quiet = TRUE)
summary(species_SMA)

#     residual standard error
sqrt(species_SMA$SSE/nrow(plot_1_6))

#     To examine auto - correlation of residuals:
summary(species_SMA)$Wald1




par(ask=F)

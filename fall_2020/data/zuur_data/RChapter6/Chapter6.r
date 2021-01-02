#    R code for: Chapter 6 in:
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



setwd("D:\\applicat\\HighlandStatistics\\Book\\R\\RChapter6\\")


par(mar = c(4.5, 4.5, 0.5, 0.5), cex.lab = 1.5, cex.axis = 1.5)



#      Figure 6.1
par(mfrow = c(2,2))
x <- seq(0, 50, 1)
plot(dpois(x, lambda = 1, log = FALSE), type = "h", 
     xlab = "x", ylab = "density curve", lwd = 2)
plot(dpois(x, lambda = 5, log = FALSE), type = "h", 
     xlab = "x", ylab = "density curve", lwd = 2)
plot(dpois(x, lambda = 15, log = FALSE), type = "h", 
     xlab = "x", ylab = "density curve", lwd = 2)
plot(dpois(x, lambda = 25, log = FALSE), type = "h", 
     xlab = "x", ylab = "density curve", lwd = 2)
par(mfrow = c(1,1))




#      output for Poisson regression, page 83
RIKZ <- read.table(file = "RIKZ.txt",header = TRUE)
RIKZ$Richness <- rowSums(RIKZ[,2:76] > 0)
RIKZ_poisson <- glm(Richness ~ NAP, 
					data = RIKZ, 
					family = poisson)
summary(RIKZ_poisson)




#     output for quasi-Poisson regression, page 83
RIKZ <- read.table(file = "RIKZ.txt",header = TRUE)
RIKZ$Richness <- rowSums(RIKZ[,2:76] > 0)
RIKZ_quasi <- glm(Richness ~ NAP, 
					data = RIKZ, 
					family = quasipoisson)
summary(RIKZ_quasi)



#      parameters for Poisson regression, page 84
RIKZ <- read.table(file = "RIKZ.txt",header = TRUE)
RIKZ$Richness <- rowSums(RIKZ[,2:76] > 0)
RIKZ_poisson.1 <- glm(Richness ~  NAP + 
						factor(week) + 
						factor(exposure), 
						data = RIKZ, 
						family = poisson)
summary(RIKZ_poisson.1)




#     output of "drop 1 variable" tool, page 84
RIKZ <- read.table(file = "RIKZ.txt",header = TRUE)
RIKZ$Richness <- rowSums(RIKZ[,2:76] > 0)
RIKZ_poisson.1 <- glm(Richness ~ NAP + 
						factor(week) + 
						factor(exposure), 
						data = RIKZ, 
						family = poisson)
drop1(RIKZ_poisson.1, test = "Chisq")




#      Compare quasi-Poisson models, page 85
RIKZ <- read.table(file = "RIKZ.txt",header = TRUE)
RIKZ$Richness <- rowSums(RIKZ[,2:76] > 0)
RIKZ_poisson.1 <- glm(Richness ~ NAP + 
						factor(week) + 
						factor(exposure), 
						data = RIKZ, 
						family = quasipoisson)
RIKZ_poisson.2 <- glm(Richness ~ NAP + 
						factor(week), 
						data = RIKZ, 
						family = quasipoisson)
anova(RIKZ_poisson.1,RIKZ_poisson.2, test = "F")




#     Figure 6.4
Solea <- read.table("Solea.txt", header = T)
plot(Solea$salinity, Solea$Solea_solea, 
	 xlab = "Salinity", ylab = "S. solea")
Solea_model <- lm(Solea_solea ~ salinity, data = Solea)
abline(Solea_model)




#     Logistic regression, page 90
Solea <- read.table("Solea.txt", header = T)
Solea_logist <- glm(Solea_solea ~ salinity, 
					data = Solea, family = binomial)
summary(Solea_logist)



#     Figure 6.5
Solea <- read.table("Solea.txt", header = T)
plot(Solea$salinity, Solea$Solea_solea, 
	 xlab = "Salinity", ylab = "P")
Solea_logist <- glm(Solea_solea ~ salinity, 
					data = Solea, family = binomial)
new_salinity <- seq(0,35,1)
lines(new_salinity, 
	  predict(Solea_logist, data.frame(salinity = new_salinity), 
	  type = "response"))


	 
#     Figure 6.6
Solea <- read.table("Solea.txt", header = T)
Solea_logist <- glm(Solea_solea ~ salinity, 
					data = Solea, family = binomial)
par(mfrow = c(2,2), mar = c(5,5,1.5,0.5))
plot(Solea_logist, which = c(1:4), add.smooth = F, cex.id = 1)
par(mfrow = c(1,1), mar = c(4.5,4.5,0.5,0.5))



#     output of logistic model, page 96
Solea <- read.table("Solea.txt", header = T)
Solea_logist.2 <- glm(Solea_solea ~ temperature + salinity, 
					data = Solea, family = binomial)
summary(Solea_logist.2)



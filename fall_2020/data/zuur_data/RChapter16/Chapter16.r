#    R code for: Chapter 16 in:
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



setwd("D:\\applicat\\HighlandStatistics\\Book\\R\\RChapter16\\")

library(MASS)


par(mar = c(4.5, 4.5, 0.5, 1.5), cex.lab = 1.5, cex.axis = 1.2)




#       Figure 16.1
Squid <- read.table(file = "Squid.txt", header = TRUE, 
                      row.names = 1)
boxplot(Squid$GSI ~ Squid$MONTH, xlab = "Month", ylab = "GSI index")




#      Figure 16.2
SSTNA <- read.table(file = "SSTNorthAmerica.txt", header = TRUE)
library(lattice)
xyplot(SST_22_24 + SST_24_26 + 
       SST_26_28 + SST_28_30 + 
       SST_30_32 + SST_32_34 + 
       SST_34_36 + SST_36_38 +
       SST_38_40 + SST_40_42 + 
       SST_42_44 + SST_44_46 + 
       SST_46_48 ~ AxisYear, data = SSTNA, 
       type = "l", col = 1, outer = T, 
       xlab = "Time", ylab = "SST",  
       scales = list(x = list(relation = "same"), 
                     y = list(relation = "free")), 
       background = "white")




#      Table 16.2
#      Variable 1 corresponds to SST-22-24, 
#      Variable 2 corresponds to SST-26-28, etc.
library(lattice)
SSTNA <- read.table(file = "SSTNorthAmerica.txt", header = TRUE)
names(SSTNA)
SST <- cbind(SSTNA$SST_22_24, SSTNA$SST_24_26, 
             SSTNA$SST_26_28, SSTNA$SST_28_30, 
             SSTNA$SST_30_32, SSTNA$SST_32_34, 
             SSTNA$SST_34_36, SSTNA$SST_36_38, 
             SSTNA$SST_38_40, SSTNA$SST_40_42, 
             SSTNA$SST_42_44, SSTNA$SST_44_46, 
             SSTNA$SST_46_48)
ID <- c("SST_22_24", "SST_22_26", 
        "SST_26_28", "SST_28_30", 
        "SST_30_32", "SST_32_34", 
        "SST_34_36", "SST_36_38", 
        "SST_38_40", "SST_40_42", 
        "SST_42_44", "SST_44_46", 
        "SST_46_48")
for (i in 1:ncol(SST)) {
     cat("Variable", i, "\t", ID[i], "\n")
     for (j in 1:ncol(SST)) {
          if (i != j) {
              corr <- cor(SST[, i], SST[, j], use = "complete.obs")
              if (corr>0.9) {
                  cat(i, "\t", j, "\t", round(corr, 3), "\n")
              }
          }
     }
}



#        Figure 16.3
CPUE <- read.table("fish.txt", header = T, sep = "\t")
CPUE <- ts(CPUE[, 2:12], start = min(CPUE$Year), 
                      end = max(CPUE$Year))
plot.ts(CPUE, plot.type = "single", col = seq(1:11)) 

#        or 
CPUE <- read.table("fish.txt", header = T, sep = "\t")
plot(c(min(CPUE$Year), max(CPUE$Year)), 
     c(min(CPUE[, 2:12], na.rm = T), max(CPUE[, 2:12], na.rm = T)), 
     type = "n", xlab = "Time (Year)", ylab = "CPUE")
for (i in 2:12) {
     isNAfalse <- is.na(CPUE[, i]) == F
     lines(CPUE$Year[isNAfalse], CPUE[isNAfalse, i], col = i-1)
}




#       Figure 16.4
#       autocorrelation
CPUE <- read.table("fish.txt", header = T, sep = "\t")
source("Ch_16_Library.r")
acor(CPUE[, 2], num_of_lags = 10, plotf = T)
#      or
#      Figure 16.4
#      using acf:stats function
CPUE <- read.table("fish.txt", header = T, sep = "\t")
Station1 <- CPUE[, 2]
acf(Station1, na.action = na.pass, 
    type = "correlation", lag.max = 10, main = "", 
    xlab = "time lag", ylab = "correlation")




#       Figure 16.5
#       cross-correlation
CPUE <- read.table("fish.txt", header = T, sep = "\t")
source("Ch_16_Library.r")
ccor(CPUE[, 2], CPUE[, 3], num_of_lags = 10, plotf = T)
#      or
#      Figure 16.5
#      using ccf:stats function
CPUE <- read.table("fish.txt", header = T, sep = "\t")
Station1 <- CPUE[, 2]
Station2 <- CPUE[, 3]
ccf(Station1, Station2, na.action = na.omit, 
    type = "correlation", lag.max = 10, main = "", 
    xlab = "time lag", ylab = "correlation")




#     Table 16.2 and
#     Table 16.3
#The correlations in Table 16.2 and 16.3 were calculated in
#Brodgar, which makes use of the FORTRAN IMSL libary
#The functions cor, cor.test and ccor in R deal in a different
#way with missing values than the IMSL. Hence, any differences
#in the output obtained by R and the numbers in Table 16.2 and 16.3
#are due to how these packages deal with missing values.
#Specific differences: If there are non-matching missing values in
#each series, do you calculate the standard deviation of each individual
#series on the matching observed data (i.e. remove every row of data
#of the two series if there is a missing value in one of them), or do
#you calculate the variance of each individual series on all the
#observed data of that series. The latter approach is followed
#by Brodgar. R chops out all rows, and then calculates the variance.
#Note that for the cross-product, rows with missing values have to be
#removed anyway.

CPUE <- read.table("fish.txt", header = T, sep = "\t")
source("Ch_16_Library.r")
table_16_2 <- matrix(nrow = 11, ncol = 11) # to collect correlations
table_16_3 <- matrix(nrow = 11, ncol = 11) # to collect max cross-corr
for (i in 1:10) {
     for (j in (i+1):11) {
         temp_cor <- cor.test(CPUE[, i+1], CPUE[, j+1], 
                              use = "complete.obs")
         table_16_2[i, j] <- as.numeric(temp_cor$estimate)
         if (as.numeric(temp_cor$p.value)<0.05) {
         table_16_2[j, i] <- T
         } else {
         table_16_2[j, i] <- F
         }
         
         temp_cor <- ccor(CPUE[, i+1], CPUE[, j+1], 
                          num_of_lags = 10, plotf = F)
         temp_max <- which.max(abs(temp_cor$cross_cor))
         table_16_3[i, j] <- temp_cor$cross_cor[temp_max]
         table_16_3[j, i] <- temp_cor$lag_k[temp_max]
         
         }
}

round(table_16_2, 2)
round(table_16_3, 3)





#     Figure 16.6
NAO_SST <- read.table("Scottish_SST.txt", header = T, 
                      sep = "\t", dec = ".")

library(lattice)
xyplot(NAO + SST ~ Time, type = "l", data = NAO_SST, 
       outer = T, layout = c(1, 2), ylab = "", 
       scales = list(x = list(relation = "same"), 
                     y = list(relation = "free")))
 
 
#      Figure 16.7
NAO_SST <- read.table("Scottish_SST.txt", header = T, 
                      sep = "\t", dec = ".")
library(lattice)
source("MyLibrary.r")
coplot(SST ~ NAO | Month, data = NAO_SST, 
       overlap = 0, number = 12, panel = panel.smooth2)



#       Figure 16.8
NAO_SST <- read.table("Scottish_SST.txt", header = T, 
                      sep = "\t", dec = ".")
library(lattice)
SST_loess <- loess(SST ~ Time, data = NAO_SST, span = 0.05)
SST_trend <- predict(SST_loess, data.frame(Time = NAO_SST$Time))
NAO_loess <- loess(NAO ~ Time, data = NAO_SST, span = 0.05)
NAO_trend <- predict(NAO_loess, data.frame(Time = NAO_SST$Time))
xyplot(NAO_trend + SST_trend ~ Time, type = "l", data = NAO_SST, 
       outer = T, layout = c(1, 2), ylab = "", col="black",
       scales = list(x = list(relation = "same"), 
                     y = list(relation = "free")))
 

 
#       Figure 16.9
NAO_SST <- read.table("Scottish_SST.txt", header = T, 
                      sep = "\t", dec = ".")
library(lattice)
SST_loess <- loess(SST ~ Time, data = NAO_SST, span = 0.05)
SST_trend <- predict(SST_loess, data.frame(Time = NAO_SST$Time))
NAO_loess <- loess(NAO ~ Time, data = NAO_SST, span = 0.05)
NAO_trend <- predict(NAO_loess, data.frame(Time = NAO_SST$Time))
source("Ch_16_Library.r")
ccor_N_S <- ccor(SST_trend, NAO_trend, num_of_lags = 60, plotf = T)

 
 
 
#       Figure 16.10
ducks <- read.table("ducks.txt", header = T, sep = "\t", dec = ".")
library(lattice)
xyplot(Gadwall + Goldeneye + Goosander + Mallard + 
       Pochard + Pintail ~ Year, data = ducks, 
       type = "l", outer = T, layout = c(3, 2), 
       scales = list(x = list(relation = "same"), 
                     y = list(relation = "free")), 
       ylab = "Abundance", xlab = "Time (years)")
 

 
 
#     Figure 16.11
ducks <- read.table("ducks.txt", header = T, sep = "\t", dec = ".")
library(vegan)
ducks_pca <- rda(ducks[, -c(1, 2)], scale = T)
plot(ducks_pca, scaling = 2, type = "n", xlab = "axis 1", ylab = "axis 2")
segments(x0 = 0, 
         y0 = 0, 
         x1 = scores(ducks_pca, display = "species", scaling = 2)[, 1], 
         y1 = scores(ducks_pca, display = "species", scaling = 2)[, 2])
text(ducks_pca, display = "sp", scaling = 2, col = 2)
text(ducks_pca, display = "wa", labels = ducks[, 1], scaling = 2, pch = 19)




#      Figure 16.12
CPUE <- read.table("fish.txt", header = T, sep = "\t")
plot(CPUE$Year, CPUE$Station3, type = "l", 
     xlab = "Year", ylab = "CPUE")
first_diff <- diff(CPUE$Station3, lag = 1)
plot(CPUE$Year[2:nrow(CPUE)], first_diff, type = "l", 
     xlab = "Year", ylab = "CPUE")




#      Figure 16.13  A and B
NAO_SST <- read.table("Scottish_SST.txt", header = T, 
                      sep = "\t", dec = ".")
par(mfrow = c(2, 1))
plot(NAO_SST$SST, type = "l", 
     xlab = "Time lag", ylab = "SST")
acf(NAO_SST$SST, main = "", ylab = "ACF")
par(mfrow = c(1, 1))



#      Figure 16.13  C and D
NAO_SST <- read.table("Scottish_SST.txt", header = T, 
                      sep = "\t", dec = ".")
par(mfrow = c(2, 1))
seas_diff <- diff(NAO_SST$SST, lag = 12)
plot(seas_diff, type = "l", 
     xlab = "Year", ylab = "diff(SST, lag = 12)")
acf(seas_diff, main = "", ylab = "ACF")
par(mfrow = c(1, 1))



#      Figure 16.13  E and F
NAO_SST <- read.table("Scottish_SST.txt", header = T, 
                      sep = "\t", dec = ".")
par(mfrow = c(2, 1))
first_seas_diff <- diff(diff(NAO_SST$SST, lag = 12), lag = 1)
plot(first_seas_diff, type = "l", 
     xlab = "Year", ylab = "diff(diff(SST, lag = 12))")
acf(first_seas_diff, main = "", ylab = "ACF")
par(mfrow = c(1, 1))




#      Table 16.4
NAO_SST <- read.table("Scottish_SST.txt", header = T, 
                      sep = "\t", dec = ".")
first_seas_diff <- diff(diff(NAO_SST$SST, lag = 12), lag = 1)
library(nlme)
for (p in 1:14) {
     temp_model <- arima(first_seas_diff, order = c(p, 0, 0))
     cat("p  = ", p, "\t", "AIC  = ", AIC(temp_model), "\n")
}



#      Table 16.5
NAO_SST <- read.table("Scottish_SST.txt", header = T, 
                      sep = "\t", dec = ".")
first_seas_diff <- diff(diff(NAO_SST$SST, lag = 12), lag = 1)
library(nlme)
for (p in 1:14) {
     for (q in 1:3) {
          temp_model <- arima(first_seas_diff, order = c(p, 0, q))
          cat("p  = ", p, "\t", "q  = ", q, "\t", 
              "AIC  = ", AIC(temp_model), "\n")
}
}




#      Table 16.6
NAO_SST <- read.table("Scottish_SST.txt", header = T, 
                      sep = "\t", dec = ".")
first_seas_diff <- diff(diff(NAO_SST$SST, lag = 12), lag = 1)
library(nlme)
model_14_3 <- arima(first_seas_diff, order = c(14, 0, 3))
model_14_3




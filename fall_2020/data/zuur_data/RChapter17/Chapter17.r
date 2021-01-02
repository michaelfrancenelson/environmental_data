#    R code for: Chapter 17 in:
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



setwd("D:\\applicat\\HighlandStatistics\\Book\\R\\RChapter17\\")




par(mar = c(4.5, 4.5, 0.5, 1.5), cex.lab = 1.5, cex.axis = 1.2)


#       Figure 17.1 and 17.2
CPUE <- read.table("fish.txt", header = T, sep = "\t")
#Repeated LOESS smoothing was programmed in the software
#package Brodgar (www.brodgar.com)




#      Figure 17.6
library(lattice)
SSTNA <- read.table(file = "SSTNorthAmerica.txt", header = TRUE)
fig_17_6<-matrix(ncol = 4, nrow = 1)
source("Ch_17_Library.r")
fig_17_6<-rbind(acor(SSTNA$SST_22_24, num_of_lags = 60, plotf = F), 
                acor(SSTNA$SST_24_26, num_of_lags = 60, plotf = F), 
                acor(SSTNA$SST_26_28, num_of_lags = 60, plotf = F), 
                acor(SSTNA$SST_28_30, num_of_lags = 60, plotf = F), 
                acor(SSTNA$SST_30_32, num_of_lags = 60, plotf = F), 
                acor(SSTNA$SST_32_34, num_of_lags = 60, plotf = F), 
                acor(SSTNA$SST_34_36, num_of_lags = 60, plotf = F), 
                acor(SSTNA$SST_36_38, num_of_lags = 60, plotf = F), 
                acor(SSTNA$SST_38_40, num_of_lags = 60, plotf = F), 
                acor(SSTNA$SST_40_42, num_of_lags = 60, plotf = F), 
                acor(SSTNA$SST_42_44, num_of_lags = 60, plotf = F), 
                acor(SSTNA$SST_44_46, num_of_lags = 60, plotf = F), 
                acor(SSTNA$SST_46_48, num_of_lags = 60, plotf = F), 
                acor(SSTNA$SST_Bight, num_of_lags = 60, plotf = F))
fig_17_6$SST<-rep(c("SST_22_24", "SST_24_26", "SST_26_28", 
                    "SST_28_30", "SST_30_32", "SST_32_34", 
                    "SST_34_36", "SST_36_38", "SST_38_40", 
                    "SST_40_42", "SST_42_44", "SST_44_46", 
                    "SST_46_48", "SST_Bight"), each = 61)
fig_17_6$xline<-rep(0, times = nrow(fig_17_6))
xyplot(sample_autocor+limit_hi+limit_lo+xline ~ lag_k|SST, 
       data = fig_17_6, xlab = "Time lags", ylab = "Correlations", 
       type = "l", lty = c(1, 2, 2, 1), col = "black")



 #      Figure 17.7
SSTNA <- read.table(file = "SSTNorthAmerica.txt", header = TRUE)
par(mfrow = c(4, 1), mar = c(4.5, 4.5, 0.5, 0.5))

library(vegan)
SST_22_24<-decostand(SSTNA$SST_22_24, "standardize", na.rm = T)
AxisTime<-SSTNA$AxisYear-SSTNA$Year[1]
plot(AxisTime, SST_22_24, 
     type = "l", xlab = "time", ylab = "data")
#     to calculate seasonal component
#     the mean value per month is calculated 
#     then values are concatenated to unite time series 
month_mean<-tapply(SST_22_24, INDEX = SSTNA$Month, 
                   FUN = function(x) mean(x, na.rm = T))
seasonal<-rep(month_mean, length = length(SST_22_24))
plot(AxisTime, seasonal, 
    type = "l", xlab = "time", ylab = "seasonal")

trend_data<-SST_22_24-seasonal
trend_loess<-loess(trend_data ~ AxisTime, span = 0.07)
trend<-predict(trend_loess, data.frame(AxisTime))
plot(AxisTime, trend, 
     type = "l", xlab = "time", ylab = "trend")

remainder<-SST_22_24-seasonal-trend
plot(AxisTime, remainder, 
     type = "h", xlab = "time", ylab = "remainder")
abline(h = 0)
par(mfrow = c(1, 1))



 #      Figure 17.8
SSTNA <- read.table(file = "SSTNorthAmerica.txt", header = TRUE)
hi<-order(SSTNA$AxisYear, SSTNA$Month)
SSTNA<-SSTNA[hi, ]
par(mfrow = c(4, 1), mar = c(4.5, 4.5, 0.5, 0.5))

library(vegan)
SST_22_24<-decostand(SSTNA$SST_22_24, "standardize", na.rm = T)
AxisTime<-SSTNA$AxisYear-SSTNA$Year[1]
plot(AxisTime, SST_22_24, 
     type = "l", xlab = "time", ylab = "data")
#     to calculate seasonal component
#     loess smoothing is applied on monthly time series 
#     (on January time series, February ts and so on)
month_mean<-tapply(SST_22_24, INDEX = SSTNA$Month, 
                   FUN = function(x) loess(x ~ unique(SSTNA$Year), 
                                         span = 0.3, 
                                         na.action = na.omit))
seasonal<-matrix(nrow = length(AxisTime))
for (month in 1:12) {
     seasonal[SSTNA$Month == month]<-predict(month_mean[[month]], 
                                   newdata = data.frame(Year = 
                                           unique(SSTNA$Year)))
}
plot(AxisTime, seasonal, 
    type = "l", xlab = "time", ylab = "seasonal")

trend_data<-SST_22_24-seasonal
trend_loess<-loess(trend_data ~ AxisTime, span = 0.07)
trend<-predict(trend_loess, data.frame(AxisTime))
plot(AxisTime, trend, 
    type = "l", xlab = "time", ylab = "trend")

remainder<-SST_22_24-seasonal-trend
plot(AxisTime, remainder, 
    type = "h", xlab = "time", ylab = "remainder")
abline(h = 0)
par(mfrow = c(1, 1))



#      Figure 17.9
SSTNA <- read.table(file = "SSTNorthAmerica.txt", header = TRUE)
hi<-order(SSTNA$AxisYear, SSTNA$Month)
all_trend<-matrix(ncol = 13, nrow = length(SSTNA$AxisYear))
SSTNA<-SSTNA[hi, ]
library(vegan)
for (i in 1:13) {
     series<-decostand(SSTNA[, i+6], "standardize", na.rm = T)
     AxisTime<-SSTNA$AxisYear-SSTNA$Year[1]
     month_mean<-tapply(series, INDEX = SSTNA$Month, 
                        FUN = function(x) loess(x ~ unique(SSTNA$Year), 
                                                span = 0.3, 
                                                na.action = na.omit))
     seasonal<-matrix(nrow = length(AxisTime))
     for (month in 1:12) {
          seasonal[SSTNA$Month == month]<-predict(month_mean[[month]], 
                                        newdata = data.frame(Year = 
                                                unique(SSTNA$Year)))
     }
     trend_data<-series-seasonal
     trend_loess<-loess(trend_data ~ AxisTime, span = 0.07)
     all_trend[, i]<-predict(trend_loess, data.frame(AxisTime))
}
plot(c(min(AxisTime), max(AxisTime)), 
     c(min(all_trend), max(all_trend)), 
     type = "n", xlab = "Time", ylab = "Trends")
for (i in 1:13) {
     lines(AxisTime, all_trend[, i], col = i)
}



#Section 17.3
#MAFA was programmed in FORTRAN, and is available from Brodgar
#www.brodgar.com
#To the best of our knowledge, no R code exists


#Section 17.4
#DFA was programmed in FORTRAN, and is available from Brodgar
#www.brodgar.com
#To the best of our knowledge, no R code exists



#     Figure 17.21
CPUE <- read.table("fish.txt", header = T, sep = "\t", row.names=1)
library(vegan)
nephrops <- decostand(CPUE[,1:11], "normalize", MARGIN = 2, na.rm = T)
nephrops <- nephrops[rowSums(is.na(nephrops))<11,]
diss_matrix <- dist(nephrops, "euclidean")
clust_result <- hclust(diss_matrix, "average")
plot(clust_result)



#Section 17.5
#Chronological clusteriung was programmed in FORTRAN, and is
#available in Brodgar
#www.brodgar.com
#To the best of our knowledge, no R code exists

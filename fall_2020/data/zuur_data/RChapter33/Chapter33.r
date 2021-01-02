#    R code for: Chapter 33 in:
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

#    This file was produced by Alain Zuur
#    (highstat@highstat.com)


setwd("D:\\applicat\\HighlandStatistics\\Book\\R\\RChapter33\\")


#############################
#
#     Data exploration. 


Algarve <- read.table("Algarve1982-1999.txt", header = TRUE)
names(Algarve)

# [1] "Year"    "Ee"      "Tt"      "Sj"      "Sp"      "Lc"      "Mm"
# [8] "Bb"      "Pb"      "Ar"      "Ms"      "So"      "Ov"      "Fishers"
#[15] "UI"      "NAO"     "SST"     "PPT1"    "F0"      "Boats"

##Fishermen		                             Fishers
##Upwelling index		                       UI
##NAO Hurrell Ponto Delgada		             NAO
##Mean annual SST at 7.5W 37.5N		         SST
##Faro annual rainfall		                 PPT1
##Annual (Jan-Dec) Guadiana river flow		 F0



Fish <- Algarve[, 2:13]
ExplVar <- Algarve[, 14:20]
Time <- Algarve[,1]
names(Fish)
names(ExplVar)


#      Figure 31.1
library(lattice)

#Put the data in vectors
Fish12 <- c(Fish[,1],Fish[,2],Fish[,3],Fish[,4],Fish[,5],Fish[,6],
            Fish[,7],Fish[,8],Fish[,9],Fish[,10],Fish[,11],Fish[,12])
Time12 <- rep(Time, 12)
ID12 <- rep(names(Fish), each = length(Time))

xyplot(Fish12 ~ Time12 | factor(ID12), type="l",
    xlab = "Time", ylab = "Fisheries time series",
    scales = list(alternating = TRUE,
    x = list(relation = "same"),
    y = list(relation = "free")),
    col=1)



#Figure 33.2

X7 <- c(scale(ExplVar[,1]),scale(ExplVar[,2]),scale(ExplVar[,3]),
        scale(ExplVar[,4]),scale(ExplVar[,5]), scale(Fish[,6]),
        scale(ExplVar[,7]))
Time7 <- rep(Time, 7)
ID7 <- rep(names(ExplVar), each = length(Time))

xyplot(X7 ~ Time7 | factor(ID7), type="l",
    xlab = "Time", ylab = "Explanatory variables",
    scales = list(alternating = TRUE,
    x = list(relation = "same"),
    y = list(relation = "free")),
    col=1)


#Figure 33.2
source("MyLibrary.r")
pairs(ExplVar, lower.panel = panel.cor)

#See our R survival guide or other chapters for adding smoothers in
#the upper right panel



#Sections 33.3 - 33.6
#MAFA and DFA were implemented in Brodgar. The underlying code for DFA
#was written in FORTRAN, and contains thousands of lines. It would take
#too much time to implement it in R. Hence, you will need to purchase
#Brodgar to do the DFA, see www.brodgar.com
#The same story holds for MAFA, although you should be able to
#program MAFA yourself in R. It is not that difficult. It will take
#half a day if you are an experienced R user and have a copy of the
#Solow paper.
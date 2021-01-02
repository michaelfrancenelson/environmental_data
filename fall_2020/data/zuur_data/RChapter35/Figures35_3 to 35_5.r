#    R code for: Chapter 35 in:
#    Analysing Ecological Data. (2007). Zuur,  Ieno and Smith. Springer,  680p.
#    This file was produced by Alain Zuur (highstat@highstat.com)
#    www.highstat.com

#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License,  or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,  
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.



setwd("D:\\applicat\\HighlandStatistics\\Book\\R\\RChapter35")

par(mar = c(4.5,  4.5,  0.5,  0.5),  cex.lab = 1,  cex.axis = 1)

Y <- read.table("Saltmarsh.txt", header = T, row.names=1)



#      Figure 35.3
 plot(Y[,1], Y[,20], type="l", xlab="Year", ylab="MHT (mm)")
 points(Y[,1], Y[,20], cex=1.5)



#      Figure 35.5 
a<-princomp(Y[,2:19], cor=T)
biplot(a)
lines(c(-10,10),c(0,0), lty=2)
lines(c(0,0),c(-10,10), lty=2)
#Note: The book uses the software package Brodgar
#which produces a graph with a slightly different format
#The actual biplot is the same though.



summary(a)
# The first two eigenvalues are corresponding to 66% 
# of the variation in the data 
# (see Cumulativ Proportion for Comp. 2). 
# Here Comp.1, Comp.2, etc. - eigenvalues. 
 
 
 
 
 
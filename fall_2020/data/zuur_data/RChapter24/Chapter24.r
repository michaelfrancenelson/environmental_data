#    R code for: Chapter 24 in:
#    Analysing Ecological Data. (2007). Zuur, Ieno and Smith. Springer, 680p.
#    This file was produced by Erik Meesters and Alain Zuur
#    (highstat@highstat.com  www.highstat.com)

#    This program is free software; you can redistribute it and/or modify
#    it under the terms of the GNU General Public License as published by
#    the Free Software Foundation; either version 2 of the License, or
#    (at your option) any later version.
#
#    This program is distributed in the hope that it will be useful,
#    but WITHOUT ANY WARRANTY; without even the implied warranty of
#    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#    GNU General Public License for more details.


## Results may be slightly different because of version differences and/or
## differences from cross validation.

setwd("D:\\applicat\\HighlandStatistics\\Book\\R\\RChapter24\\")

#Get the data:
TreeData<-read.table(file="Radar_tree_data.txt",header=TRUE)


##Or directly from Excel using RODBC
#library(RODBC)
#xls<-"Radar_tree_data.xls"
#channel1<-odbcConnectExcel(xls)
#TreeData<-sqlFetch(channel1,"treedata")


names(TreeData)

# Table 24.2
table(TreeData$groups)

# Exclude groups with too few observations
TreeData.1 <- TreeData[TreeData$exclude == 0,]

library(rpart)
# Setting up the tree
tree1 <- rpart(g9 ~ EPT + TKQ + TKT + AVV + VEL + MXA + AREA + MAXREF + TRKDIS +
          MAXSEG + ORIENT + ELLRATIO + ELONG + COMPACT + CHY + MAXREF1 + MINREF +
          SDREF, data = TreeData.1, method = "class", control = rpart.control(cp = 0.001))

# Determine number of branches
plotcp(tree1)          
printcp(tree1, digits = 3)

# Final tree
tree1a <- prune(tree1,cp = 0.0323)
printcp(tree1a,digits=3)
tree1a

summary(tree1a)

# Plotting the tree
windows(15,10)

plot(tree1a, compress = F, margin = 0.5)
  text(tree1a, use.n = TRUE)

# 24.5 A tree for birds, clutter and more clutter
tree3 <- rpart(g4 ~ EPT + TKQ + TKT + AVV + VEL + MXA + AREA + MAXREF + TRKDIS +
          MAXSEG + ORIENT + ELLRATIO + ELONG + COMPACT + CHY + MAXREF1 + MINREF +
          SDREF, data = TreeData.1, method = "class", control = rpart.control(cp = 0.001))
plotcp(tree3)          

printcp(tree3, digits = 3)

tree3a <- prune(tree3, cp = 0.035)

tree3a

summary(tree3a)

plot(tree3a,compress=F,margin=0.1)
    text(tree3a, use.n = T, cex = .8)


# Or try newer library mvpart
library(mvpart)
tree2 <- rpart(g9 ~ EPT + TKQ + TKT + AVV + VEL + MXA + AREA + MAXREF + TRKDIS +
          MAXSEG + ORIENT + ELLRATIO + ELONG + COMPACT + CHY + MAXREF1 + MINREF +
          SDREF, data = TreeData.1, method = "class", control = rpart.control(cp = 0.001))

plotcp(tree2)          

printcp(tree2, digits = 3)

tree2a <- prune(tree2, cp = 0.0323)

plot(tree2a,compress=F,margin=0.1)
text(tree2a, use.n = T, cex = .8, bord = T, which = 2, legend = T)


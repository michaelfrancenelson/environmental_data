#    R code for: Chapter 23 in:
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


setwd("D:\\applicat\\HighlandStatistics\\Book\\R\\RChapter23\\")

#Figure 23.2
RFBirds<-read.table("RiceFieldBirds.txt",header=T)
names(RFBirds)
dotchart(RFBirds$AQBIRDS)
#Note: This is the same data, but ordered in a slightly different way.


#Figure 23.3
#REMOVE 3 SAMPLES WITH LARGE DEPTH: dotchart(RFBirds$DEPTH)
I <- RFBirds$DEPTH < 60
RFBirds2 <- RFBirds[I, ]

RFBirds2$LogAQBirds <- log10(RFBirds2$AQBIRDS + 1)
coplot(LogAQBirds ~ DEPTH | SPTREAT * factor(YEAR),
       panel = panel.smooth,span=0.7,col.smooth=1,
       xlab = "DEPTH", ylab="AQBIRDS",
       data = RFBirds2, number=4 )

#The graph in the book was created with Brodgar. In this package,
#you need to label the levels of SPTREAT as 1, 2, 3, etc. Here, we
#use the original labels. Because of this, the order of the
#panels is slightly different, but essentially the graph is the same.
#Use the levels option of the factor command if you want to have
#exactly the order of the panels as in the book.


#Figure 23.4
#Note: The word "Census" is (wrongly) used for survey in the textfile.
boxplot(RFBirds2$LogAQBirds ~ RFBirds2$CENSUS * RFBirds2$YEAR)


#Figure 23.5A
RFBirds<-read.table("RiceFieldBirds.txt",header=T)
RFBirds2<-data.frame(
    LogAQBirds = log10(RFBirds$AQBIRDS+1),
    fCensus    = factor(RFBirds$CENSUS),
    Time       = RFBirds$CENSUS,
    fBlock     = factor(RFBirds$BLOCK),
    Depth      = RFBirds$DEPTH,
    fSite      = factor(RFBirds$SITE),
    DepthSQ    = RFBirds$DEPTH^2,
    fSptreat   = factor(RFBirds$SPTREAT),
    fYear      = factor(RFBirds$YEAR),
    fMonth     = factor(RFBirds$Data),
    fField     = factor(RFBirds$FIELDNEW),
    fCheck     = factor(RFBirds$CHECK))

I4 <- RFBirds2$Depth<60  #REMOVE 3 SAMPLES WITH LARGE DEPTH
I3 <- RFBirds2$fYear=="1"   # Use data from year 1
I5 <- RFBirds2$fField=="304" & RFBirds2$fCheck=="4"  #Remove these two fields/checks
RFBirds3 <- RFBirds2[I4 & I3 & !I5,]
library(mgcv)
#Year 1 data
M1 <- gam(LogAQBirds ~ s(Depth) + fSptreat + fCensus, data = RFBirds3)
plot(M1)

#Year 2 data: Run the same code as above, but change I3 to:
#I3 <- RFBirds2$fYear=="2"
#This will give figure 23.5B


#In the remaining part of the text/code/chapter, we use data from year 1.

#Model M0 on page 425 (gives an error message)
library(nlme)
M.lmeNA<-lme(LogAQBirds ~ Depth + I(Depth^2) + fSptreat + fBlock,
             random= ~1| fField, data = RFBirds3)


#Model M1 on page 426:
M.1<-lme(LogAQBirds ~ Depth + I(Depth^2) + fSptreat ,
             random= ~1| fField, data = RFBirds3)
plot(M.1)
summary(M.1)
E<-residuals(M.1,type="normalized")
par(mfrow=c(2,2))   #This is all standard model validation
plot(RFBirds3$Depth,E)
plot(RFBirds3$fBlock,E)
plot(RFBirds3$fSptreat,E)


#Figure 23.6
par(mar=c(4,4,2,2))
plot(RFBirds3$fCensus,E,ylab="Residuals",xlab="Survey",cex.lab=1.5)


#Model M2 on page 426:
M.2<-lme(LogAQBirds ~ Depth + I(Depth^2) + fSptreat + fCensus ,
             random= ~1| fField, data = RFBirds3)

#Figure 23.7
E<-residuals(M.2,type="normalized")
par(mar=c(4,4,2,2))
plot(RFBirds3$fCensus,E,ylab="Residuals",xlab="Survey",cex.lab=1.5)


#Model M3 on page 426:

M.3<-lme(LogAQBirds ~ Depth + I(Depth^2) + fSptreat + fCensus ,
             random= ~1 | fField, data = RFBirds3,
             weights=varIdent(form= ~ 1 | fCensus))


#First anova table on page 428
anova(M.2, M.3)
#> anova(M.2, M.3)
#    Model df      AIC      BIC     logLik   Test  L.Ratio p-value
#M.2     1 20 2135.853 2237.968 -1047.9264
#M.3     2 31 1997.534 2155.813  -967.7668 1 vs 2 160.3191  <.0001
#The analysis in the book was done in 2005, with a considerble older
#R version (I think it was 1.6). Output above is from R version 2.7.


#Following the text on page 428:
M.4<-lme(LogAQBirds ~ Depth + I(Depth^2) + fSptreat + fCensus ,
             random= ~1 | fField, data = RFBirds3,
             weights=varIdent(form= ~ 1 | fSptreat))
anova(M.2, M.4)
#> anova(M.2, M.4)
#    Model df      AIC      BIC    logLik   Test  L.Ratio p-value
#M.2     1 20 2135.853 2237.968 -1047.926
#M.4     2 24 2136.361 2258.900 -1044.181 1 vs 2 7.491654  0.1121
#Note the small difference due to different R versions (I guess..unless I
#a mistake somewhere)


#Add an auto-correlation structure
cs0 <- corARMA(c(0.2), form = ~ Time | fField/fCheck, p = 1, q = 0)
T1 <- !((RFBirds3$fField == 302 & (RFBirds3$fCheck==5 | RFBirds3$fCheck==6))  |
        (RFBirds3$fField == 305 & ((RFBirds3$fCheck == 2 | RFBirds3$fCheck == 3 | RFBirds3$fCheck == 4 | RFBirds3$fCheck == 5))))
#You have to remove some fields and checks due to the design of the data.
#Vandalism by muskrats, see end of page 428

M.3A<-lme(LogAQBirds ~ Depth + I(Depth^2) + fSptreat + fCensus ,
             random= ~1 | fField, data = RFBirds3,
             weights=varIdent(form= ~ 1 | fCensus),
             subset=T1)
             
M.5A<-lme(LogAQBirds ~ Depth + I(Depth^2) + fSptreat + fCensus ,
             random= ~1 | fField, data = RFBirds3,
             weights=varIdent(form= ~ 1 | fCensus),
             subset=T1,
             correlation=cs0)

anova(M.3A,M.5A)
#     Model df      AIC      BIC    logLik   Test  L.Ratio p-value
#M.3A     1 31 1925.329 2081.883 -931.6644
#M.5A     2 32 1866.602 2028.206 -901.3011 1 vs 2 60.72658  <.0001

summary(M.5A)

#Correlation Structure: ARMA(1,0)
# Formula: ~Time | fField/fCheck
# Parameter estimate(s):
#     Phi1
#0.2740109


#Dropping fixed terms from the model
cs0 <- corARMA(c(0.274),form = ~ Time | fField/fCheck,p=1,q=0,fix=TRUE)   #Year 1
M.5<-lme(LogAQBirds ~ Depth + I(Depth^2) + fSptreat + fCensus ,
             random= ~1 | fField, data = RFBirds3,
             weights=varIdent(form= ~ 1 | fCensus),
             subset=T1,
             correlation=cs0,
             method="ML")

lmc<-lmeControl(niterEM=5000,msMaxIter=1000)
M.6 <- update(M.5, .~. - fCensus,control=lmc)
M.7 <- update(M.5, .~. - fSptreat)
M.8 <- update(M.5, .~. + fSptreat:Depth)
M.9 <- update(M.5, .~. - I(Depth^2))
M.10 <- update(M.5, .~. - Depth - I(Depth^2))


# Table 23.2
anova(M.5,M.6)
anova(M.5,M.7)
anova(M.5,M.8)
anova(M.5,M.9)
anova(M.5,M.10)
#Odd...now I have exactly the same results as in the book!
#Must have made a mistake in the book on page 428?


#Figure 23.8
plot(M.8)

E<-residuals(M.8,type="normalized")
hist(E,nclass=20)

library(lattice)
trellis.device(new=F)
background <- trellis.par.get("background")
background$col<-"white"
trellis.par.set("background",background)
plot(M.8,col=1)



#Numerical output in Section 23.7
M.8<-lme(LogAQBirds ~ Depth + I(Depth^2) + fSptreat + fCensus + fSptreat:Depth,
             random= ~1 | fField, data = RFBirds3,
             weights=varIdent(form= ~ 1 | fCensus),
             subset=T1,
             correlation=cs0,
             method="REML")

summary(M.8)
#Results are nearly identical as in the book:
#Correlation Structure: ARMA(1,0)
# Formula: ~Time | fField/fCheck
# Parameter estimate(s):
# Phi1
#0.274
#Variance function:
# Structure: Different standard deviations per stratum
# Formula: ~1 | fCensus
# Parameter estimates:
#        1         3         4         5         6        12        11         9        10         8         7         2
#1.0000000 1.3424466 1.4067120 1.3198019 1.4387185 0.3517652 0.7165251 1.2885885 1.0721044 1.2762587 1.2085159 1.3891901
#Fixed effects: LogAQBirds ~ Depth + I(Depth^2) + fSptreat + fCensus + fSptreat:Depth
#                          Value  Std.Error   DF   t-value p-value
#(Intercept)           0.1385318 0.12633983 1118  1.096501  0.2731
#Depth                 0.0365910 0.00980253 1118  3.732806  0.0002
#I(Depth^2)           -0.0005452 0.00021955 1118 -2.483307  0.0132
#fSptreatfldrl        -0.1288419 0.09449907   31 -1.363420  0.1826
#fSptreatincfld       -0.1603500 0.14734598   31 -1.088255  0.2849
#fSptreatrlfld        -0.0735918 0.08129673   31 -0.905225  0.3723
#fSptreatrmvfld       -0.1489260 0.14144136   31 -1.052917  0.3005
#fCensus2              0.1372059 0.07761936 1118  1.767676  0.0774
#fCensus3              0.1859873 0.07592468 1118  2.449630  0.0145
#fCensus4              0.2781485 0.08385590 1118  3.316981  0.0009
#fCensus5              0.4798481 0.08092094 1118  5.929838  0.0000
#fCensus6              0.6974404 0.08701673 1118  8.015016  0.0000
#fCensus7              0.6555820 0.07910782 1118  8.287196  0.0000
#fCensus8              0.6221966 0.08277601 1118  7.516630  0.0000
#fCensus9              0.4171447 0.08718071 1118  4.784828  0.0000
#fCensus10             0.4267281 0.10390563 1118  4.106881  0.0000
#fCensus11             0.1173667 0.09791975 1118  1.198601  0.2309
#fCensus12            -0.0318265 0.09322669 1118 -0.341388  0.7329
#Depth:fSptreatfldrl  -0.0107542 0.00598384 1118 -1.797199  0.0726
#Depth:fSptreatincfld  0.0263719 0.02010665 1118  1.311602  0.1899
#Depth:fSptreatrlfld  -0.0055091 0.00549314 1118 -1.002897  0.3161
#Depth:fSptreatrmvfld -0.0207014 0.01060124 1118 -1.952734  0.0511







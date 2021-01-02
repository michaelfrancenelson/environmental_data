#    R code for: Chapter 36 in:
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



setwd("D:\\applicat\\HighlandStatistics\\Book\\R\\RChapter36\\")


library(mgcv)
library(nlme)
library(lattice)


Hawaii <- read.table("waterbirdislandseries.txt", header = TRUE)
names(Hawaii)


# [1] "Year"               "Stilt_Oahu"         "Stilt_Maui"
# [4] "Stilt_Kauai_Niihau" "Coot_Oahu"          "Coot_Maui"
# [7] "Coot_Kauai_Niihau"  "Moorhen_Oahu"       "Moorhen_Kauai"
#[10] "Rainfall"



Birds <- c(scale(sqrt(Hawaii$Stilt_Oahu)),
           scale(sqrt(Hawaii$Stilt_Maui)),
           scale(sqrt(Hawaii$Stilt_Kauai_Niihau)),
           scale(sqrt(Hawaii$Coot_Oahu)),
           scale(sqrt(Hawaii$Coot_Maui)),
           scale(sqrt(Hawaii$Coot_Kauai_Niihau)),
           scale(sqrt(Hawaii$Moorhen_Oahu)),
           scale(sqrt(Hawaii$Moorhen_Kauai)))
#This is a bit clumsy R programming

           
Time <- rep(Hawaii$Year, 8)
ID <- factor(rep(names(Hawaii[,2:9]), each = length(Hawaii[,1])),
             levels=c(
             "Stilt_Oahu",
             "Stilt_Kauai_Niihau",
             "Stilt_Maui",
             "Coot_Oahu",
             "Coot_Kauai_Niihau",
             "Coot_Maui",
             "Moorhen_Oahu",
             "Moorhen_Kauai"))

#Figure 36.1
xyplot(Birds ~ Time|ID, ylab = "Normalised time series",
       scales = list(x = list(relation = "same"), 
                     y = list(relation = "same")),
       layout = c(3, 3), type = "l", col = 1)


a1 <- acf(sqrt(Hawaii$Stilt_Oahu), lag.max=15, na.action=na.pass)
a2 <- acf(sqrt(Hawaii$Stilt_Maui), lag.max=15, na.action=na.pass)
a3 <- acf(sqrt(Hawaii$Stilt_Kauai_Niihau), lag.max=15, na.action=na.pass)
a4 <- acf(sqrt(Hawaii$Coot_Oahu), lag.max=15, na.action=na.pass)
a5 <- acf(sqrt(Hawaii$Coot_Maui), lag.max=15, na.action=na.pass)
a6 <- acf(sqrt(Hawaii$Coot_Kauai_Niihau), lag.max=15, na.action=na.pass)
a7 <- acf(sqrt(Hawaii$Moorhen_Oahu), lag.max=15, na.action=na.pass)
a8 <- acf(sqrt(Hawaii$Moorhen_Kauai), lag.max=15, na.action=na.pass)

AllACFs<-c(a1$acf,a2$acf,a3$acf,a4$acf,a5$acf,a6$acf,a7$acf,a8$acf)
AllLags <- rep(1:16, 8)
IDLags <- factor(rep(names(Hawaii[,2:9]), each = 16),
             levels=c(
             "Stilt_Oahu",
             "Stilt_Kauai_Niihau",
             "Stilt_Maui",
             "Coot_Oahu",
             "Coot_Kauai_Niihau",
             "Coot_Maui",
             "Moorhen_Oahu",
             "Moorhen_Kauai"))

ttot <- length(Hawaii$Year)
xyplot(AllACFs ~ AllLags|IDLags, ylab = "Correlation",
       xlab = "Time lags",
       scales = list(x = list(relation = "same"),
                     y = list(relation = "same")),
       layout = c(3, 3), type = "l", col = 1,
       ylim=c(-1,1),
       panel = function(x,y) {
                    llines(x,y,col=1)
                    SE05<-rep(2/sqrt(ttot),length(x))
                    ZeroLine<-rep(0,length(x))
                    llines(x,SE05,col=1,lty=2)
                    llines(x,-1*SE05,col=1,lty=2)
                    llines(x,ZeroLine,col=1,lty=1)} )


#Section 36.4
#All methods in this section were implemented in the the software
#package Brodgar (www.brodgar.com). It would take too much time
#to program them again in R. See also the comments in Chapter 33.


#Section 36.5

#Let us first organise the data again (this is copy-paste from
#above)



######################
## DO SOME EXTRA CHECKING FOR SMOOTHER INTERACTIONS
Y<-read.table("waterbirdislandseriesInVectorFormat.txt",header=T)

names(Y)
attach(Y)
library(mgcv)
#[1] "Year"       "Year.1"     "Birds"      "ID"         "Rainfall"
#[6] "ListEffect" "Species"    "Island"     "C75"

#Species: 1 = Stlt, 2 = Coot, 3 = Moorhen
#Island: 1 = Oaho, 2 = Maui, 3 = Kauai Niihau


#Normalise each time series
for (i in 1:8){
  Birds[ID==i]<-(Birds[ID==i]-mean(Birds[ID==i],na.rm=T))/(sqrt(var(Birds[ID==i],na.rm=T)))
}

S1<-as.numeric(ID==1)
S2<-as.numeric(ID==2)
S3<-as.numeric(ID==3)
S4<-as.numeric(ID==4)
S5<-as.numeric(ID==5)
S6<-as.numeric(ID==6)
S7<-as.numeric(ID==7)
S8<-as.numeric(ID==8)

tmp1<-gamm(Birds ~ s(Rainfall, fx = F, k = -1,by=S1) +
                s(Rainfall, fx = F, k = -1,by=S2) +
                s(Rainfall, fx = F, k = -1,by=S3) +
                s(Rainfall, fx = F, k = -1,by=S4) +
                s(Rainfall, fx = F, k = -1,by=S5) +
                s(Rainfall, fx = F, k = -1,by=S6) +
                s(Rainfall, fx = F, k = -1,by=S7) +
                s(Rainfall, fx = F, k = -1,by=S8) +
                s(Year, fx = F, k = -1,by=S1) +
                s(Year, fx = F, k = -1,by=S2) +
                s(Year, fx = F, k = -1,by=S3) +
                s(Year, fx = F, k = -1,by=S4) +
                s(Year, fx = F, k = -1,by=S5) +
                s(Year, fx = F, k = -1,by=S6) +
                s(Year, fx = F, k = -1,by=S7) +
                s(Year, fx = F, k = -1,by=S8) ,
                family=gaussian,correlation=corAR1(form=~Year|ID))

summary(tmp1$gam)

#VERSUS
tmp2<-gamm(Birds ~ s(Rainfall, fx = F, k = -1) +
                   s(Year, fx = F, k = -1) ,
                family=gaussian,correlation=corAR1(form=~Year|ID))



anova(tmp1$lme,tmp2$lme)

######## DIFFERENCE BETWEEN SPECIES????
S1<-as.numeric(Species==1)
S2<-as.numeric(Species==2)
S3<-as.numeric(Species==3)

tmp3<-gamm(Birds ~ s(Rainfall, fx = F, k = -1,by=S1) +
                s(Rainfall, fx = F, k = -1,by=S2) +
                s(Rainfall, fx = F, k = -1,by=S3) +
                s(Year, fx = F, k = -1,by=S1) +
                s(Year, fx = F, k = -1,by=S2) +
                s(Year, fx = F, k = -1,by=S3) ,
                family=gaussian,correlation=corAR1(form=~Year|ID))


anova(tmp3$lme,tmp2$lme)
#p=0.17


######## DIFFERENCE BETWEEN Islands????

S1<-as.numeric(Island==1)
S2<-as.numeric(Island==2)
S3<-as.numeric(Island==3)

tmp4<-gamm(Birds ~ s(Rainfall, fx = F, k = -1,by=S1) +
                s(Rainfall, fx = F, k = -1,by=S2) +
                s(Rainfall, fx = F, k = -1,by=S3) +
                s(Year, fx = F, k = -1,by=S1) +
                s(Year, fx = F, k = -1,by=S2) +
                s(Year, fx = F, k = -1,by=S3) ,
                family=gaussian,correlation=corAR1(form=~Year|ID))


anova(tmp4$lme,tmp2$lme)

#Instead of all these anova comparisons, perhaps the use of the AIC
#is better.
AIC(tmp4$lme,tmp2$lme)


anova(tmp4$gam)
#Approximate significance of smooth terms:
#                 edf Est.rank      F  p-value
#s(Rainfall):S1 2.462    5.000  2.945 0.013024
#s(Rainfall):S2 1.000    1.000 13.310 0.000312
#s(Rainfall):S3 3.656    8.000  5.409 2.28e-06
#s(Year):S1     4.640    9.000  4.958 3.22e-06
#s(Year):S2     1.000    1.000 20.591 8.27e-06
#s(Year):S3     1.000    1.000  0.293 0.588588


#Figure 36.7  (In the book, we copied graph by graph to Word
par(mfrow=c(3,2))
plot(tmp4$gam)


#Section 36.6
#Brodgar was used for these calculations. It would take too
#much effort to program this in R
#See: www.brodgar.com


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




setwd("D:\\applicat\\HighlandStatistics\\Book\\R\\RChapter35\\")

par(mar = c(4.5,  4.5,  0.5,  0.5),  cex.lab = 1.5,  cex.axis = 1.5)

source("MyLibrary.r")
source("SaltmarshLibrary.r")
library(mgcv)
library(nlme)
library(lattice)

##############    FRIESLAND    ###############

Y <- read.table("datavectorFR.txt", header = T)
names(Y)
attach(Y)



#[1] "Year"     
#[2] "Distance" - Y,  lateral   seaward   shift
#[3] "Type"     - (1) saltmarsh  (2) Pioneer    (3) Pre-pioneer
#[4] "County"   - (1) Friesland  (2) Groningen
#[5] "Location" - (1) west       (2) mid        (3) east
#[6] "MHT"      
#[7] "ID"             1             2             3 
#[8] "Name"      "K_FR_West"    "K_FR_Mid"   "K_FR_East" 
#[7] "ID"             7             8             9      
#[8] "Name"      "P_FR_West"    "P_FR_Mid"   "P_FR_East" 
#[7] "ID"            13             14            15 
#[8] "Name"      "PP_FR_West"   "PP_FR_Mid"  "PP_FR_East" 




#      Figure 35.6 Right
xyplot(Distance ~ Year|Name, ylab = "Lateral seaward shift", 
       scales = list(x = list(relation = "same"), 
                     y = list(relation = "free")), 
       layout = c(3, 3), type = "l", col = 1, 
       par.settings = list(background = "white"))



#      Figure 35.7 Right
xyplot(Distance ~ MHT|Name, ylab = "Lateral seaward shift", 
       scales = list(x = list(relation = "same"), 
                     y = list(relation = "free")), 
       layout = c(3, 3), col = 1, 
       par.settings = list(background = "white"), 
       panel = function (x, y) {panel.xyplot(x, y, col = 1, type = "p") 
                              I3 <- !is.na(y) & !is.na(x)
                              y1 <- y[I3]
                              x1 <- x[I3]
                              tmp <- lowess(x1, y1, f = 2/3)
                              llines(tmp$x, tmp$y, col = 1) 
              }
)



############################################
#
#    STEP 1. STARTING MODEL (35.4, page 608):
#
#    fixed terms:
#      different non-linear MHT effect for each station
#      different non-linear Year effect for each station
#      station effect  
#    random terms:
#      normally independently homogeneously distributed error term
#       


S1<-as.numeric(ID==1)
S2<-as.numeric(ID==2)
S3<-as.numeric(ID==3)
S7<-as.numeric(ID==7)
S8<-as.numeric(ID==8)
S9<-as.numeric(ID==9)
S13<-as.numeric(ID==13)
S14<-as.numeric(ID==14)
S15<-as.numeric(ID==15)


#Model (35.4) 
gam1 <- gam(Distance ~ factor(ID) + 
            s(Year, by = S1) + s(MHT, by = S1) + 
            s(Year, by = S2) + s(MHT, by = S2) + 
            s(Year, by = S3) + s(MHT, by = S3) + 
            s(Year, by = S7) + s(MHT, by = S7) + 
            s(Year, by = S8) + s(MHT, by = S8) + 
            s(Year, by = S9) + s(MHT, by = S9) + 
            s(Year, by = S13) + s(MHT, by = S13) + 
            s(Year, by = S14) + s(MHT, by = S14) + 
            s(Year, by = S15) + s(MHT, by = S15), 
            data = Y, family = gaussian)
summary(gam1)$s.table
# most Year the smoothers are non-linear 
# most MHT smoothers are straight lines 
# see estimated degrees of freedom (edf)
# so some simpification may be needed



############################################
#
#    STEP 2. FIND OPTIMAL RANDOM STRUCTURE 

#    Starting model has 
#      fixed terms:
#        different non-linear MHT effect for each station
#        different non-linear Year effect for each station
#        station effect  
#      random terms:
#        auto-correlation 


#    Question (i). 
#    Do we need the nine variances or is one sufficient?
#    Apply a likelihood ratio test on the models:
#    (A) with nine variances
#    (B) with one variance


# (A)
lmc<-lmeControl(niterEM=3500,msMaxIter=2000)
vf1 <- varIdent( form =~ 1 | ID)
vf2 <- Initialize(vf1, data = Y)

gam2.A <- gamm(Distance ~ factor(ID) + 
            s(Year, by = S1) + s(MHT, by = S1) + 
            s(Year, by = S2) + s(MHT, by = S2) + 
            s(Year, by = S3) + s(MHT, by = S3) + 
            s(Year, by = S7) + s(MHT, by = S7) + 
            s(Year, by = S8) + s(MHT, by = S8) + 
            s(Year, by = S9) + s(MHT, by = S9) + 
            s(Year, by = S13) + s(MHT, by = S13) + 
            s(Year, by = S14) + s(MHT, by = S14) + 
            s(Year, by = S15) + s(MHT, by = S15), 
            data = Y, family = gaussian, 
            correlation = corAR1(form =  ~ Year|ID), 
            weights = vf2, control = lmc, method = "REML")


#(B)
gam2.B <- gamm(Distance ~ factor(ID) + 
            s(Year, by = S1) + s(MHT, by = S1) + 
            s(Year, by = S2) + s(MHT, by = S2) + 
            s(Year, by = S3) + s(MHT, by = S3) + 
            s(Year, by = S7) + s(MHT, by = S7) + 
            s(Year, by = S8) + s(MHT, by = S8) + 
            s(Year, by = S9) + s(MHT, by = S9) + 
            s(Year, by = S13) + s(MHT, by = S13) + 
            s(Year, by = S14) + s(MHT, by = S14) + 
            s(Year, by = S15) + s(MHT, by = S15), 
            data = Y, family = gaussian, 
            correlation = corAR1(form =  ~ Year|ID), 
            control = lmc, method = "REML")

anova(gam2.A$lme, gam2.B$lme)
# p-value smaller than 0.001 can be seen as an indication 
# that adding nine variances does improve the model
# so model with nine variances is preferred (gam2.A)




#     Question (ii). 
#     Which ARMA structure do we need for the autocorrelation?
#     Try different ARMA structures and 
#     select the combination with lowest AIC:

cs1 <- corAR1(form =~ Year|ID)
cs2 <- corARMA(c(0.7), p = 1, q = 0, form =~ Year|ID)
cs3 <- corARMA(c(0.7, 0.02), p = 1, q = 1, form =~ Year|ID)
cs4 <- corARMA(c(0.7, 0.1, 0.02), p = 2, q = 1, form =~ Year|ID)
cs5 <- corARMA(c(0.3, 0.2, 0.3, 0.2), p = 2, q = 2, form =~ Year|ID)
cs6 <- corARMA(c(0.3, 0.2, 0.3), p = 3, q = 0, form =~ Year|ID)
cs7 <- corARMA(c(0.3, 0.2, 0.3, 0.01), p = 3, q = 1, form =~ Year|ID)
cs8 <- corARMA(c(0.3, 0.2, 0.3, 0.01, 0.01), p = 3, q = 2, form =~ Year|ID)
cs9 <- corARMA(c(0.3, 0.2, 0.3, 0.01, 0.01, 0.01), p=3, q=3, form=~ Year|ID)

gam2.C.cs0 <- gamm(Distance ~ factor(ID) + 
            s(Year, by = S1) + s(MHT, by = S1) + 
            s(Year, by = S2) + s(MHT, by = S2) + 
            s(Year, by = S3) + s(MHT, by = S3) + 
            s(Year, by = S7) + s(MHT, by = S7) + 
            s(Year, by = S8) + s(MHT, by = S8) + 
            s(Year, by = S9) + s(MHT, by = S9) + 
            s(Year, by = S13) + s(MHT, by = S13) + 
            s(Year, by = S14) + s(MHT, by = S14) + 
            s(Year, by = S15) + s(MHT, by = S15), 
            data=Y, family = gaussian, 
            weights = vf2, 
            control = lmc, method = "REML")

gam2.C.cs1 <- gamm(Distance ~ factor(ID) + 
            s(Year, by = S1) + s(MHT, by = S1) + 
            s(Year, by = S2) + s(MHT, by = S2) + 
            s(Year, by = S3) + s(MHT, by = S3) + 
            s(Year, by = S7) + s(MHT, by = S7) + 
            s(Year, by = S8) + s(MHT, by = S8) + 
            s(Year, by = S9) + s(MHT, by = S9) + 
            s(Year, by = S13) + s(MHT, by = S13) + 
            s(Year, by = S14) + s(MHT, by = S14) + 
            s(Year, by = S15) + s(MHT, by = S15), 
            data=Y, family = gaussian, 
            correlation = cs1, weights = vf2, 
            control = lmc, method = "REML")


gam2.C.cs2 <- gamm(Distance ~ factor(ID) + 
            s(Year, by = S1) + s(MHT, by = S1) + 
            s(Year, by = S2) + s(MHT, by = S2) + 
            s(Year, by = S3) + s(MHT, by = S3) + 
            s(Year, by = S7) + s(MHT, by = S7) + 
            s(Year, by = S8) + s(MHT, by = S8) + 
            s(Year, by = S9) + s(MHT, by = S9) + 
            s(Year, by = S13) + s(MHT, by = S13) + 
            s(Year, by = S14) + s(MHT, by = S14) + 
            s(Year, by = S15) + s(MHT, by = S15), 
            data = Y, family = gaussian, 
            correlation = cs2, weights = vf2, 
            control = lmc, method = "REML")


gam2.C.cs3 <- gamm(Distance ~ factor(ID) + 
            s(Year, by = S1) + s(MHT, by = S1) + 
            s(Year, by = S2) + s(MHT, by = S2) + 
            s(Year, by = S3) + s(MHT, by = S3) + 
            s(Year, by = S7) + s(MHT, by = S7) + 
            s(Year, by = S8) + s(MHT, by = S8) + 
            s(Year, by = S9) + s(MHT, by = S9) + 
            s(Year, by = S13) + s(MHT, by = S13) + 
            s(Year, by = S14) + s(MHT, by = S14) + 
            s(Year, by = S15) + s(MHT, by = S15), 
            dat = Y, family = gaussian, 
            correlation = cs3, weights = vf2, 
            control = lmc, method = "REML")


gam2.C.cs4 <- gamm(Distance ~ factor(ID) + 
            s(Year, by = S1) + s(MHT, by = S1) + 
            s(Year, by = S2) + s(MHT, by = S2) + 
            s(Year, by = S3) + s(MHT, by = S3) + 
            s(Year, by = S7) + s(MHT, by = S7) + 
            s(Year, by = S8) + s(MHT, by = S8) + 
            s(Year, by = S9) + s(MHT, by = S9) + 
            s(Year, by = S13) + s(MHT, by = S13) + 
            s(Year, by = S14) + s(MHT, by = S14) + 
            s(Year, by = S15) + s(MHT, by = S15), 
            data = Y, family = gaussian, 
            correlation = cs4, weights = vf2, 
            control = lmc, method = "REML")


gam2.C.cs5 <- gamm(Distance ~ factor(ID) + 
            s(Year, by = S1) + s(MHT, by = S1) + 
            s(Year, by = S2) + s(MHT, by = S2) + 
            s(Year, by = S3) + s(MHT, by = S3) + 
            s(Year, by = S7) + s(MHT, by = S7) + 
            s(Year, by = S8) + s(MHT, by = S8) + 
            s(Year, by = S9) + s(MHT, by = S9) + 
            s(Year, by = S13) + s(MHT, by = S13) + 
            s(Year, by = S14) + s(MHT, by = S14) + 
            s(Year, by = S15) + s(MHT, by = S15), 
            data = Y, family = gaussian, 
            correlation = cs5, weights = vf2, 
            control = lmc, method = "REML")


gam2.C.cs6 <- gamm(Distance ~ factor(ID) + 
            s(Year, by = S1) + s(MHT, by = S1) + 
            s(Year, by = S2) + s(MHT, by = S2) + 
            s(Year, by = S3) + s(MHT, by = S3) + 
            s(Year, by = S7) + s(MHT, by = S7) + 
            s(Year, by = S8) + s(MHT, by = S8) + 
            s(Year, by = S9) + s(MHT, by = S9) + 
            s(Year, by = S13) + s(MHT, by = S13) + 
            s(Year, by = S14) + s(MHT, by = S14) + 
            s(Year, by = S15) + s(MHT, by = S15), 
            data = Y, family = gaussian, 
            correlation = cs6, weights = vf2, 
            control = lmc, method = "REML")


gam2.C.cs7 <- gamm(Distance ~ factor(ID) + 
            s(Year, by = S1) + s(MHT, by = S1) + 
            s(Year, by = S2) + s(MHT, by = S2) + 
            s(Year, by = S3) + s(MHT, by = S3) + 
            s(Year, by = S7) + s(MHT, by = S7) + 
            s(Year, by = S8) + s(MHT, by = S8) + 
            s(Year, by = S9) + s(MHT, by = S9) + 
            s(Year, by = S13) + s(MHT, by = S13) + 
            s(Year, by = S14) + s(MHT, by = S14) + 
            s(Year, by = S15) + s(MHT, by = S15), 
            data = Y, family = gaussian, 
            correlation = cs7, weights = vf2, 
            control = lmc, method = "REML")


gam2.C.cs8 <- gamm(Distance ~ factor(ID) + 
            s(Year, by = S1) + s(MHT, by = S1) + 
            s(Year, by = S2) + s(MHT, by = S2) + 
            s(Year, by = S3) + s(MHT, by = S3) + 
            s(Year, by = S7) + s(MHT, by = S7) + 
            s(Year, by = S8) + s(MHT, by = S8) + 
            s(Year, by = S9) + s(MHT, by = S9) + 
            s(Year, by = S13) + s(MHT, by = S13) + 
            s(Year, by = S14) + s(MHT, by = S14) + 
            s(Year, by = S15) + s(MHT, by = S15), 
            data = Y, family = gaussian, 
            correlation = cs8, weights = vf2, 
            control = lmc, method = "REML")


gam2.C.cs9 <- gamm(Distance ~ factor(ID) + 
            s(Year, by = S1) + s(MHT, by = S1) + 
            s(Year, by = S2) + s(MHT, by = S2) + 
            s(Year, by = S3) + s(MHT, by = S3) + 
            s(Year, by = S7) + s(MHT, by = S7) + 
            s(Year, by = S8) + s(MHT, by = S8) + 
            s(Year, by = S9) + s(MHT, by = S9) + 
            s(Year, by = S13) + s(MHT, by = S13) + 
            s(Year, by = S14) + s(MHT, by = S14) + 
            s(Year, by = S15) + s(MHT, by = S15), 
            data = Y, family = gaussian, 
            correlation = cs9, weights = vf2, 
            control = lmc, method = "REML")
#"numerical problems"

anova(gam2.C.cs0$lme, gam2.C.cs1$lme, gam2.C.cs2$lme, 
      gam2.C.cs3$lme, gam2.C.cs4$lme, gam2.C.cs5$lme,
      gam2.C.cs6$lme, gam2.C.cs7$lme, gam2.C.cs8$lme)
# The model with no auto-correlation (gam2.C.cs0) has the biggest AIC
# The models with ARMA(1, 1) and ARMA(2, 1) have the lowest AICs:
# 2389 and 2387. We select ARMA(1, 1), because a difference of less than  
# 2 is seen as not important enough to go for a more complicated model

# Thus to the next step we go with cs3 correlation structure 


############################################
#
#    STEP 3. FIND OPTIMAL FIXED STRUCTURE 

#    Starting model has 
#      fixed terms:
#        different non-linear MHT effect for each station
#        different non-linear Year effect for each station
#        station effect  
#      random terms:
#        nine variances to account for heteroscedasticity
#        ARMA (p = 1, q = 1) auto-correlation structure

#    Dropping non-significant smoothers.
#    ML estimation instead REML is used.


lmc<-lmeControl(niterEM=3500,msMaxIter=2000)
vf1 <- varIdent( form =~ 1 | ID)
vf2 <- Initialize(vf1, data = Y)
cs1 <- corARMA(c(0.7, 0.02), p = 1, q = 1, form =~ Year|ID)

gam3.ML.cs1 <- gamm(Distance ~ factor(ID) + 
            s(Year, by = S1) + s(MHT, by = S1) + 
            s(Year, by = S2) + s(MHT, by = S2) + 
            s(Year, by = S3) + s(MHT, by = S3) + 
            s(Year, by = S7) + s(MHT, by = S7) + 
            s(Year, by = S8) + s(MHT, by = S8) + 
            s(Year, by = S9) + s(MHT, by = S9) + 
            s(Year, by = S13) + s(MHT, by = S13) + 
            s(Year, by = S14) + s(MHT, by = S14) + 
            s(Year, by = S15) + s(MHT, by = S15), 
            data=Y, family = gaussian, 
            correlation = cs1, weights = vf2, 
            control = lmc, method = "ML")
summary(gam3.ML.cs1$gam)
# All smoothers except one have 1 degree of freedom, 




# DROP terms which are not significant 
# at the 5% level (one by one):

#s(Year):S13
#s(MHT):S2
#s(Year):S8
#s(MHT):S3
#s(MHT):S1
#s(Year):S15
#s(MHT):S15
#s(Year):S7
#s(MHT):S9
#s(Year):S9
#s(MHT):S13
#s(Year):S14
#s(MHT):S7
#s(Year):S2

#RESULTING MODEL:
gam3.ML.cs1 <- gamm(Distance ~ factor(ID) + 
                               s(Year,by=S1) +
                               s(Year,by=S3) +
                               s(MHT,by=S8) +
                               s(MHT,by=S14),
                               data=Y, family = gaussian, 
                               correlation = cs1, weights = vf2, 
                               control = lmc, method = "ML")
gam.OPTIM <- gam3.ML.cs1

summary(gam.OPTIM$gam)





##########################################
#
#      Figure 35.8


# EXTRACT YEAR SMOOTHERS
tmp <- predict(gam.OPTIM$gam, se = T, type = "terms")

# Numbering of fixed terms:
#  1  2  3      factor(ID)   s(Year):S1   s(Year):S3
#  4  5         s(MHT):S8    s(MHT):S14  


Year_st1.smoother <- tmp$fit[S1 == 1, 2]
Year_st1.se <- tmp$se[S1 == 1, 2]
Year_st1.real <- Year[S1 == 1]

Year_st3.smoother <- tmp$fit[S3 == 1, 3]
Year_st3.se <- tmp$se[S3 == 1, 3]
Year_st3.real <- Year[S3 == 1]

Year.smoother <- c(Year_st1.smoother, Year_st3.smoother)
Year.se <- c(Year_st1.se, Year_st3.se)
Year.real <- c(Year_st1.real, Year_st3.real)

Year.se_upper <- Year.smoother + 2*Year.se
Year.se_lower <- Year.smoother - 2*Year.se

Name4plot.Y <- factor(c(rep(c("K_FR_West", "K_FR_East"), each=29)))



# graph ver. 1
xyplot(Year.smoother ~ Year.real|Name4plot.Y, col = 1, type = "l",
       xlab = "Year", ylab = "Trends", layout=c(2,1), 
       par.settings = list(background = "white"))

#graph ver. 2 (this plot uses functions from SaltmarshLibrary.r)
source("SaltmarshLibrary.r")
mydeviceTrellis()

Year2plot <- Year.smoother ~ Year.real|Name4plot.Y

myplotN1Trellis(Year2plot, "Smoothers for year", "Year", 
                "Contribution to the fitted values", 
                "same", "same", 0.3, "lines only", 
                Year.se_upper, Year.se_lower)



#EXTRACT MHT SMOOTHER
tmp <- predict(gam.OPTIM$gam, se = T, type = "terms")

# Numbering of fixed terms:
# Numbering of fixed terms:
#  1  2  3      factor(ID)   s(Year):S1   s(Year):S3
#  4  5         s(MHT):S8    s(MHT):S14  


MHT_st8.smoother <- tmp$fit[S8 == 1, 4]
MHT_st8.se <- tmp$se[S8 == 1, 4]
MHT_st8.real <- MHT[S8 == 1]

MHT_st14.smoother <- tmp$fit[S14 == 1, 5]
MHT_st14.se <- tmp$se[S14 == 1, 5]
MHT_st14.real <- MHT[S14 == 1]

ord<-order(MHT_st14.real)

MHT.smoother <- c(MHT_st8.smoother[ord], MHT_st14.smoother[ord])
MHT.se <- c(MHT_st8.se[ord], MHT_st14.se[ord])
MHT.real <-  c(MHT_st8.real[ord], MHT_st14.real[ord])

Name4plot.M <- factor(c(rep(c("P_FR_Mid", "PP_FR_Mid"), each=29)))


MHT.se_upper <- MHT.smoother + 2*MHT.se
MHT.se_lower <- MHT.smoother - 2*MHT.se


# graph ver. 1
xyplot(MHT.smoother ~ MHT.real|Name4plot.M, col = 1, type = "l", 
       xlab = "MHT", ylab = "Trends", layout=c(2,2), 
       par.settings = list(background = "white"))

#graph ver. 2 (this plot uses functions from SaltmarshLibrary.r)
source("SaltmarshLibrary.r")
mydeviceTrellis()

MHT2plot <- MHT.smoother ~ MHT.real|Name4plot.M

myplotN1Trellis(MHT2plot, "Smoothers for MHT", "MHT", 
                "Contribution to the fitted values", 
                "same", "same", 0.3, "lines only",
                MHT.se_upper, MHT.se_lower)


###########################################
#
#      Remove non-linear MHT effect 
#      Try models with linear MHT effect and 
#      with Station*MHT interaction term

lmc<-lmeControl(niterEM=3500,msMaxIter=2000)
vf1 <- varIdent( form =~ 1 | ID)
vf2 <- Initialize(vf1, data = Y)
cs1 <- corARMA(c(0.7, 0.02), p = 1, q = 1, form =~ Year|ID)

# Model with linear MHT term 
gam3.ML.fin <- gamm(Distance ~ factor(ID) + MHT + 
                               s(Year,by=S1) +
                               s(Year,by=S2) +
                               s(Year,by=S3) +
                               s(Year,by=S7) +
                               s(Year,by=S8) +
                               s(Year,by=S9) +
                               s(Year,by=S13) +
                               s(Year,by=S14) +
                               s(Year,by=S15),
                            data=Y, family = gaussian, 
                            correlation = cs1, weights = vf2, 
                            control = lmc, method = "ML")

# Model with interaction between station and MHT (25.7B, page 611)
gam3.ML.fin2 <- gamm(Distance ~ factor(ID)*MHT + 
                               s(Year,by=S1) +
                               s(Year,by=S2) +
                               s(Year,by=S3) +
                               s(Year,by=S7) +
                               s(Year,by=S8) +
                               s(Year,by=S9) +
                               s(Year,by=S13) +
                               s(Year,by=S14) +
                               s(Year,by=S15),
                            data=Y, family = gaussian, 
                            correlation = cs1, weights = vf2, 
                            control = lmc, method = "ML")

anova(gam3.ML.fin$lme, gam3.ML.fin2$lme)

#anova table shows that model with the interaction term 
# is better

anova(gam3.ML.fin2$gam)
#Also remove terms:
#s(Year):S13
#s(Year):S15
#s(Year):S7
#s(Year):S9
#s(Year):S14


gam3.ML.fin3 <- gamm(Distance ~ factor(ID)*MHT + 
                               s(Year,by=S1) +
                               s(Year,by=S2) +
                               s(Year,by=S3) +
                               s(Year,by=S8),
                            data=Y, family = gaussian, 
                            correlation = cs1, weights = vf2, 
                            control = lmc, method = "ML")

anova(gam.OPTIM$lme,gam3.ML.fin2$lme, gam3.ML.fin3$lme)




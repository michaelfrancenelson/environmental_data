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

par(mar = c(4.5,  4.5,  0.5,  0.5),  cex.lab = 1.2,  cex.axis = 1.2)

source("MyLibrary.r")
source("SaltmarshLibrary.r")
library(mgcv)
library(nlme)
library(lattice)

##############    GRONINGEN    ###############

GR <- read.table("datavectorGR.txt", header = T)
names(GR)
#attach(Y)



#[1] "Year"     
#[2] "Distance" - Y,  lateral   seaward   shift
#[3] "Type"     - (1) saltmarsh  (2) Pioneer    (3) Pre-pioneer
#[4] "County"   - (1) Friesland  (2) Groningen
#[5] "Location" - (1) west       (2) mid        (3) east
#[6] "MHT"      
#[7] "ID"             4             5             6 
#[8] "Name"      "K_GR_West"    "K_GR_Mid"   "K_GR_East" 
#[7] "ID"            10             11           12      
#[8] "Name"      "P_GR_West"    "P_GR_Mid"   "P_GR_East" 
#[7] "ID"            16             17           18 
#[8] "Name"      "PP_GR_West"   "PP_GR_Mid"  "PP_GR_East" 



#      Figure 35.6 Left
xyplot(Distance ~ Year|Name, ylab = "Lateral seaward shift", data = GR,
       scales = list(x = list(relation = "same"), 
                     y = list(relation = "free")), 
       layout = c(3, 3), type = "l", col = 1)
#Use the levels option in the factor function for Name to change the order of the
#panels.

#Note that the order of the panels is slighlty different. Use the levels
#option in the factor(Name) command to change the order of the levels.
#It will also change the order of the panels



#      Figure 35.7 Left
xyplot(Distance ~ MHT|Name, ylab = "Lateral seaward shift", data = GR,
       scales = list(x = list(relation = "same"), 
                     y = list(relation = "free")), 
       layout = c(3, 3), col = 1, 
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


GR$S4 <- as.numeric(GR$ID == 4)
GR$S5 <- as.numeric(GR$ID == 5)
GR$S6 <- as.numeric(GR$ID == 6)
GR$S10 <- as.numeric(GR$ID == 10)
GR$S11 <- as.numeric(GR$ID == 11)
GR$S12 <- as.numeric(GR$ID == 12)
GR$S16 <- as.numeric(GR$ID == 16)
GR$S17 <- as.numeric(GR$ID == 17)
GR$S18 <- as.numeric(GR$ID == 18)


#Model (35.4) 
gam1 <- gamm(Distance ~ factor(ID) +
            s(Year, by = S4) + s(MHT, by = S4) + 
            s(Year, by = S5) + s(MHT, by = S5) + 
            s(Year, by = S6) + s(MHT, by = S6) + 
            s(Year, by = S10) + s(MHT, by = S10) + 
            s(Year, by = S11) + s(MHT, by = S11) + 
            s(Year, by = S12) + s(MHT, by = S12) + 
            s(Year, by = S16) + s(MHT, by = S16) + 
            s(Year, by = S17) + s(MHT, by = S17) + 
            s(Year, by = S18) + s(MHT, by = S18), 
            data = GR, family = gaussian,
            method = "REML")
            
summary(gam1$gam)$s.table
# for some stations the smoothers are non-linear 
# for some they are straight lines 
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
lmc <- lmeControl(niterEM = 1900, msMaxIter = 1200)
vf1 <- varIdent( form =~ 1 | ID)
vf2 <- Initialize(vf1, data = GR)

gam2.A <- gamm(Distance ~ factor(ID) + 
            s(Year, by = S4) + s(MHT, by = S4) + 
            s(Year, by = S5) + s(MHT, by = S5) + 
            s(Year, by = S6) + s(MHT, by = S6) + 
            s(Year, by = S10) + s(MHT, by = S10) + 
            s(Year, by = S11) + s(MHT, by = S11) + 
            s(Year, by = S12) + s(MHT, by = S12) + 
            s(Year, by = S16) + s(MHT, by = S16) + 
            s(Year, by = S17) + s(MHT, by = S17) + 
            s(Year, by = S18) + s(MHT, by = S18), 
            data = GR, family = gaussian,
            correlation = corAR1(form =  ~ Year|ID), 
            weights = vf2, control = lmc, method = "REML")


#(B)
gam2.B <- gamm(Distance ~ factor(ID) + 
            s(Year, by = S4) + s(MHT, by = S4) + 
            s(Year, by = S5) + s(MHT, by = S5) + 
            s(Year, by = S6) + s(MHT, by = S6) + 
            s(Year, by = S10) + s(MHT, by = S10) + 
            s(Year, by = S11) + s(MHT, by = S11) + 
            s(Year, by = S12) + s(MHT, by = S12) + 
            s(Year, by = S16) + s(MHT, by = S16) + 
            s(Year, by = S17) + s(MHT, by = S17) + 
            s(Year, by = S18) + s(MHT, by = S18), 
            data = GR, family = gaussian,
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

cs1 <- corAR1(form =~ Year |ID)
cs2 <- corARMA(c(0.7), p = 1, q = 0, form =~ Year|ID)
cs3 <- corARMA(c(0.7, 0.02), p = 1, q = 1, form =~ Year|ID)
cs4 <- corARMA(c(0.7, 0.1, 0.02), p = 2, q = 1, form =~ Year|ID)
cs5 <- corARMA(c(0.3, 0.2, 0.3, 0.2), p = 2, q = 2, form =~ Year|ID)
cs6 <- corARMA(c(0.3, 0.2, 0.3), p = 3, q = 0, form =~ Year|ID)
cs7 <- corARMA(c(0.3, 0.2, 0.3, 0.01), p = 3, q = 1, form =~ Year|ID)
cs8 <- corARMA(c(0.3, 0.2, 0.3, 0.01, 0.01), p = 3, q = 2, form =~ Year|ID)
cs9 <- corARMA(c(0.3, 0.2, 0.3, 0.01, 0.01, 0.01), p=3, q=3, form=~ Year|ID)

#Instead of repeating the commands like we do, you can also use the update
#command. I guess it will be faster
gam2.C.cs0 <- gamm(Distance ~ factor(ID) + 
            s(Year, by = S4) + s(MHT, by = S4) + 
            s(Year, by = S5) + s(MHT, by = S5) + 
            s(Year, by = S6) + s(MHT, by = S6) + 
            s(Year, by = S10) + s(MHT, by = S10) + 
            s(Year, by = S11) + s(MHT, by = S11) + 
            s(Year, by = S12) + s(MHT, by = S12) + 
            s(Year, by = S16) + s(MHT, by = S16) + 
            s(Year, by = S17) + s(MHT, by = S17) + 
            s(Year, by = S18) + s(MHT, by = S18), 
            data=GR, family = gaussian,
            weights = vf2, 
            control = lmc, method = "REML")

gam2.C.cs1 <- gamm(Distance ~ factor(ID) + 
            s(Year, by = S4) + s(MHT, by = S4) + 
            s(Year, by = S5) + s(MHT, by = S5) + 
            s(Year, by = S6) + s(MHT, by = S6) + 
            s(Year, by = S10) + s(MHT, by = S10) + 
            s(Year, by = S11) + s(MHT, by = S11) + 
            s(Year, by = S12) + s(MHT, by = S12) + 
            s(Year, by = S16) + s(MHT, by = S16) + 
            s(Year, by = S17) + s(MHT, by = S17) + 
            s(Year, by = S18) + s(MHT, by = S18), 
            data=GR, family = gaussian,
            correlation = cs1, weights = vf2, 
            control = lmc, method = "REML")


gam2.C.cs2 <- gamm(Distance ~ factor(ID) + 
            s(Year, by = S4) + s(MHT, by = S4) + 
            s(Year, by = S5) + s(MHT, by = S5) + 
            s(Year, by = S6) + s(MHT, by = S6) + 
            s(Year, by = S10) + s(MHT, by = S10) + 
            s(Year, by = S11) + s(MHT, by = S11) + 
            s(Year, by = S12) + s(MHT, by = S12) + 
            s(Year, by = S16) + s(MHT, by = S16) + 
            s(Year, by = S17) + s(MHT, by = S17) + 
            s(Year, by = S18) + s(MHT, by = S18), 
            data = GR, family = gaussian,
            correlation = cs2, weights = vf2, 
            control = lmc, method = "REML")


gam2.C.cs3 <- gamm(Distance ~ factor(ID) + 
            s(Year, by = S4) + s(MHT, by = S4) + 
            s(Year, by = S5) + s(MHT, by = S5) + 
            s(Year, by = S6) + s(MHT, by = S6) + 
            s(Year, by = S10) + s(MHT, by = S10) + 
            s(Year, by = S11) + s(MHT, by = S11) + 
            s(Year, by = S12) + s(MHT, by = S12) + 
            s(Year, by = S16) + s(MHT, by = S16) + 
            s(Year, by = S17) + s(MHT, by = S17) + 
            s(Year, by = S18) + s(MHT, by = S18), 
            dat = GR, family = gaussian,
            correlation = cs3, weights = vf2, 
            control = lmc, method = "REML")


gam2.C.cs4 <- gamm(Distance ~ factor(ID) + 
            s(Year, by = S4) + s(MHT, by = S4) + 
            s(Year, by = S5) + s(MHT, by = S5) + 
            s(Year, by = S6) + s(MHT, by = S6) + 
            s(Year, by = S10) + s(MHT, by = S10) + 
            s(Year, by = S11) + s(MHT, by = S11) + 
            s(Year, by = S12) + s(MHT, by = S12) + 
            s(Year, by = S16) + s(MHT, by = S16) + 
            s(Year, by = S17) + s(MHT, by = S17) + 
            s(Year, by = S18) + s(MHT, by = S18), 
            data = GR, family = gaussian,
            correlation = cs4, weights = vf2, 
            control = lmc, method = "REML")
#"numerical problems"

gam2.C.cs5 <- gamm(Distance ~ factor(ID) + 
            s(Year, by = S4) + s(MHT, by = S4) + 
            s(Year, by = S5) + s(MHT, by = S5) + 
            s(Year, by = S6) + s(MHT, by = S6) + 
            s(Year, by = S10) + s(MHT, by = S10) + 
            s(Year, by = S11) + s(MHT, by = S11) + 
            s(Year, by = S12) + s(MHT, by = S12) + 
            s(Year, by = S16) + s(MHT, by = S16) + 
            s(Year, by = S17) + s(MHT, by = S17) + 
            s(Year, by = S18) + s(MHT, by = S18), 
            data = GR, family = gaussian,
            correlation = cs5, weights = vf2, 
            control = lmc, method = "REML")

gam2.C.cs6 <- gamm(Distance ~ factor(ID) + 
            s(Year, by = S4) + s(MHT, by = S4) + 
            s(Year, by = S5) + s(MHT, by = S5) + 
            s(Year, by = S6) + s(MHT, by = S6) + 
            s(Year, by = S10) + s(MHT, by = S10) + 
            s(Year, by = S11) + s(MHT, by = S11) + 
            s(Year, by = S12) + s(MHT, by = S12) + 
            s(Year, by = S16) + s(MHT, by = S16) + 
            s(Year, by = S17) + s(MHT, by = S17) + 
            s(Year, by = S18) + s(MHT, by = S18), 
            data = GR, family = gaussian,
            correlation = cs6, weights = vf2, 
            control = lmc, method = "REML")


gam2.C.cs7 <- gamm(Distance ~ factor(ID) + 
            s(Year, by = S4) + s(MHT, by = S4) + 
            s(Year, by = S5) + s(MHT, by = S5) + 
            s(Year, by = S6) + s(MHT, by = S6) + 
            s(Year, by = S10) + s(MHT, by = S10) + 
            s(Year, by = S11) + s(MHT, by = S11) + 
            s(Year, by = S12) + s(MHT, by = S12) + 
            s(Year, by = S16) + s(MHT, by = S16) + 
            s(Year, by = S17) + s(MHT, by = S17) + 
            s(Year, by = S18) + s(MHT, by = S18), 
            data = GR, family = gaussian,
            correlation = cs7, weights = vf2, 
            control = lmc, method = "REML")

gam2.C.cs8 <- gamm(Distance ~ factor(ID) + 
            s(Year, by = S4) + s(MHT, by = S4) + 
            s(Year, by = S5) + s(MHT, by = S5) + 
            s(Year, by = S6) + s(MHT, by = S6) + 
            s(Year, by = S10) + s(MHT, by = S10) + 
            s(Year, by = S11) + s(MHT, by = S11) + 
            s(Year, by = S12) + s(MHT, by = S12) + 
            s(Year, by = S16) + s(MHT, by = S16) + 
            s(Year, by = S17) + s(MHT, by = S17) + 
            s(Year, by = S18) + s(MHT, by = S18), 
            data = GR, family = gaussian,
            correlation = cs8, weights = vf2, 
            control = lmc, method = "REML")
#numerical problems"

gam2.C.cs9 <- gamm(Distance ~ factor(ID) + 
            s(Year, by = S4) + s(MHT, by = S4) + 
            s(Year, by = S5) + s(MHT, by = S5) + 
            s(Year, by = S6) + s(MHT, by = S6) + 
            s(Year, by = S10) + s(MHT, by = S10) + 
            s(Year, by = S11) + s(MHT, by = S11) + 
            s(Year, by = S12) + s(MHT, by = S12) + 
            s(Year, by = S16) + s(MHT, by = S16) + 
            s(Year, by = S17) + s(MHT, by = S17) + 
            s(Year, by = S18) + s(MHT, by = S18), 
            data = GR, family = gaussian,
            correlation = cs9, weights = vf2, 
            control = lmc, method = "REML")
#"numerical problems"

anova(gam2.C.cs0$lme, gam2.C.cs1$lme, gam2.C.cs2$lme, 
      gam2.C.cs3$lme, gam2.C.cs5$lme, gam2.C.cs6$lme,
      gam2.C.cs7$lme)
# The model with no auto-correlation (gam2.C.cs0) has the highest AIC
# The model with ARMA(1, 0) structure has the lowest AIC, 
# so this error structure should be used

# ARMA(1, 0) is also called AR(1)
# these are the models gam2.C.cs2 and gam2.C.cs1
# They are the same and have the same AIC




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
#        ARMA (p = 1, q = 0) auto-correlation structure

#    Dropping non-significant smoothers.
#    ML estimation instead REML is used.


lmc <- lmeControl(niterEM = 2500, msMaxIter = 2000)
cs1 <- corARMA(c(0.7), p = 1, q = 0, form =~ Year|ID)

gam3.ML.cs1 <- gamm(Distance ~ factor(ID) + 
            s(Year, by = S4) + s(MHT, by = S4) + 
            s(Year, by = S5) + s(MHT, by = S5) + 
            s(Year, by = S6) + s(MHT, by = S6) + 
            s(Year, by = S10) + s(MHT, by = S10) + 
            s(Year, by = S11) + s(MHT, by = S11) + 
            s(Year, by = S12) + s(MHT, by = S12) + 
            s(Year, by = S16) + s(MHT, by = S16) + 
            s(Year, by = S17) + s(MHT, by = S17) + 
            s(Year, by = S18) + s(MHT, by = S18), 
            data=GR, family = gaussian,
            correlation = cs1, weights = vf2, 
            control = lmc, method = "ML")




# DROP terms which are not significant 
# at the 5% level (one by one):

#s(MHT):S6
#s(MHT):S4
#s(Year):S17
#s(MHT):S17
#s(Year):S5
#s(MHT):S11



#RESULTING MODEL:
gam3.ML.cs1 <- gamm(Distance ~ factor(ID) + 
            s(Year, by = S4) + 
                                s(MHT, by = S5) + 
            s(Year, by = S6) + 
            s(Year, by = S10) + s(MHT, by = S10) + 
            s(Year, by = S11) + 
            s(Year, by = S12) + s(MHT, by = S12) + 
            s(Year, by = S16) + s(MHT, by = S16) + 
            s(Year, by = S18) + s(MHT, by = S18), 
            data=GR, family = gaussian,
            correlation = cs1, weights = vf2, 
            control = lmc, method = "ML")
gam.OPTIM <- gam3.ML.cs1

summary(gam.OPTIM$gam)
# All MHT smoothers have 1 degree of freedom, 
# it indicates a linear relationship 
# see below the models with linear MHT term




##########################################
#
#      Figure 35.8


# EXTRACT YEAR SMOOTHERS
tmp <- predict(gam.OPTIM$gam, se = T, type = "terms")

# Numbering of fixed terms:
#  1  2  3      factor(ID)   s(Year):S4   s(MHT):S5  
#  4  5  6      s(Year):S6   s(Year):S10  s(MHT):S10  
#  7  8  9      s(Year):S11  s(Year):S12  s(MHT):S12 
# 10 11 12      s(Year):S16  s(MHT):S16  s(Year):S18
# 13            s(MHT):S18

Year_st4.smoother <- tmp$fit[S4 == 1, 2]
Year_st4.se <- tmp$se[S4 == 1, 2]
Year_st4.real <- Year[S4 == 1]

Year_st6.smoother <- tmp$fit[S6 == 1, 4]
Year_st6.se <- tmp$se[S6 == 1, 4]
Year_st6.real <- Year[S6 == 1]

Year_st10.smoother <- tmp$fit[S10 == 1, 5]
Year_st10.se <- tmp$se[S10 == 1, 5]
Year_st10.real <- Year[S10 == 1]

Year_st11.smoother<- tmp$fit[S11 == 1, 7]
Year_st11.se <- tmp$se[S11 == 1, 7]
Year_st11.real <- Year[S11 == 1]

Year_st12.smoother <- tmp$fit[S12 == 1, 8]
Year_st12.se <- tmp$se[S12 == 1, 8]
Year_st12.real <- Year[S12 == 1]


Year_st16.smoother <- tmp$fit[S16 == 1, 10]
Year_st16.se <- tmp$se[S16 == 1, 10]
Year_st16.real <- Year[S16 == 1]

Year_st18.smoother <- tmp$fit[S18 == 1, 12]
Year_st18.se <- tmp$se[S18 == 1, 12]
Year_st18.real <- Year[S18 == 1]

Year.smoother <- c(Year_st4.smoother, Year_st6.smoother,
                   Year_st10.smoother, Year_st11.smoother,
                   Year_st12.smoother, Year_st16.smoother,
                   Year_st18.smoother)
Year.se <- c(Year_st4.se, Year_st6.se, Year_st10.se,
             Year_st11.se, Year_st12.se, Year_st16.se,
             Year_st18.se)
Year.real <- c(Year_st4.real, Year_st6.real, Year_st10.real,
               Year_st11.real, Year_st12.real, Year_st16.real,
               Year_st18.real)

Year.se_upper <- Year.smoother + 2*Year.se
Year.se_lower <- Year.smoother - 2*Year.se

Name4plot.Y <- factor(c(rep(c("K_GR_West", "K_GR_East",
                              "P_GR_West", "P_GR_Mid",
                              "P_GR_East", "PP_GR_West", 
                              "PP_GR_East"), each=29)))


# graph ver. 1
xyplot(Year.smoother ~ Year.real|Name4plot.Y, col = 1, type = "l",
       xlab = "Year", ylab = "Trends", layout=c(2,4), 
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
#  1  2  3      factor(ID)   s(Year):S4   s(MHT):S5  
#  4  5  6      s(Year):S6   s(Year):S10  s(MHT):S10  
#  7  8  9      s(Year):S11  s(Year):S12  s(MHT):S12 
# 10 11 12      s(Year):S16  s(MHT):S16  s(Year):S18
# 13            s(MHT):S18

MHT_st5.smoother <- tmp$fit[S5 == 1, 3]
MHT_st5.se <- tmp$se[S5 == 1, 3]
MHT_st5.real <- MHT[S5 == 1]

MHT_st10.smoother <- tmp$fit[S10 == 1, 6]
MHT_st10.se <- tmp$se[S10 == 1, 6]
MHT_st10.real <- MHT[S10 == 1]

MHT_st12.smoother <- tmp$fit[S12 == 1, 9]
MHT_st12.se <- tmp$se[S12 == 1, 9]
MHT_st12.real <- MHT[S12 == 1]

MHT_st16.smoother <- tmp$fit[S16 == 1, 11]
MHT_st16.se <- tmp$se[S16 == 1, 11]
MHT_st16.real <- MHT[S16 == 1]

MHT_st18.smoother <- tmp$fit[S18 == 1, 13]
MHT_st18.se <- tmp$se[S18 == 1, 13]
MHT_st18.real <- MHT[S18 == 1]

ord<-order(MHT_st18.real)

MHT.smoother <- c(MHT_st5.smoother[ord], MHT_st10.smoother[ord], 
                  MHT_st12.smoother[ord], MHT_st16.smoother[ord],
                  MHT_st18.smoother[ord])
MHT.se <- c(MHT_st5.se[ord], MHT_st10.se[ord], MHT_st12.se[ord], 
            MHT_st16.se[ord], MHT_st18.se[ord])
MHT.real <-  c(MHT_st5.real[ord], MHT_st10.real[ord],
               MHT_st12.real[ord], MHT_st16.real[ord],
               MHT_st18.real[ord])

Name4plot.M <- factor(c(rep(c("K_GR_Mid", "P_GR_West", 
                              "P_GR_East", "PP_GR_West",
                              "PP_GR_East"),each=29)))

MHT.se_upper <- MHT.smoother + 2*MHT.se
MHT.se_lower <- MHT.smoother - 2*MHT.se


# graph ver. 1
xyplot(MHT.smoother ~ MHT.real|Name4plot.M, col = 1, type = "l", 
       xlab = "MHT", ylab = "Trends", layout=c(2,4), 
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
#      Compare models with linear MHT effect and 
#      with Station*MHT interaction term

lmc <- lmeControl(niterEM = 2500, msMaxIter = 2000)
cs1 <- corARMA(c(0.7), p = 1, q = 0, form =~ Year|ID)

# Model with linear MHT term (analogue of 25.7A, page 611)
gam3.ML.fin <- gamm(Distance ~ factor(ID) + MHT + 
                            s(Year, by = S4) + 
                            s(Year, by = S6) + 
                            s(Year, by = S10) + 
                            s(Year, by = S11) + 
                            s(Year, by = S12) + 
                            s(Year, by = S16) +  
                            s(Year, by = S18), 
                            data=Y, family = gaussian, 
                            correlation = cs1, weights = vf2, 
                            control = lmc, method = "ML")

# Model with interaction between station and MHT (25.7B, page 611)
gam3.ML.fin2 <- gamm(Distance ~ factor(ID)*MHT + 
                            s(Year, by = S4) + 
                            s(Year, by = S6) + 
                            s(Year, by = S10) + 
                            s(Year, by = S11) + 
                            s(Year, by = S12) + 
                            s(Year, by = S16) +  
                            s(Year, by = S18), 
                            data=Y, family = gaussian, 
                            correlation = cs1, weights = vf2, 
                            control = lmc, method = "ML")

anova(gam3.ML.fin$lme, gam3.ML.fin2$lme)
# p-value of 0.03 indicates that the interaction term 
# is weakly significant


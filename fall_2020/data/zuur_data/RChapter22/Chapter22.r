#    R code for: Chapter 22 in:
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

#    This file was produced by Alain Zuur
#    (highstat@highstat.com)



setwd("D:\\applicat\\HighlandStatistics\\Book\\R\\RChapter22\\")

par(mar = c(4.5, 4.5, 0.5, 0.5), cex.lab = 1.3, cex.axis = 1.3)

Beesdat <- read.table("Bees.txt", header = T)
attach(Beesdat)
source("MyLibrary.r")
library(mgcv)



#      Figure 22.2
plot(Beesdat[, c(7, 5, 6, 4)], upper.panel = panel.smooth2, 
     lower.panel = panel.cor)
#Note: The panel.cor function in our library
#file MyLibrary.R uses only one digit for the correlation.
#In the book, we used two digits. You can easily
#change this.

#Perhaps a Poisson distribution is better!


#      Figure 22.2
coplot(Bees~Temperature|as.factor(Transect)*factor(AMPM),
       data = Beesdat, overlap = 0, xlab = c("Temperature", "Transect"), ylab = c("Bees", "AMPM"), 
       panel = panel.smooth2)
#Note: The panel.smooth2 function in our library
#file MyLibrary.R uses span 2/3.
#In the book, we used span=0.5. You can easily
#change this.



############################################
# 
#   Step (ii). FIND OPTIMAL RANDOM STRUCTURE 

#   The starting model has fixed terms:
#   different non-linear temperature effect for each transect
#   different non-linear PercFlower effect for each transect
#   Transect*AMPM interaction term 

# DO WE NEED AUTO-CORRELATION OR NOT?
# Let's compare the models: 
# (A) with autocorrelation and 
# (B) without auto-correlation

T1 = as.numeric(Transect == 1)
T2 = as.numeric(Transect == 2)
T3 = as.numeric(Transect == 3)
T4 = as.numeric(Transect == 4)
T5 = as.numeric(Transect == 5)
T6 = as.numeric(Transect == 6)
T7 = as.numeric(Transect == 7)

# (A) Model with auto-correlation (22.5, page 410)
step_ii.A <- gamm(Bees~ s(Temperature, k = 5, by = T1) + 
                        s(Temperature, k = 5, by = T2) + 
                        s(Temperature, k = 5, by = T3) + 
                        s(Temperature, k = 5, by = T4) + 
                        s(Temperature, k = 5, by = T5) + 
                        s(Temperature, k = 5, by = T6) + 
                        s(Temperature, k = 5, by = T7) + 
                        s(PercFlower, k = 5, by = T1) + 
                        s(PercFlower, k = 5, by = T2) + 
                        s(PercFlower, k = 5, by = T3) + 
                        s(PercFlower, k = 5, by = T4) + 
                        s(PercFlower, k = 5, by = T5) + 
                        s(PercFlower, k = 5, by = T6) + 
                        s(PercFlower, k = 5, by = T7) + 
                        factor(AMPM)*factor(Transect), 
                data = Beesdat, family = gaussian, method = "REML", 
                correlation = corAR1(form = ~ TimeUnit|Transect/Capitula))

# (B) Model without auto-correlation
step_ii.B <- gamm(Bees~ s(Temperature, k = 5, by = T1) + 
                        s(Temperature, k = 5, by = T2) + 
                        s(Temperature, k = 5, by = T3) + 
                        s(Temperature, k = 5, by = T4) + 
                        s(Temperature, k = 5, by = T5) + 
                        s(Temperature, k = 5, by = T6) + 
                        s(Temperature, k = 5, by = T7) + 
                        s(PercFlower, k = 5, by = T1) + 
                        s(PercFlower, k = 5, by = T2) + 
                        s(PercFlower, k = 5, by = T3) + 
                        s(PercFlower, k = 5, by = T4) + 
                        s(PercFlower, k = 5, by = T5) + 
                        s(PercFlower, k = 5, by = T6) + 
                        s(PercFlower, k = 5, by = T7) + 
                        factor(AMPM)*factor(Transect), 
                  data = Beesdat, family = gaussian, method = "REML")

anova(step_ii.A$lme, step_ii.B$lme)
#L-ratio 8.83 and p-value of 0.003 
#indicate that we need the auto-correlation (model A)



# DO WE NEED MODEL WITH DIFFERENT VARIANCES
# (D) per morning-afternoon or
# (E) per transect?


# (D) model with random variance per AMPM
vf1 <- varIdent( form = ~ 1 | AMPM)
vf2 <- Initialize(vf1, data = Beesdat)
lmc <- lmeControl(niterEM = 3300, msMaxIter = 3500)

step_ii.D <- gamm(Bees~ s(Temperature, k = 5, by = T1) + 
                        s(Temperature, k = 5, by = T2) + 
                        s(Temperature, k = 5, by = T3) + 
                        s(Temperature, k = 5, by = T4) + 
                        s(Temperature, k = 5, by = T5) + 
                        s(Temperature, k = 5, by = T6) + 
                        s(Temperature, k = 5, by = T7) + 
                        s(PercFlower, k = 5, by = T1) + 
                        s(PercFlower, k = 5, by = T2) + 
                        s(PercFlower, k = 5, by = T3) + 
                        s(PercFlower, k = 5, by = T4) + 
                        s(PercFlower, k = 5, by = T5) + 
                        s(PercFlower, k = 5, by = T6) + 
                        s(PercFlower, k = 5, by = T7) + 
                        factor(AMPM)*factor(Transect), 
                 data = Beesdat, family = gaussian, method = "REML", 
                 correlation = corAR1(form = ~TimeUnit|Transect/Capitula), 
                 weights = vf2)
#"numerical problems"

# (E) model with random variance per Transect
vf1 <- varIdent( form = ~ 1 | Transect)
vf2 <- Initialize(vf1, data = Beesdat)
lmc <- lmeControl(niterEM = 3300, msMaxIter = 3500)

step_ii.E <- gamm(Bees~ s(Temperature, k = 5, by = T1) + 
                 s(Temperature, k = 5, by = T2) + 
                 s(Temperature, k = 5, by = T3) + 
                 s(Temperature, k = 5, by = T4) + 
                 s(Temperature, k = 5, by = T5) + 
                 s(Temperature, k = 5, by = T6) + 
                 s(Temperature, k = 5, by = T7) + 
                 s(PercFlower, k = 5, by = T1) + 
                 s(PercFlower, k = 5, by = T2) + 
                 s(PercFlower, k = 5, by = T3) + 
                 s(PercFlower, k = 5, by = T4) + 
                 s(PercFlower, k = 5, by = T5) + 
                 s(PercFlower, k = 5, by = T6) + 
                 s(PercFlower, k = 5, by = T7) + 
                 factor(AMPM)*factor(Transect), data = Beesdat, 
           family = gaussian, method = "REML", 
           correlation = corAR1(form = ~TimeUnit|Transect/Capitula), 
           weights = vf2)
#"numerical problems" again



# An alternative way to check whether we need different variance is to
# plot the residuals from model (22.5) (step_ii.A) 
# against the Transect, AMPM and Sex

plot(factor(AMPM), residuals(step_ii.A$gam), 
     names = c("Morning", "Afternoon"), ylab = "Residuals")

plot(factor(Transect), residuals(step_ii.A$gam), 
     xlab = "Transect", ylab = "Residuals")

plot(factor(Sex), residuals(step_ii.A$gam), 
     names = c("Male-fertile", "Male-sterile"), ylab = "Residuals")

# There are no clear differences in spread
# So we go to step (iii) with random component that contains 
# auto-correlation and one variance term (model step_ii.A)



########################################
#
#   Step (iii). FIND OPTIMAL FIXED STRUCTURE

# starting model has 
#   random terms:
#      auto-correlation
#   fixed terms:
#      different non-linear temperature effect for each transect
#      different non-linear PercFlower effect for each transect
#      Transect*AMPM interaction term 


# INSPECTION OF STARTING MODEL
step_ii.A <- gamm(Bees~ s(Temperature, k = 5, by = T1) + 
                        s(Temperature, k = 5, by = T2) + 
                        s(Temperature, k = 5, by = T3) + 
                        s(Temperature, k = 5, by = T4) + 
                        s(Temperature, k = 5, by = T5) + 
                        s(Temperature, k = 5, by = T6) + 
                        s(Temperature, k = 5, by = T7) + 
                        s(PercFlower, k = 5, by = T1) + 
                        s(PercFlower, k = 5, by = T2) + 
                        s(PercFlower, k = 5, by = T3) + 
                        s(PercFlower, k = 5, by = T4) + 
                        s(PercFlower, k = 5, by = T5) + 
                        s(PercFlower, k = 5, by = T6) + 
                        s(PercFlower, k = 5, by = T7) + 
                        factor(AMPM)*factor(Transect), 
               data = Beesdat, family = gaussian, method = "REML", 
               correlation = corAR1(form = ~ TimeUnit|Transect/Capitula))

anova(step_ii.A$gam)
# Most temperature smoothers are significantly different from 0 at the 5%
# Most temperature smoothers have 4 degrees if freedom
# Most PercFlower smoothers have only one degree of freedom 
# Most PercFlower smoothers are non-significant (except for Transect 3 one)

# We drop PercFlower smoothers.



# THEN HOW SHOULD WE ALLOW FOR PercFlower EFFECT?
# compare the models with:
# (A) PercFlower*Transect interaction term or
# (B) linear PercFlower effect 

step_iii.A <- gamm(Bees~ s(Temperature, k = 5, by = T1) + 
                         s(Temperature, k = 5, by = T2) + 
                         s(Temperature, k = 5, by = T3) + 
                         s(Temperature, k = 5, by = T4) + 
                         s(Temperature, k = 5, by = T5) + 
                         s(Temperature, k = 5, by = T6) + 
                         s(Temperature, k = 5, by = T7) + 
                         PercFlower*factor(Transect) + 
                         factor(AMPM)*factor(Transect), 
                   data = Beesdat, family = gaussian, method = "ML", 
                   correlation = corAR1(form = ~TimeUnit|Transect/Capitula))

step_iii.B <- gamm(Bees~ s(Temperature, k = 5, by = T1) + 
                          s(Temperature, k = 5, by = T2) + 
                          s(Temperature, k = 5, by = T3) + 
                          s(Temperature, k = 5, by = T4) + 
                          s(Temperature, k = 5, by = T5) + 
                          s(Temperature, k = 5, by = T6) + 
                          s(Temperature, k = 5, by = T7) + 
                          PercFlower + 
                          factor(AMPM)*factor(Transect), 
                   data = Beesdat, family = gaussian, method = "ML",
                   correlation = corAR1(form = ~TimeUnit|Transect/Capitula))

anova(step_iii.A$lme, step_iii.B$lme)
# The likelihood ratio test indicates that interaction term
# PercFlower*factor(Transect) (model A) is not significant (p-value = 0.19)

#we remove PercFlower*factor(Transect) term from the model

# IS LINEAR PercFlower EFFECT SIGNIFICANT?
# Compare the models:
# (C) with linear PercFlower effect 
# (D) without PercFlower effect

step_iii.C <- gamm(Bees~ s(Temperature, k = 5, by = T1) + 
                         s(Temperature, k = 5, by = T2) + 
                         s(Temperature, k = 5, by = T3) + 
                         s(Temperature, k = 5, by = T4) + 
                         s(Temperature, k = 5, by = T5) + 
                         s(Temperature, k = 5, by = T6) + 
                         s(Temperature, k = 5, by = T7) + 
                         PercFlower + 
                         factor(AMPM)*factor(Transect), 
                   data = Beesdat, family = gaussian, method = "ML", 
                   correlation = corAR1(form = ~TimeUnit|Transect/Capitula))

step_iii.D <- gamm(Bees~ s(Temperature, k = 5, by = T1) + 
                         s(Temperature, k = 5, by = T2) + 
                         s(Temperature, k = 5, by = T3) + 
                         s(Temperature, k = 5, by = T4) + 
                         s(Temperature, k = 5, by = T5) + 
                         s(Temperature, k = 5, by = T6) + 
                         s(Temperature, k = 5, by = T7) + 
                         factor(AMPM)*factor(Transect), 
                  data = Beesdat, family = gaussian, method = "ML", 
                  correlation = corAR1(form = ~TimeUnit|Transect/Capitula))
 
anova(step_iii.C$lme, step_iii.D$lme)
# The likelihood ratio test indicates that 
# main linear term PercFlower is of borderline significance (p-value = 0.05)

#But keep for the moment PercFlower (keep model step_iii.C)



# IS AMPM*Transect INTERACTION TERM SIGNIFICANT?
# Compare the models:
# (E) with AMPM*Transect interaction term 
# (F) without AMPM*Transect term but with separate terms AMPM and Transect

step_iii.E <- gamm(Bees~ s(Temperature, k = 5, by = T1) + 
                         s(Temperature, k = 5, by = T2) + 
                         s(Temperature, k = 5, by = T3) + 
                         s(Temperature, k = 5, by = T4) + 
                         s(Temperature, k = 5, by = T5) + 
                         s(Temperature, k = 5, by = T6) + 
                         s(Temperature, k = 5, by = T7) + 
                         PercFlower + 
                         factor(AMPM)*factor(Transect), 
                  data = Beesdat, family = gaussian, method = "ML", 
                  correlation = corAR1(form = ~TimeUnit|Transect/Capitula))

anova(step_iii.E$gam)
# AMPM*Transect interaction term is not siginificant (p-value = 0.97)


step_iii.F <- gamm(Bees~ s(Temperature, k = 5, by = T1) + 
                         s(Temperature, k = 5, by = T2) + 
                         s(Temperature, k = 5, by = T3) + 
                         s(Temperature, k = 5, by = T4) + 
                         s(Temperature, k = 5, by = T5) + 
                         s(Temperature, k = 5, by = T6) + 
                         s(Temperature, k = 5, by = T7) + 
                         PercFlower + 
                         factor(AMPM) + 
                         factor(Transect), 
                  data = Beesdat, family = gaussian, method = "ML", 
                  correlation = corAR1(form = ~TimeUnit|Transect/Capitula))
 
anova(step_iii.E$lme, step_iii.F$lme)
# The likelihood ratio test also indicates that 
# AMPM*Transect interaction term is not siginificant (p-value = 0.97)



#INSPECT THE RESULTING MODEL (F) (with separate AMPM and Transect terms):
anova(step_iii.F$gam)
# We remove factor(Transect) term as it is the least significant term
#It will be the model (G)



# INSPECT THE RESULTING MODEL (G) (without Transect term):
step_iii.G <- gamm(Bees~ s(Temperature, k = 5, by = T1) + 
                         s(Temperature, k = 5, by = T2) + 
                         s(Temperature, k = 5, by = T3) + 
                         s(Temperature, k = 5, by = T4) + 
                         s(Temperature, k = 5, by = T5) + 
                         s(Temperature, k = 5, by = T6) + 
                         s(Temperature, k = 5, by = T7) + 
                         PercFlower + 
                         factor(AMPM), 
                  data = Beesdat, family = gaussian, method = "ML", 
                  correlation = corAR1(form = ~TimeUnit|Transect/Capitula))
anova(step_iii.G$gam)
# Also we remove PercFlower terms - it is also non-significant term
#It will be the model (H)



# INSPECT THE RESULTING MODEL (H) (with remained AMPM term):
step_iii.H <- gamm(Bees~ s(Temperature, k = 5, by = T1) + 
                         s(Temperature, k = 5, by = T2) + 
                         s(Temperature, k = 5, by = T3) + 
                         s(Temperature, k = 5, by = T4) + 
                         s(Temperature, k = 5, by = T5) + 
                         s(Temperature, k = 5, by = T6) + 
                         s(Temperature, k = 5, by = T7) + 
                         factor(AMPM), 
                  data = Beesdat, family = gaussian, method = "ML", 
                  correlation = corAR1(form = ~TimeUnit|Transect/Capitula))
anova(step_iii.H$gam)
# We remove temperature smoother for transect 7 - it is non-significant term
#It will be the model (I)


# INSPECT THE RESULTING MODEL (I) (whithout temp. smoother for T7):
step_iii.I <- gamm(Bees~ s(Temperature, k = 5, by = T1) + 
                 s(Temperature, k = 5, by = T2) + 
                 s(Temperature, k = 5, by = T3) + 
                 s(Temperature, k = 5, by = T4) + 
                 s(Temperature, k = 5, by = T5) + 
                 s(Temperature, k = 5, by = T6) + 
                 factor(AMPM), 
            data = Beesdat, family = gaussian, method = "ML", 
            correlation = corAR1(form = ~TimeUnit|Transect/Capitula))

# check non-significance of temperature smmother for transect 7
anova(step_iii.I$lme, step_iii.H$lme)
# Likelihood test also confirms 
# non-significance of temperature smoother for transect 7
# Model 22.6 (step_iii.I) - is optimal.


# THE SHAPES OF THE Temperature SMOOTHERS ARE ALL SIMILAR.
# WHY SHOULD WE USE 6 Temperature SMOOTHERS ?
# Compare the models:
# (step_iii.I)  with six temperature smoothers for each (except 7) transect
# (step_iii.II) with single overall temperature smoother
# (step_iii.III) with four temperature smoothers for SEX-AMPM combinations
# (step_iii.IV) with two temperature smoothers for Sex
# (step_iii.V) with two temperature smoothers for AMPM 


# Model (II) single overall temperature smoother
step_iii.II <- gamm(Bees~ s(Temperature, k = 5)  + 
                          factor(AMPM), 
                   data = Beesdat, family = gaussian, method = "ML", 
                   correlation = corAR1(form = ~TimeUnit|Transect/Capitula))


# Model (III) four temperature smoothers for Sex-AMPM combinations
SA1 = as.numeric(AMPM == 0 & Sex == 0)
SA2 = as.numeric(AMPM == 1 & Sex == 0)
SA3 = as.numeric(AMPM == 0 & Sex == 1)
SA4 = as.numeric(AMPM == 1 & Sex == 1)
step_iii.III <- gamm(Bees~s(Temperature, k = 5, by = SA1) + 
                          s(Temperature, k = 5, by = SA2) + 
                          s(Temperature, k = 5, by = SA3) + 
                          s(Temperature, k = 5, by = SA4) + 
                          factor(AMPM), 
                   data = Beesdat, family = gaussian, method = "ML", 
                   correlation = corAR1(form = ~TimeUnit|Transect/Capitula))


# Model (IV) two temperature smoothers for Sex
S1 = as.numeric(Sex == 0)
S2 = as.numeric(Sex == 1)
step_iii.IV <- gamm(Bees~ s(Temperature, k = 5, by = S1) + 
                          s(Temperature, k = 5, by = S2) + 
                          factor(AMPM), 
                  data = Beesdat, family = gaussian, method = "ML", 
                  correlation = corAR1(form = ~TimeUnit|Transect/Capitula))


# Model (V) two temperature smoothers for AMPM
A1 = as.numeric(AMPM == 0)
A2 = as.numeric(AMPM == 1)
step_iii.V <- gamm(Bees~s(Temperature, k = 5, by = A1) + 
                        s(Temperature, k = 5, by = A2) + 
                        factor(AMPM), 
                  data = Beesdat, family = gaussian, method = "ML", 
                  correlation = corAR1(form = ~TimeUnit|Transect/Capitula))



#   Compare models (22.6) (I)
#   and model with two temperature smoothers conditional of AMPM (V)
anova(step_iii.I$lme, step_iii.V$lme)
#   The model (V) is not better 
#   The likelihood ratio test gave p-value of 0.53



#   Compare models I-IV. Anova table at the page 411
anova(step_iii.I$lme, step_iii.II$lme, step_iii.III$lme, step_iii.IV$lme)
#   FINAL
#   THE MOST OPTIMAL MODEL: step_iii.IV (22.7)
#   i.e. the model with two temperature smoothers for Sex




#############################
# MODEL VALIDATION

#   Numerical output of the resulting model (page 411)
summary(step_iii.IV$gam)
#   We should not consider the time of sampling (AMPM) as 
#   important variable because p-value close to 0.05




#      Figure 22.4
#A
plot(step_iii.IV$gam, select = 1)
text(13, 0.6, "Male-fertile", cex = 3, pos = 4)

#B
plot(step_iii.IV$gam, select = 2)
text(13, 0.6, "Male-sterile", cex = 3, pos = 4)




#      Figure 22.5
par(mfrow = c(1, 2), mar = c(2.5, 2.5, 2.5, 0.5))
hist(residuals(step_iii.IV$lme)[Sex == 1], nclass = 12, 
     xlab = "", ylab = "", main = "male-sterile")
hist(residuals(step_iii.IV$lme)[Sex == 0], nclass = 12, 
     xlab = "", ylab = "", main = "male-fertile")
par(mfrow = c(1, 1), mar = c(4.5, 4.5, 0.5, 0.5))




#      Figure 22.6
model_fit <- fitted(step_iii.IV$gam)
model_resid <- resid(step_iii.IV$gam, type = "p")
MyNames <- Sex
MyNames[Sex == 0] <- "male-sterile"
MyNames[Sex == 1] <- "male-fertile"

library(lattice)

xyplot(model_fit~model_resid | MyNames, col = 1, 
       xlab = "Fitted values", ylab = "Residuals", 
       layout = c(1, 2), par.settings = list(background = "white"))




#      Figure 22.7
# A
plot(factor(Sex), residuals(step_iii.IV$gam), 
     xlab = "Sex", ylab = "Residuals", 
     names = c("Male-fertile", "Male-sterile"))

# B
plot(factor(Transect), residuals(step_iii.IV$gam), 
     xlab = "Transect", ylab = "Residuals")




#      Figure 22.8
model_resid <- resid(step_iii.IV$gam, type = "p")
xyplot(model_resid~Temperature | MyNames, col = 1, 
       xlab = "Temperature", ylab = "Residuals", layout = c(1, 2))

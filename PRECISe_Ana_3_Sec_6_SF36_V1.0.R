###############################################################################|
### PRECISe Analysis of secondary outcomes: SF-36 PCS, MCS and total
### Julia Bels & Sander van Kuijk, February 2024
###
### This script is to model SF-36 scores over the course of 
### follow-up, and compare the course between the two treatment allocations.
###
### This script is run after data prepping.
###
###############################################################################|

options(scipen = 999)

### Install packages if not installed previously ----

list.of.packages <- c("nlme", "lme4", "lattice", "car", "tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

### Load packages ----

lapply(list.of.packages, library, character.only = TRUE)
rm(list = ls())

### Read in environment ----

setwd("L:/SPEC/ICU/RESEARCH/PRECISe/Analyses")
load("PRECISeobjects_V1.0.RData")
rm(t)

### Analyses ----

## rename
l <- questLong
rm(questLong)

## Merge analysis variables into long data
pal <- subset(p, select = c("Participant.Id", "Institute"))
l <- merge(l, pal, by = "Participant.Id", all = TRUE); rm(pal)


## Outcomes PCS and MCS
# PCS
class(l$PCS2)
range(l$PCS2, na.rm = TRUE)
hist(l$PCS2, xlab = "SF-36 PCS", main = NULL)
sum(!is.na(l$PCS2))
l$fupday <- as.numeric(l$fupday)

## Visual inspection of patient-level data
lm.group1 = lmList(PCS2 ~ I(fupday - 30)|Participant.Id,
                   subset = studyfeed == "Group 1", data = l, na.action = na.omit)
lm.group2 = lmList(PCS2 ~ I(fupday - 30)|Participant.Id,
                   subset = studyfeed == "Group 2", data = l, na.action = na.omit)
group1.coef <- coef(lm.group1)
group2.coef <- coef(lm.group2)

setwd("L:/SPEC/ICU/RESEARCH/PRECISe/Results/Figures")

pdf("int_and_slopes_PCS.pdf", width = 8, height = 4.5)
op <- par(mfrow = c(1, 2))
boxplot(group1.coef[, 1], group2.coef[, 1], main = "Intercepts \n at 30 days",
        names = c("Group 1", "Group 2"), ylab = "SF-36 PCS")
boxplot(group1.coef[, 2], group2.coef[, 2], main="Slopes",
        names = c("Group 1", "Group 2"))
par(op)
dev.off()

rm(list=setdiff(ls(), c("l")))

# MCS
class(l$MCS2)
range(l$MCS2, na.rm = TRUE)
hist(l$MCS2, xlab = "SF-36 MCS", main = NULL)
sum(!is.na(l$MCS2))

## Visual inspection of patient-level data
lm.group1 = lmList(MCS2 ~ I(fupday - 30)|Participant.Id,
                   subset = studyfeed == "Group 1", data = l, na.action = na.omit)
lm.group2 = lmList(MCS2 ~ I(fupday - 30)|Participant.Id,
                   subset = studyfeed == "Group 2", data = l, na.action = na.omit)
group1.coef <- coef(lm.group1)
group2.coef <- coef(lm.group2)

pdf("int_and_slopes_MCS.pdf", width = 8, height = 4.5)
op <- par(mfrow = c(1, 2))
boxplot(group1.coef[, 1], group2.coef[, 1], main = "Intercepts \n at 30 days",
        names = c("Group 1", "Group 2"), ylab = "SF-36 MCS")
boxplot(group1.coef[, 2], group2.coef[, 2], main="Slopes",
        names = c("Group 1", "Group 2"))
par(op)
dev.off()

rm(list=setdiff(ls(), c("l")))

# Total score
l$TOS2 <- (l$PCS2 + l$MCS2)/2
range(l$TOS2, na.rm = TRUE)
hist(l$TOS2, xlab = "SF-36 TOS", main = NULL)
sum(!is.na(l$TOS2))

## Visual inspection of patient-level data
lm.group1 = lmList(TOS2 ~ I(fupday - 30)|Participant.Id,
                   subset = studyfeed == "Group 1", data = l, na.action = na.omit)
lm.group2 = lmList(TOS2 ~ I(fupday - 30)|Participant.Id,
                   subset = studyfeed == "Group 2", data = l, na.action = na.omit)
group1.coef <- coef(lm.group1)
group2.coef <- coef(lm.group2)

pdf("int_and_slopes_TOS.pdf", width = 8, height = 4.5)
op <- par(mfrow = c(1, 2))
boxplot(group1.coef[, 1], group2.coef[, 1], main = "Intercepts \n at 30 days",
        names = c("Group 1", "Group 2"), ylab = "SF-36 Total score")
boxplot(group1.coef[, 2], group2.coef[, 2], main="Slopes",
        names = c("Group 1", "Group 2"))
par(op)
dev.off()

## Build lme-model step-by-step
# Helper variables
l$incr <- as.numeric(factor(l$fupday))

# Make sure coefficient is mean fu
l$fupdaym <- scale(l$fupday, center = TRUE, scale = FALSE)

## PCS
# Random intercept on hospital and on patient level
fit_1 <- lme(PCS2 ~ studyfeed*fupdaym, 
             data = l,
             na.action = na.omit,
             random = list(Institute = ~ 1,
                           Participant.Id = ~ 1))
summary(fit_1)
AIC(fit_1) # 6557.932

# As fit_1, but random slope on patient- and hospital level, no covariance structure RE
fit_2a <- update(fit_1, random = list(Institute = pdDiag(~ 1 + fupdaym),
                                      Participant.Id = pdDiag(~ 1 + fupdaym)))
summary(fit_2a)
AIC(fit_2a) # 6544.969 (better fit)

# As fit_1, but random slope on patient-level, UN covariance structure RE
fit_2b <- update(fit_1, random = list(Institute = ~ 1 + fupdaym,
                                      Participant.Id = ~ 1 + fupdaym))
summary(fit_2b)
AIC(fit_2b) # 6519.591 (better fit)

# As fit_2a, but but taking correlation over time into account
fit_3a <- update(fit_2b, correlation =
                   corAR1(form = ~ fupdaym | Institute/Participant.Id))
summary(fit_3a)
AIC(fit_3a) # 6521 (worse)

fit_3b <- update(fit_2b, correlation =
                   corSymm(form = ~ incr | Institute/Participant.Id),
                 control = list(opt = "optim"))
summary(fit_3b)
AIC(fit_3b) # 6488 (best fit!)

rm(list=setdiff(ls(), c("l", "fit_3b")))

# Homogeneity of variance means that the variance of the residuals is constant
# over values of the predictor variables, which are studyfeed and fupday

plot(fit_3b, resid(., type = "p") ~ fitted(.) | studyfeed)      # No evidence
plot(fit_3b, factor(fupdaym) ~ resid(.), abline = 0, ylab = "") # Some minor evidence

# Now test homogeneity of variance formally
dl_fitted <- fit_3b$data
keep <- c("Participant.Id", "fupdaym", "PCS2", "Institute",
          "studyfeed", "incr")
dl_fitted <- subset(dl_fitted, select = keep)
dl_fitted <- dl_fitted[complete.cases(dl_fitted), ]

dl_fitted$fit_3b.Res <- residuals(fit_3b)
dl_fitted$Abs.fit_3b.Res <- abs(dl_fitted$fit_3b.Res)
dl_fitted$fit_3b.Res2 <- dl_fitted$fit_3b.Res^2

Levene.Model.1 <- lm(fit_3b.Res2 ~ studyfeed, data = dl_fitted)
anova(Levene.Model.1) # No heteroscedasticity for predictor group

Levene.Model.2 <- lm(fit_3b.Res2 ~ fupdaym, data = dl_fitted)
anova(Levene.Model.2) # Some evidence of heteroscedasticity for fupday

# Normality of residuals 
qqnorm(fit_3b, ~resid(.) | studyfeed) # Perfect
histogram(~ dl_fitted$fit_3b.Res | dl_fitted$studyfeed, xlab = "Residuals")
# Conclusion: no deviation from normality

rm(list=setdiff(ls(), c("l", "fit_3b")))

# Coefficient of PCS2, including 95% confidence interval
round(intervals(fit_3b, which = "fixed")$fixed[2, ], 3)
# It's associated p-value = 0.591

## Repeat this for MCS

# Random intercept on hospital and on patient level
fit_1 <- lme(MCS2 ~ studyfeed*fupdaym, 
             data = l,
             na.action = na.omit,
             random = list(Institute = ~ 1,
                           Participant.Id = ~ 1))
summary(fit_1)
AIC(fit_1) # 6683.77

# As fit_1, but random slope on patient- and hospital level, no covariance structure RE
fit_2a <- update(fit_1, random = list(Institute = pdDiag(~ 1 + fupdaym),
                                      Participant.Id = pdDiag(~ 1 + fupdaym)))
summary(fit_2a)
AIC(fit_2a) # 6676.187 (better fit)

# As fit_1, but random slope on patient-level, UN covariance structure RE
fit_2b <- update(fit_1, random = list(Institute = ~ 1 + fupdaym,
                                      Participant.Id = ~ 1 + fupdaym))
summary(fit_2b)
AIC(fit_2b) # 6672.744 (better)

# As fit_2b, but but taking correlation over time into account
fit_3a <- update(fit_2b, correlation =
                   corAR1(form = ~ fupdaym | Institute/Participant.Id))
summary(fit_3a)
AIC(fit_3a) # 6674.744 (worse)

fit_3b <- update(fit_2a, correlation =
                   corSymm(form = ~ incr | Institute/Participant.Id),
                 control = list(opt = "optim"))
summary(fit_3b)
AIC(fit_3b) # 6631.268 (best fit!)

rm(list=setdiff(ls(), c("l", "fit_3b")))

# Homogeneity of variance means that the variance of the residuals is constant
# over values of the predictor variables, which are studyfeed and fupday

plot(fit_3b, resid(., type = "p") ~ fitted(.) | studyfeed)      # OK
plot(fit_3b, factor(fupdaym) ~ resid(.), abline = 0, ylab = "") # OK

# Now test homogeneity of variance formally
dl_fitted <- fit_3b$data
keep <- c("Participant.Id", "fupdaym", "MCS2", "Institute",
          "studyfeed", "incr")
dl_fitted <- subset(dl_fitted, select = keep)
dl_fitted <- dl_fitted[complete.cases(dl_fitted), ]

dl_fitted$fit_3b.Res <- residuals(fit_3b)
dl_fitted$Abs.fit_3b.Res <- abs(dl_fitted$fit_3b.Res)
dl_fitted$fit_3b.Res2 <- dl_fitted$fit_3b.Res^2

Levene.Model.1 <- lm(fit_3b.Res2 ~ studyfeed, data = dl_fitted)
anova(Levene.Model.1) # No heteroscedasticity for predictor group

Levene.Model.2 <- lm(fit_3b.Res2 ~ fupdaym, data = dl_fitted)
anova(Levene.Model.2) # No heteroscedasticity for fupday

# Normality of residuals 
qqnorm(fit_3b, ~resid(.) | studyfeed) # OK
histogram(~ dl_fitted$fit_3b.Res | dl_fitted$studyfeed, xlab = "Residuals")
# Conclusion: no deviation from normality

rm(list=setdiff(ls(), c("l", "fit_3b")))

# Coefficient of MCS2, including 95% confidence interval
round(intervals(fit_3b, which = "fixed")$fixed[2, ], 3)
# It's associated p-value = 0.273

## Repeat this for the total score

# Random intercept on hospital and on patient level
fit_1 <- lme(TOS2 ~ studyfeed*fupdaym, 
             data = l,
             na.action = na.omit,
             random = list(Institute = ~ 1,
                           Participant.Id = ~ 1))
summary(fit_1)
AIC(fit_1) # 6535.143

# As fit_1, but random slope on patient- and hospital level, no covariance structure RE
fit_2a <- update(fit_1, random = list(Institute = pdDiag(~ 1 + fupdaym),
                                      Participant.Id = pdDiag(~ 1 + fupdaym)))
summary(fit_2a)
AIC(fit_2a) # 6523.475 (better fit)

# As fit_1, but random slope on patient-level, UN covariance structure RE
fit_2b <- update(fit_1, random = list(Institute = ~ 1 + fupdaym,
                                      Participant.Id = ~ 1 + fupdaym))
summary(fit_2b)
AIC(fit_2b) # 6507.854 (better)

# As fit_2b, but but taking correlation over time into account
fit_3a <- update(fit_2b, correlation =
                   corAR1(form = ~ fupdaym | Institute/Participant.Id))
summary(fit_3a)
AIC(fit_3a) # 6509.854 (worse)

fit_3b <- update(fit_2a, correlation =
                 corSymm(form = ~ incr | Institute/Participant.Id),
                 control = list(opt = "optim"))
summary(fit_3b)
AIC(fit_3b) # 6469.493 (best fit!)

rm(list = setdiff(ls(), c("l", "fit_3b")))

# Homogeneity of variance means that the variance of the residuals is constant
# over values of the predictor variables, which are studyfeed and fupday

plot(fit_3b, resid(., type = "p") ~ fitted(.) | studyfeed)      # OK
plot(fit_3b, factor(fupdaym) ~ resid(.), abline = 0, ylab = "") # OK

# Now test homogeneity of variance formally
dl_fitted <- fit_3b$data
keep <- c("Participant.Id", "fupdaym", "TOS2", "Institute",
          "studyfeed", "incr")
dl_fitted <- subset(dl_fitted, select = keep)
dl_fitted <- dl_fitted[complete.cases(dl_fitted), ]

dl_fitted$fit_3b.Res <- residuals(fit_3b)
dl_fitted$Abs.fit_3b.Res <- abs(dl_fitted$fit_3b.Res)
dl_fitted$fit_3b.Res2 <- dl_fitted$fit_3b.Res^2

Levene.Model.1 <- lm(fit_3b.Res2 ~ studyfeed, data = dl_fitted)
anova(Levene.Model.1) # No heteroscedasticity for predictor group

Levene.Model.2 <- lm(fit_3b.Res2 ~ fupdaym, data = dl_fitted)
anova(Levene.Model.2) # No heteroscedasticity for fupday

# Normality of residuals 
qqnorm(fit_3b, ~resid(.) | studyfeed) # OK
histogram(~ dl_fitted$fit_3b.Res | dl_fitted$studyfeed, xlab = "Residuals")
# Conclusion: no deviation from normality

rm(list = setdiff(ls(), c("l", "fit_3b")))

# Coefficient of TOS2, including 95% confidence interval
round(intervals(fit_3b, which = "fixed")$fixed[2, ], 3)
# It's associated p-value = 0.413

### End of file.
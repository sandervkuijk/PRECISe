###############################################################################|
### PRECISe Analysis of the primary outcome in survivors only 
### Sander van Kuijk, January 2024
###
### This script is to model the primary outcome over the course of follow-up,
### and compare the course between the two treatment allocations.
###
### This script is run after data prepping.
###
###############################################################################|

options(scipen = 999)

### Install packages if not installed previously ----

list.of.packages <- c("nlme", "lme4", "lattice", "car")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

### Load packages ----

lapply(list.of.packages, library, character.only = TRUE)
rm(list = ls())

### Read in environment ----

setwd("L:/SPEC/ICU/RESEARCH/PRECISe/Analyses")
load("PRECISeobjects_V1.0.RData")
rm(i, t)

### Analyses ----

## Short name for long data.frame

dl <- questLong
rm(questLong)

## Number of observations level 1 and level 2

length(dl$Participant.Id)
length(unique(dl$Participant.Id))

## Treatment allocation
table(dl$studyfeed)

## Merge analysis variables into long data
pal <- subset(p, select = c("Participant.Id", "Institute",
                            "EQ5D.HUS.proxy.0", "DEM_AGE", "DEM_SEX",
                            "DEM_BMICALC", "APACHEscore", "NRSscore",
                            "ADM_SYSTEMDIAGNOSIS", "ADM_TYPEOPT"))
dl <- merge(dl, pal, by = "Participant.Id", all = TRUE); rm(pal)

dl$ADM_SYSTEMDIAGNOSIS <- factor(dl$ADM_SYSTEMDIAGNOSIS)
dl$ADM_TYPEOPT[dl$ADM_TYPEOPT == 3] <- 2
dl$ADM_TYPEOPT <- factor(dl$ADM_TYPEOPT, levels = c(1, 2),
                         labels = c("Nonoperative", "Operative"))

## Primary outcome: EQ5D.HUS, without deceased = 0
class(dl$EQ5D.HUS)
range(dl$EQ5D.HUS, na.rm = TRUE) # Is lowest value OK?
hist(dl$EQ5D.HUS, xlab = "EQ-5D-5L HUS", main = NULL)
sum(!is.na(dl$EQ5D.HUS)) # Now manu missings, as those who died have not
                         # received a value of 0.

## Visual inspection of patient-level data
lm.group1 = lmList(EQ5D.HUS ~ I(fupday - 30)|Participant.Id,
                   subset = studyfeed == "Group 1", data = dl)
lm.group2 = lmList(EQ5D.HUS ~ I(fupday - 30)|Participant.Id,
                   subset = studyfeed == "Group 2", data = dl)
group1.coef <- coef(lm.group1)
group2.coef <- coef(lm.group2)

setwd("L:/SPEC/ICU/RESEARCH/PRECISe/Results/Figures")

pdf("int_and_slopes_survivors.pdf", width = 8, height = 4.5)
op <- par(mfrow = c(1, 2))
boxplot(group1.coef[, 1], group2.coef[, 1], main = "Intercepts \n at 30 days",
        names = c("Group 1", "Group 2"), ylab = "EQ-5D-5L HUS")
boxplot(group1.coef[, 2], group2.coef[, 2], main="Slopes",
        names = c("Group 1", "Group 2"))
par(op)
dev.off()

rm(list=setdiff(ls(), c("p", "dl")))

## Build lme-model step-by-step
# Helper variables
dl$incr <- as.numeric(factor(dl$fupday))

# Make sure coefficient is mean fu
dl$fupdaym <- scale(dl$fupday, center = TRUE, scale = FALSE)

# Random intercept on hospital and on patient level
fit_1 <- lme(EQ5D.HUS ~ studyfeed*fupdaym +
             EQ5D.HUS.proxy.0, data = dl,
             na.action = na.omit,
             random = list(Institute = ~ 1,
                           Participant.Id = ~ 1))
summary(fit_1)
AIC(fit_1)

# As fit_1, but random slope on patient-level, no covariance structure RE
fit_2a <- update(fit_1, random = list(Institute = pdDiag(~ 1 + fupdaym),
                        Participant.Id = pdDiag(~ 1 + fupdaym)))
summary(fit_2a)
AIC(fit_2a)

# As fit_1, but random slope on patient-level, UN covariance structure RE
fit_2b <- update(fit_1, random = list(Institute = ~ 1 + fupdaym,
                        Participant.Id = ~ 1 + fupdaym))
summary(fit_2b)
AIC(fit_2b)

# As fit_2b, but but taking correlation over time into account
fit_3a <- update(fit_2b, correlation =
                 corAR1(form = ~ fupdaym | Institute/Participant.Id))
summary(fit_3a)
AIC(fit_3a)

fit_3b <- update(fit_2b, correlation =
                 corSymm(form = ~ incr | Institute/Participant.Id),
                 control = list(opt = "optim"))
summary(fit_3b)
AIC(fit_3b) # Again the best fitting model

rm(list=setdiff(ls(), c("p", "dl", "fit_3b")))

## Hence, model fit_3b is the final model for the primary outcome in those
## who survived, but let's check/ test assumptions.

# Homogeneity of variance means that the variance of the residuals is constant
# over values of the predictor variables, which are studyfeed,  fupday, and
# EQ5D.HUS.proxy.0.

pdf("fit_vs_resid_by_group_survivors.pdf", width = 8, height = 4.5)
plot(fit_3b, resid(., type = "p") ~ fitted(.) | studyfeed)
dev.off() # No obvious difference in variance between groups

pdf("fit_vs_resid_by_fupday_survivors.pdf", width = 8, height = 4.5)
plot(fit_3b, factor(fupdaym) ~ resid(.), abline = 0, ylab = "")
dev.off() # No differences in distributions

# Now test homogeneity of variance formally
dl_fitted <- fit_3b$data
keep <- c("Participant.Id", "fupdaym", "EQ5D.HUS", "Institute",
          "studyfeed", "EQ5D.HUS.proxy.0", "incr")
dl_fitted <- subset(dl_fitted, select = keep)
dl_fitted <- dl_fitted[complete.cases(dl_fitted), ]

dl_fitted$fit_3b.Res <- residuals(fit_3b)
dl_fitted$Abs.fit_3b.Res <- abs(dl_fitted$fit_3b.Res)
dl_fitted$fit_3b.Res2 <- dl_fitted$fit_3b.Res^2

Levene.Model.1 <- lm(fit_3b.Res2 ~ studyfeed, data = dl_fitted)
anova(Levene.Model.1) # No heteroscedasticity for predictor group

Levene.Model.2 <- lm(fit_3b.Res2 ~ fupdaym, data = dl_fitted)
anova(Levene.Model.2) # No heteroscedasticity for predictor follow-up time

Levene.Model.3 <- lm(fit_3b.Res2 ~ EQ5D.HUS.proxy.0, data = dl_fitted)
anova(Levene.Model.3) # No heteroscedasticity for predictor baseline HUS

# Normality of residuals 

pdf("qq_resid_by_group_survivors.pdf", width = 8, height = 4.5)
qqnorm(fit_3b, ~resid(.) | studyfeed)
dev.off() # No evidence of skewness

pdf("hist_by_group_survivors.pdf", width = 8, height = 4.5)
histogram(~ dl_fitted$fit_3b.Res | dl_fitted$studyfeed, xlab = "Residuals")
dev.off() # Both no skewness, only slightly lopsided.

rm(list=setdiff(ls(), c("p", "dl", "fit_3b")))

## Conclusion: both homoscedasticity and normality are OK. Go forward with
## interpretation of coefficients.

# Coefficient of primary outcome, including 95% confidence interval
round(intervals(fit_3b, which = "fixed")$fixed[2, ], 3)
# It's associated p-value = 0.485

### End of file.
###############################################################################|
### PRECISe Analysis of functional secondary outcomes: HADS
### Julia Bels & Sander van Kuijk, February 2024
###
### This script is to model the functional secondary outcomes over the course of 
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

## Outcome
class(l$HADS.total)
range(l$HADS.total, na.rm = TRUE)
hist(l$HADS.total, xlab = "HADS total score", main = NULL)
sum(!is.na(l$HADS.total))
l$fupday <- as.numeric(l$fupday)

## Visual inspection of patient-level data
lm.group1 = lmList(HADS.total ~ I(fupday - 30)|Participant.Id,
                   subset = studyfeed == "Group 1", data = l, na.action = na.omit)
lm.group2 = lmList(HADS.total ~ I(fupday - 30)|Participant.Id,
                   subset = studyfeed == "Group 2", data = l, na.action = na.omit)
group1.coef <- coef(lm.group1)
group2.coef <- coef(lm.group2)

setwd("L:/SPEC/ICU/RESEARCH/PRECISe/Results/Figures")

pdf("int_and_slopes_HADS.pdf", width = 8, height = 4.5)
op <- par(mfrow = c(1, 2))
boxplot(group1.coef[, 1], group2.coef[, 1], main = "Intercepts \n at 30 days",
        names = c("Group 1", "Group 2"), ylab = "HADS total score")
boxplot(group1.coef[, 2], group2.coef[, 2], main="Slopes",
        names = c("Group 1", "Group 2"))
par(op)
dev.off()

rm(list=setdiff(ls(), c("l")))

## Build lme-model step-by-step
# Helper variables
l$incr <- as.numeric(factor(l$fupday))

# Make sure coefficient is mean fu
l$fupdaym <- scale(l$fupday, center = TRUE, scale = FALSE)

# Random intercept on hospital and on patient level
fit_1 <- lme(HADS.total ~ studyfeed*fupdaym, 
             data = l,
             na.action = na.omit,
             random = list(Institute = ~ 1,
                           Participant.Id = ~ 1))
summary(fit_1)
AIC(fit_1) # 5100

# As fit_1, but random slope on patient- and hospital level, no covariance structure RE
fit_2a <- update(fit_1, random = list(Institute = pdDiag(~ 1 + fupdaym),
                                      Participant.Id = pdDiag(~ 1 + fupdaym)))
summary(fit_2a)
AIC(fit_2a) # 5101 (not better)

# As fit_1, but only random slope on patient-level, UN covariance structure RE
fit_2b <- update(fit_1, random = list(Institute = ~ 1,
                                      Participant.Id = ~ 1 + fupdaym),
                 control = list(opt = "optim"))
summary(fit_2b)
AIC(fit_2b) # Higher than AIC for fit_1

## Fit_1 is the final fit as more complex random-effects structures would have led
## to singular fit.

rm(list=setdiff(ls(), c("l", "fit_1")))

# Homogeneity of variance means that the variance of the residuals is constant
# over values of the predictor variables, which are studyfeed and fupday

plot(fit_1, resid(., type = "p") ~ fitted(.) | studyfeed)      # No issues here
plot(fit_1, factor(fupdaym) ~ resid(.), abline = 0, ylab = "") # No substantial issue here

# Now test homogeneity of variance formally
dl_fitted <- fit_1$data
keep <- c("Participant.Id", "fupdaym", "HADS.total", "Institute",
          "studyfeed", "incr")
dl_fitted <- subset(dl_fitted, select = keep)
dl_fitted <- dl_fitted[complete.cases(dl_fitted), ]

dl_fitted$fit_1.Res <- residuals(fit_1)
dl_fitted$Abs.fit_1.Res <- abs(dl_fitted$fit_1.Res)
dl_fitted$fit_1.Res2 <- dl_fitted$fit_1.Res^2

Levene.Model.1 <- lm(fit_1.Res2 ~ studyfeed, data = dl_fitted)
anova(Levene.Model.1) # No heteroscedasticity for predictor group

Levene.Model.2 <- lm(fit_1.Res2 ~ fupdaym, data = dl_fitted)
anova(Levene.Model.2) # Evidence of heteroscedasticity for fupday, however,
                      # more complex models dit not converge
# Normality of residuals 
qqnorm(fit_1, ~resid(.) | studyfeed)
histogram(~ dl_fitted$fit_1.Res | dl_fitted$studyfeed, xlab = "Residuals")
# Conclusion: no deviation from normality

rm(list=setdiff(ls(), c("l", "fit_1")))

# Coefficient of primary outcome, including 95% confidence interval
round(intervals(fit_1, which = "fixed")$fixed[2, ], 3)
# It's associated p-value = 0.148

### End of file.
###############################################################################|
### PRECISe Analysis of functional secondary outcomes: IES-R
### Julia Bels & Sander van Kuijk, February 2024
###
### This script is to model IES-R scores over the course of 
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

## Outcomes IES-R
class(l$IESR_score)
range(l$IESR_score, na.rm = TRUE)
hist(l$IESR_score, xlab = "IES-R score", main = NULL)
sum(!is.na(l$IESR_score))
l$fupday <- as.numeric(l$fupday)

## Visual inspection of patient-level data
lm.group1 = lmList(IESR_score ~ I(fupday - 30)|Participant.Id,
                   subset = studyfeed == "Group 1", data = l, na.action = na.omit)
lm.group2 = lmList(IESR_score ~ I(fupday - 30)|Participant.Id,
                   subset = studyfeed == "Group 2", data = l, na.action = na.omit)
group1.coef <- coef(lm.group1)
group2.coef <- coef(lm.group2)

setwd("L:/SPEC/ICU/RESEARCH/PRECISe/Results/Figures")

pdf("int_and_slopes_IES-R.pdf", width = 8, height = 4.5)
op <- par(mfrow = c(1, 2))
boxplot(group1.coef[, 1], group2.coef[, 1], main = "Intercepts \n at 30 days",
        names = c("Group 1", "Group 2"), ylab = "IES-R")
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
fit_1 <- lme(IESR_score ~ studyfeed*fupdaym, 
             data = l,
             na.action = na.omit,
             random = list(Institute = ~ 1,
                           Participant.Id = ~ 1))
summary(fit_1)
AIC(fit_1) # 7491.332

# As fit_1, but random slope on patient- and hospital level, no covariance structure RE
fit_2a <- update(fit_1, random = list(Institute = pdDiag(~ 1 + fupdaym),
                                      Participant.Id = pdDiag(~ 1 + fupdaym)))
summary(fit_2a)
AIC(fit_2a) # 7481.814 (better fit)

# As fit_1, but random slope on patient-level, UN covariance structure RE
fit_2b <- update(fit_1, random = list(Institute = ~ 1 + fupdaym,
                                      Participant.Id = ~ 1 + fupdaym))
summary(fit_2b)
AIC(fit_2b) # 7482.386 (worse than previous)

# As fit_2a, but but taking correlation over time into account
fit_3a <- update(fit_2a, correlation =
                   corAR1(form = ~ fupdaym | Institute/Participant.Id))
summary(fit_3a)
AIC(fit_3a) # 7483.814 (worse than previous)

fit_3b <- update(fit_2a, correlation =
                   corSymm(form = ~ incr | Institute/Participant.Id),
                 control = list(opt = "optim"))
summary(fit_3b)
AIC(fit_3b) # 7463.177 (best fit)

rm(list = setdiff(ls(), c("l", "fit_3b")))

# Homogeneity of variance means that the variance of the residuals is constant
# over values of the predictor variables, which are studyfeed and fupday

plot(fit_3b, resid(., type = "p") ~ fitted(.) | studyfeed)      # OK
plot(fit_3b, factor(fupdaym) ~ resid(.), abline = 0, ylab = "") # OK

# Now test homogeneity of variance formally
dl_fitted <- fit_3b$data
keep <- c("Participant.Id", "fupdaym", "IESR_score", "Institute",
          "studyfeed", "incr")
dl_fitted <- subset(dl_fitted, select = keep)
dl_fitted <- dl_fitted[complete.cases(dl_fitted), ]

dl_fitted$fit_3b.Res <- residuals(fit_3b)
dl_fitted$Abs.fit_3b.Res <- abs(dl_fitted$fit_3b.Res)
dl_fitted$fit_3b.Res2 <- dl_fitted$fit_3b.Res^2

Levene.Model.1 <- lm(fit_3b.Res2 ~ studyfeed, data = dl_fitted)
anova(Levene.Model.1) # No heteroscedasticity for predictor group

Levene.Model.2 <- lm(fit_3b.Res2 ~ fupdaym, data = dl_fitted)
anova(Levene.Model.2) # No evidence of heteroscedasticity for fupday
                      
# Normality of residuals 
qqnorm(fit_3b, ~resid(.) | studyfeed) # No evidence of non-normality
histogram(~ dl_fitted$fit_3b.Res | dl_fitted$studyfeed, xlab = "Residuals")
# Conclusion: no deviation from normality, only slightly lopsided/ skewed right

rm(list=setdiff(ls(), c("l", "fit_3b")))

# Coefficient of IES-R score, including 95% confidence interval
round(intervals(fit_3b, which = "fixed")$fixed[2, ], 3)
# It's associated p-value = 0.266

### End of file.
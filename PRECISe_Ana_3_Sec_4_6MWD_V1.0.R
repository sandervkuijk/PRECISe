###############################################################################|
### PRECISe Analysis of functional secondary outcomes: 6-minute walking distance
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

## Convert analysis to long data

l <- p[,c("Participant.Id", "Institute","studyfeed", "hospitalmortality",
          "SMWT.pred.30","SMWT.pred.90","SMWT.pred.180")]

l <- l %>% 
  pivot_longer(
    cols = starts_with("SMWT.pred."),
    names_prefix = "SMWT.pred.",
    names_to = "fupday",
    values_to = "SMWT_predicted"
  )

l <- l[l$hospitalmortality == 0,]

## Outcome
class(l$SMWT_predicted)
range(l$SMWT_predicted, na.rm = TRUE)
hist(l$SMWT_predicted, xlab = "6-minute walking distance in % of predicted", main = NULL)
sum(!is.na(l$SMWT_predicted)) # High proportion of missings
l$fupday <- as.numeric(l$fupday)

aggregate(l$SMWT_predicted, by = list(l$fupday), FUN = mean, na.rm = TRUE)

## Visual inspection of patient-level data
lm.group1 = lmList(SMWT_predicted ~ I(fupday - 30)|Participant.Id,
                   subset = studyfeed == "Group 1", data = l, na.action = na.omit)
lm.group2 = lmList(SMWT_predicted ~ I(fupday - 30)|Participant.Id,
                   subset = studyfeed == "Group 2", data = l, na.action = na.omit)
group1.coef <- coef(lm.group1)
group2.coef <- coef(lm.group2)

setwd("L:/SPEC/ICU/RESEARCH/PRECISe/Results/Figures")

pdf("int_and_slopes_6MWT.pdf", width = 8, height = 4.5)
op <- par(mfrow = c(1, 2))
par(mar = c(5.1, 4.1, 4.1, 2.1))
boxplot(group1.coef[, 1], group2.coef[, 1], main = "Intercepts \n at 30 days",
        names = c("Group 1", "Group 2"), ylab = "6MWD")
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
fit_1 <- lme(SMWT_predicted ~ studyfeed*fupdaym, 
             data = l,
             na.action = na.omit,
             random = list(Institute = ~ 1,
                           Participant.Id = ~ 1))
summary(fit_1)
AIC(fit_1) # 4981.543

# As fit_1, but random slope on patient-level, no covariance structure RE
fit_2a <- update(fit_1, random = list(Institute = pdDiag(~ 1 + fupdaym),
                                      Participant.Id = pdDiag(~ 1 + fupdaym)))
summary(fit_2a)
AIC(fit_2a) # 4985 (not better)

# As fit_1, but random slope on patient-level, UN covariance structure RE
fit_2b <- update(fit_1, random = list(Institute = ~ 1 + fupdaym,
                                      Participant.Id = ~ 1 + fupdaym))
# Clear case of overfitting due to limited availability of data. Hence,
# fit_1 will be used as final model.

rm(list=setdiff(ls(), c("l", "fit_1")))

# Homogeneity of variance means that the variance of the residuals is constant
# over values of the predictor variables, which are studyfeed and fupday

plot(fit_1, resid(., type = "p") ~ fitted(.) | studyfeed)       # No issues here
plot(fit_1, factor(fupdaym) ~ resid(.), abline = 0, ylab = "")   # No issues here

# Now test homogeneity of variance formally
dl_fitted <- fit_1$data
keep <- c("Participant.Id", "fupdaym", "SMWT_predicted", "Institute",
          "studyfeed", "incr")
dl_fitted <- subset(dl_fitted, select = keep)
dl_fitted <- dl_fitted[complete.cases(dl_fitted), ]

dl_fitted$fit_1.Res <- residuals(fit_1)
dl_fitted$Abs.fit_1.Res <- abs(dl_fitted$fit_1.Res)
dl_fitted$fit_1.Res2 <- dl_fitted$fit_1.Res^2

Levene.Model.1 <- lm(fit_1.Res2 ~ studyfeed, data = dl_fitted)
anova(Levene.Model.1) # No heteroscedasticity for predictor group

Levene.Model.2 <- lm(fit_1.Res2 ~ fupdaym, data = dl_fitted)
anova(Levene.Model.2) # No heteroscedasticity for fupday

# Normality of residuals 
qqnorm(fit_1, ~resid(.) | studyfeed)
histogram(~ dl_fitted$fit_1.Res | dl_fitted$studyfeed, xlab = "Residuals")
# Conclusion: no significant deviation from normality

rm(list=setdiff(ls(), c("l", "fit_1")))

# Coefficient of primary outcome, including 95% confidence interval
round(intervals(fit_1, which = "fixed")$fixed[2, ], 3)
# It's associated p-value = 0.0484

### End of file.
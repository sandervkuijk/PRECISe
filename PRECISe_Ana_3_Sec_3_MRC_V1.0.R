###############################################################################|
### PRECISe Analysis of the functional secondary outcomes: MRC-SUM
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

list.of.packages <- c("nlme", "lme4", "lattice", "car")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

### Load packages ----

lapply(list.of.packages, library, character.only = TRUE)
rm(list = ls())

### Read in environment ----

setwd("L:/SPEC/ICU/RESEARCH/PRECISe/Analyses/")
load("PRECISeobjects_V1.0.RData")
rm(t)

### Analyses ----

## Convert analysis to long data

l <- p[,c("Participant.Id", "Institute","studyfeed", "hospitalmortality", "MRCSUMscoreD30","MRCSUMscoreD90","MRCSUMscoreD180")]

l <- l %>% 
  pivot_longer(
    cols = starts_with("MRCSUMscoreD"),
    names_prefix = "MRCSUMscoreD",
    names_to = "fupday",
    values_to = "MRCsum"
  )

l <- l[l$hospitalmortality == 0,]

## Outcome
class(l$MRCsum)
range(l$MRCsum, na.rm = TRUE)
hist(l$MRCsum, xlab = "MRC-sum score", main = NULL)
hist(sqrt(l$MRCsum), xlab = "MRC-sum score", main = NULL)
sum(!is.na(l$MRCsum))
l$fupday <- as.numeric(l$fupday)

## Visual inspection of patient-level data
lm.group1 = lmList(MRCsum ~ I(fupday - 30)|Participant.Id,
                   subset = studyfeed == "Group 1", data = l)
lm.group2 = lmList(MRCsum ~ I(fupday - 30)|Participant.Id,
                   subset = studyfeed == "Group 2", data = l)
group1.coef <- coef(lm.group1)
group2.coef <- coef(lm.group2)

setwd("L:/SPEC/ICU/RESEARCH/PRECISe/Results/Figures")

pdf("int_and_slopes_MRC.pdf", width = 8, height = 4.5)
op <- par(mfrow = c(1, 2))
par(mar = c(5.1, 4.1, 4.1, 2.1))
boxplot(group1.coef[, 1], group2.coef[, 1], main = "Intercepts \n at 30 days",
        names = c("Group 1", "Group 2"), ylab = "EQ-5D-5L HUS")
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
fit_1 <- lme(MRCsum ~ studyfeed*fupdaym, 
             data = l,
             na.action = na.omit,
             random = list(Institute = ~ 1,
                           Participant.Id = ~ 1))
summary(fit_1)
AIC(fit_1) # 5906.511

# As fit_1, but random slope on patient-level, no covariance structure RE
fit_2a <- update(fit_1, random = list(Institute = pdDiag(~ 1 + fupdaym),
                                      Participant.Id = pdDiag(~ 1 + fupdaym)))
summary(fit_2a)
AIC(fit_2a) # 5880.83 (better)

# As fit_1, but random slope on patient-level, UN covariance structure RE
fit_2b <- update(fit_1, random = list(Institute = ~ 1 + fupdaym,
                                      Participant.Id = ~ 1 + fupdaym))
summary(fit_2b)
AIC(fit_2b) # 5782.577 (better)

# As fit_2b, but but taking correlation over time into account
fit_3a <- update(fit_2b, correlation =
                   corAR1(form = ~ fupdaym | Institute/Participant.Id))
summary(fit_3a)
AIC(fit_3a) # 5784 (not better)

fit_3b <- update(fit_2b, correlation =
                   corSymm(form = ~ incr | Institute/Participant.Id),
                 control = list(opt = "optim"))
summary(fit_3b)
AIC(fit_3b) # 5762 (better)

rm(list=setdiff(ls(), c("l", "fit_3b")))

## Hence, model fit_3b is the final model, but first, check/ test assumptions.

# Homogeneity of variance means that the variance of the residuals is constant
# over values of the predictor variables, which are studyfeed and fupday

plot(fit_3b, resid(., type = "p") ~ fitted(.) | studyfeed)       # some issues here
plot(fit_3b, factor(fupdaym) ~ resid(.), abline = 0, ylab = "")  # some issues here

# Now test homogeneity of variance formally
dl_fitted <- fit_3b$data
keep <- c("Participant.Id", "fupdaym", "MRCsum", "Institute",
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
qqnorm(fit_3b, ~resid(.) | studyfeed)
histogram(~ dl_fitted$fit_3b.Res | dl_fitted$studyfeed, xlab = "Residuals")

rm(list=setdiff(ls(), c("l", "fit_3b")))

## Conclusion: OK. Slightly asymmetrical, but not substantial, especially given
## that there is clearly no clinically meaningful nor statistically
## significant effect. To be sure, fit a robust LME-model (with simpler
## structure) on the data.

require(robustlmm)

fit_3b_robust <- rlmer(MRCsum ~ studyfeed*fupdaym +  (1|Institute) +
                      (1|Institute:Participant.Id), 
                       data = l, na.action = na.omit)
summary(fit_3b_robust)
summary(fit_3b) # Other sign, but small and insignificant. Hence, no robust
                # estimation.

# Coefficient of primary outcome, including 95% confidence interval
round(intervals(fit_3b, which = "fixed")$fixed[2, ], 3)
# It's associated p-value = 0.729

## End of file.
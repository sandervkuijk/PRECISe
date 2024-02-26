###############################################################################|
### PRECISe Analysis of differences in Discharge destination
### Julia Bels, Februari 2024
###
### This script is run after data prepping.
###
###############################################################################|

options(scipen = 999)

### Install packages if not installed previously ----

list.of.packages <- c("nlme", "lme4", "lmerTest", "car")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

### Load packages ----

lapply(list.of.packages, library, character.only = TRUE)
rm(list = ls())

### Read in environment ----

setwd("L:/SPEC/ICU/RESEARCH/PRECISe/Analyses")
load("PRECISeobjects_V1.0.RData")
rm(questLong, t)

### Analyses ----

table(p$DIS_HOSPOPT)
table(p$DIS_HOSPTXT)

## Simple exploration of an association between group and discharge destination

table(p$DIS_HOSPOPT, p$studyfeed)
round(prop.table(table(p$DIS_HOSPOPT, p$studyfeed), 2)*100, 2)

fisher.test(table(p$DIS_HOSPOPT, p$studyfeed)) # p = 0.825

## Now using multinomial logistic regression. Note that some groups are (too)
## small. We'll use the Begg and Gray Approximation, i.e. fitting separate
## logit models for the J-1 contrasts (Dobson and Barnett, Introduction to
## Generalized Linear Models, 3d ed.).

p$dummy_home <- ifelse(p$DIS_HOSPOPT == "Home", 1 ,0)
p$dummy_nurs <- ifelse(p$DIS_HOSPOPT == "Nursing home", 1 ,0)
p$dummy_reha <- ifelse(p$DIS_HOSPOPT == "Rehabilitation facility", 1 ,0)
p$dummy_hosp <- ifelse(p$DIS_HOSPOPT == "Other hospital", 1 ,0)
p$dummy_othe <- ifelse(p$DIS_HOSPOPT == "Other", 1 ,0)

fit1 <- glmer(dummy_nurs ~ studyfeed + (1|Institute),
              data = p[(p$DIS_HOSPOPT == "Home"|p$DIS_HOSPOPT == "Nursing home"), ],
              family = "binomial")
summary(fit1)
# Odds ratio and 95% confidence interval:
round(exp(fixef(fit1)[2]), 2)
round(exp(confint(fit1, method = "Wald")[3, ]), 2)

fit2 <- glmer(dummy_reha ~ studyfeed + (1|Institute),
              data = p[(p$DIS_HOSPOPT == "Home"|p$DIS_HOSPOPT == "Rehabilitation facility"), ],
              family = "binomial")
summary(fit2)
# Odds ratio and 95% confidence interval:
round(exp(fixef(fit2)[2]), 2)
round(exp(confint(fit2, method = "Wald")[3, ]), 2)

fit3 <- glmer(dummy_hosp ~ studyfeed + (1|Institute),
              data = p[(p$DIS_HOSPOPT == "Home"|p$DIS_HOSPOPT == "Other hospital"), ],
              family = "binomial")
summary(fit3)
# Odds ratio and 95% confidence interval:
round(exp(fixef(fit3)[2]), 2)
round(exp(confint(fit3, method = "Wald")[3, ]), 2)

fit4 <- glmer(dummy_othe ~ studyfeed + (1|Institute),
              data = p[(p$DIS_HOSPOPT == "Home"|p$DIS_HOSPOPT == "Other"), ],
              family = "binomial")
summary(fit4)
# Odds ratio and 95% confidence interval:
round(exp(fixef(fit4)[2]), 2)
round(exp(confint(fit4, method = "Wald")[3, ]), 2)

## Conclusion: no evidence at all for a difference in discharge destination.
## Not all fits without problems due to complexity of models (fit2 and fit3).

### End of file.
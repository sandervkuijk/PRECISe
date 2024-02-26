###############################################################################|
### PRECISe Analysis of differences in return to work
### Julia Bels, Februari 2024
###
### This script is run after data prepping.
###
###############################################################################|

options(scipen = 999)

### Install packages if not installed previously ----

list.of.packages <- c("nlme", "lme4", "lmerTest")
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
# p <- subset(p, p$TERM_WORKOPT != "No (patient did not work before ICU admission)")
# These are not at risk of returning to work, but contribute to the test

p$TERM_WORKOPT <- factor(p$TERM_WORKOPT)
table(p$TERM_WORKOPT, p$studyfeed)
round(prop.table(table(p$TERM_WORKOPT, p$studyfeed), 2)*100)

p$RTW <- as.numeric(p$TERM_WORKOPT) - 1
table(p$RTW)

## Multinomial logistic regression. We'll use the Begg and Gray Approximation,
## i.e. fitting separate logit models for the J-1 contrasts (Dobson and Barnett,
## Introduction to Generalized Linear Models, 3d ed.).

p$dummy_no  <- ifelse(p$TERM_WORKOPT == "No (patient did work before ICU admission)", 1 ,0)
p$dummy_yes <- ifelse(p$TERM_WORKOPT == "Yes", 1 ,0)

fit1 <- glmer(dummy_no ~ studyfeed + (1|Institute), data = p, family = "binomial")
summary(fit1)
# Odds ratio and 95% confidence interval:
round(exp(fixef(fit1)[2]), 2)
round(exp(confint(fit1, method = "Wald")[3, ]), 2)

fit2 <- glmer(dummy_yes ~ studyfeed + (1|Institute), data = p, family = "binomial")
summary(fit2)
# Odds ratio and 95% confidence interval:
round(exp(fixef(fit2)[2]), 2)
round(exp(confint(fit2, method = "Wald")[3, ]), 2)

### End of file.
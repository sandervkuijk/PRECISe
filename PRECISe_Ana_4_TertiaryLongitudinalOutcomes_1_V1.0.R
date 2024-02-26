###############################################################################|
### PRECISe Analysis of differences in mobilization
### Julia Bels, Februari 2024
###
### This script is run after data prepping.
###
###############################################################################|

# > sessionInfo()
# R version 4.0.2 (2020-06-22)
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Running under: Windows 7 x64 (build 7601) Service Pack 1
# 
# Matrix products: default
# 
# locale:
#   [1] LC_COLLATE=Dutch_Netherlands.1252  LC_CTYPE=Dutch_Netherlands.1252
#  LC_MONETARY=Dutch_Netherlands.1252
# [4] LC_NUMERIC=C                       LC_TIME=Dutch_Netherlands.1252    
# 
# attached base packages:
#   [1] stats     graphics  grDevices utils     datasets  methods   base     
# 
# loaded via a namespace (and not attached):
#   [1] compiler_4.0.2 tools_4.0.2

options(scipen = 999)

### Install packages if not installed previously ----

list.of.packages <- c("nlme", "lme4", "lattice", "car", "ordinal")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

### Load packages ----

lapply(list.of.packages, library, character.only = TRUE)
rm(list = ls())

### Read in environment ----

setwd("L:/SPEC/ICU/RESEARCH/PRECISe/Analyses")
load("PRECISeobjects_V1.0.RData")
rm(questLong)

### Analyses ----

## Number of observations level 1 and level 2

length(t$Participant.Id)
length(unique(t$Participant.Id))

## Merge analysis variables into long data
# pal <- subset(p, select = c("Participant.Id", "Institute"))
# t <- merge(t, pal, by = "Participant.Id", all = TRUE); rm(pal)

## Convert to factor
t$mobilization   <- factor(t$mobilization, ordered = TRUE)
t$Institute      <- as.factor(t$Institute)
t$Participant.Id <- as.factor(t$Participant.Id)

## Define model using clmm function of "ordinal" package

# Simplest model
fit1 <- clmm(mobilization ~ studyfeed*ICUDAYCALC + (1 | Institute), 
             data = t, na.action = na.omit, Hess = TRUE)
summary(fit1) #studyfeed: -0.12 p=0.00472
AIC(fit1) # 44670

# Add extra level in random effects: participants are nested within Institutes
fit2 <- clmm(mobilization ~ studyfeed*ICUDAYCALC + (1|Institute) +
             (1|Institute:Participant.Id), data = t, na.action = na.omit,
             Hess = TRUE)

AIC(fit2) # 42084.97 (better)
anova(fit1, fit2) # Confirmed

summary(fit2)
exp(fit2$coefficients[7])
exp(confint(fit2))[7, ]

## Check proportional odds assumption, not available for mixed
fitn <- clm(mobilization ~ studyfeed+ICUDAYCALC, data = t, na.action = na.omit)
nominal_test(fitn)
scale_test(fitn)

### End of file.
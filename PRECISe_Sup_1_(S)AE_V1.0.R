###############################################################################|
### PRECISe - (Serious) adverse events (supplementary appendix)
### Julia Bels, February 2024
### R version 4.3.2
###
### This script is meant for calculation of odds ratios for (serious) adverse
### events.
###
###############################################################################|

# 1. Load data ----

library(lme4)
library(lmerTest)

load("PRECISeobjects_V1.0.RData")
rm(i, t)

# 2. Analyses ----

## 2.1 Pneumatosis intestinalis ----

#Make new variable to assign only SAE that occurred to 09-PRECISe-013 
#(see separate SAE export "PRECISe_study_SAE_export_20240115.csv")
p$SAE <- as.factor(ifelse(p$Participant.Id == "09-PRECISe-013", 1, 0))

#Calculate odds ratio
model <- glmer(SAE ~ studyfeed + (1 | Institute), data = p, family = "binomial") #does not converge

#Simplify
model <- glm(SAE ~ studyfeed, family = "binomial",data = p)
summary(model)
confint(model) #issue persists due to low occurrence of events

# Resolve by computing Fisher's Exact test, as this allows small expected cell count
table(p$SAE, p$studyfeed)
fisher.test(table(p$SAE, p$studyfeed))

## 2.2 Incidence of ventilator-acquired pneumonia ----
p$EI_OPT.Ventilator.acquired.pneumonia..VAP. <- as.factor(p$EI_OPT.Ventilator.acquired.pneumonia..VAP.)
model <- glmer(EI_OPT.Ventilator.acquired.pneumonia..VAP. ~ studyfeed + (1 | Institute), data = p, family = "binomial")
summary(model)
tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed") #OR:  0.93 (0.67, 1.29)

## 2.3 Incidence of acute kidney injury ----
p$newAKI <- as.factor(p$newAKI)
model <- glmer(newAKI ~ studyfeed + (1 | Institute), data = p, family = "binomial")
summary(model)
tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed") #OR:  0.96 (0.69, 1.32)

## 2.4 Refeeding hypophosphatemia ----
p$EI_OPT.Refeeding.hypophosphatemia..phosphate.levels.below..0.65.mmol.l..a.drop..0.16.mmol.L.from.previous.level.in.ICU.and.no.other.explanation.for.hypophosphatemia. <- as.factor(p$EI_OPT.Refeeding.hypophosphatemia..phosphate.levels.below..0.65.mmol.l..a.drop..0.16.mmol.L.from.previous.level.in.ICU.and.no.other.explanation.for.hypophosphatemia.)
model <- glmer(EI_OPT.Refeeding.hypophosphatemia..phosphate.levels.below..0.65.mmol.l..a.drop..0.16.mmol.L.from.previous.level.in.ICU.and.no.other.explanation.for.hypophosphatemia. ~ studyfeed + (1 | Institute), data = p, family = "binomial") #does not converge
summary(model)
tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed") #OR:  1.37 (0.88, 2.13)

## 2.5 Incidence of hepatic dysfunction ----
p$hepaticdysfunction <- as.factor(p$hepaticdysfunction)
model <- glmer(hepaticdysfunction ~ studyfeed + (1 | Institute), data = p, family = "binomial")
summary(model)
tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed") #OR:  0.88 (0.55, 1.41)

## 2.6 Incidence of GI symptoms ----
p$GI_symptoms <- as.factor(p$GI_symptoms)
model <- glmer(GI_symptoms ~ studyfeed + (1 | Institute), data = p, family = "binomial")
summary(model)
tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed") #OR:  1.76 (1.06, 2.92)

### END OF FILE ###
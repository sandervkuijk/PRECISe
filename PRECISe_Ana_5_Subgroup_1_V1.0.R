###############################################################################|
### PRECISe Analysis of the primary outcome 
### Julia Bels & Sander van Kuijk, January 2024
###
### This script is to model the primary outcome over the course of follow-up,
### and compare the course between the two treatment allocations, within
### predefined subgroups.
###
### This script is run after data prepping.
###
###############################################################################|

options(scipen = 999)

## Install packages if not installed previously ----

list.of.packages <- c("nlme", "lme4", "lattice", "car", "tidyverse")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load packages ----

lapply(list.of.packages, library, character.only = TRUE)
rm(list = ls())

## Read in environment ----

setwd("L:/SPEC/ICU/RESEARCH/PRECISe/Analyses")
load("PRECISeobjects_V1.0.RData")
rm(t)

## Preparation ----

## Short name for long data.frame

dl <- questLong
rm(questLong)

## Merge analysis variables into long data, including subgroup variables
pal <- subset(p, select = c("Participant.Id", "Institute",
                            "EQ5D.HUS.proxy.0","ICU_LoS","RR_YN","sub_sex",
                            "sub_age","sub_obesity","sub_admission",
                            "sub_nutrrisk","sub_frailty","sub_comorbidity",
                            "sub_sepsis","sub_disseverity", "sub_AKI",
                            "sub_organfailure","sub_COVID","sub_TBI"))
dl <- merge(dl, pal, by = "Participant.Id", all = TRUE); rm(pal)

# Helper variables
dl$incr <- as.numeric(factor(dl$fupday))

# Make sure coefficient is mean fu
dl$fupdaym <- scale(dl$fupday, center = TRUE, scale = FALSE)

## Define final fit for primary outcome EQ-5D-5L HUS
fit_3b <- lme(EQ5D.HUS.imp0 ~ studyfeed*fupdaym + EQ5D.HUS.proxy.0,
    data = dl, na.action = na.omit,
    correlation = corSymm(form = ~ incr | Institute/Participant.Id),
    random = list(Institute = ~ 1 + fupdaym,
                  Participant.Id = ~ 1 + fupdaym),
    control = list(opt = "optim"))
summary(fit_3b) #This is the correct model used for the primary outcome.

## Analyses ----

### Apply fit_3b to predefined subgroups ----

#### Subgroup: males vs females ----

# Conclusion: only in females sign. result. However, in main sens. analysis, the 
# predictor "sex" was not significantly associated with outcome ...

fit_sex <- lme(EQ5D.HUS.imp0 ~ studyfeed*sub_sex +
               EQ5D.HUS.proxy.0,
               data = dl, na.action = na.omit,
               correlation = corSymm(form = ~ incr | Institute/Participant.Id),
               random = list(Institute = ~ 1 + fupdaym,
                            Participant.Id = ~ 1 + fupdaym),
               control = list(opt = "optim"))
summary(fit_sex) # Significant at the 0.1 level, not 0.05

dl.male <- dl[dl$sub_sex == 1,]
dl.female <- dl[dl$sub_sex != 1,]

# Define fits per subset
fit_3b_male <- lme(EQ5D.HUS.imp0 ~ studyfeed + EQ5D.HUS.proxy.0,
               data = dl.male, na.action = na.omit,
               correlation = corSymm(form = ~ incr | Institute/Participant.Id),
               random = list(Institute = ~ 1,
                            Participant.Id = ~ 1 + fupdaym),
               control = list(opt = "optim"))
summary(fit_3b_male)      # Effect: -0.017, p = 0.564

fit_3b_female <- lme(EQ5D.HUS.imp0 ~ studyfeed + EQ5D.HUS.proxy.0,
                   data = dl.female, na.action = na.omit,
                   correlation = corSymm(form = ~ incr | Institute/Participant.Id),
                   random = list(Institute = ~ 1,
                                 Participant.Id = ~ 1 + fupdaym),
                   control = list(opt = "optim"))
summary(fit_3b_female)    # After simplifying RE due to non-convergence
                          # -1.001, p = 0.007

# Check assumption: these are on ever smaller groups and for exploratory
# purposes, so I'd leave this out. The main model has been checked thoroughly.

rm(list=setdiff(ls(), c("dl")))

#### Subgroup: older vs younger ----

fit_age <- lme(EQ5D.HUS.imp0 ~ studyfeed*sub_age +
               EQ5D.HUS.proxy.0,
               data = dl, na.action = na.omit,
               correlation = corSymm(form = ~ incr | Institute/Participant.Id),
               random = list(Institute = ~ 1 + fupdaym,
                             Participant.Id = ~ 1 + fupdaym),
               control = list(opt = "optim"))
summary(fit_age) # Nope

dl.old <- dl[dl$sub_age == 1,]
dl.young <- dl[dl$sub_age != 1,]

# Define fits per subset
fit_3b_old <- lme(EQ5D.HUS.imp0 ~ studyfeed + EQ5D.HUS.proxy.0,
                   data = dl.old, na.action = na.omit,
                   correlation = corSymm(form = ~ incr | Institute/Participant.Id),
                   random = list(Institute = ~ 1,
                                 Participant.Id = ~ 1 + fupdaym),
                   control = list(opt = "optim"))
summary(fit_3b_old)      # Effect: -0.05, p = 0.079

fit_3b_young <- lme(EQ5D.HUS.imp0 ~ studyfeed + EQ5D.HUS.proxy.0,
                     data = dl.young, na.action = na.omit,
                     correlation = corSymm(form = ~ incr | Institute/Participant.Id),
                     random = list(Institute = ~ 1 + fupdaym,
                                   Participant.Id = ~ 1 + fupdaym),
                     control = list(opt = "optim"))
summary(fit_3b_young)    # Effect: -0.051, p = 0.1193

rm(list=setdiff(ls(), c("dl")))

#### Subgroup: obese vs non-obese ----

fit_obe <- lme(EQ5D.HUS.imp0 ~ studyfeed*sub_obesity +
               EQ5D.HUS.proxy.0,
               data = dl, na.action = na.omit,
               correlation = corSymm(form = ~ incr | Institute/Participant.Id),
               random = list(Institute = ~ 1 + fupdaym,
                             Participant.Id = ~ 1 + fupdaym),
               control = list(opt = "optim"))
summary(fit_obe) # Nope

dl.obese <- dl[dl$sub_obesity == 1,]
dl.nonobese <- dl[dl$sub_obesity != 1,]

# Define fits per subset
fit_3b_obese <- lme(EQ5D.HUS.imp0 ~ studyfeed + EQ5D.HUS.proxy.0,
                    data = dl.obese, na.action = na.omit,
                    random = list(Institute = ~ 1,
                                  Participant.Id = ~ 1),
                    control = list(opt = "optim"))
summary(fit_3b_obese)      # Effect: -0.038, p = 0.381

fit_3b_nonobese <- lme(EQ5D.HUS.imp0 ~ studyfeed + EQ5D.HUS.proxy.0,
                       data = dl.nonobese, na.action = na.omit,
                       random = list(Institute = ~ 1,
                                     Participant.Id = ~ 1),
                       control = list(opt = "optim"))
summary(fit_3b_nonobese)    # Effect: -0.050, p = 0.0797

rm(list=setdiff(ls(), c("dl")))

#### Subgroup: medical vs surgical admission ----

# Conclusion: large difference in medical patients ! No difference in surgical
# patients.

fit_m_s <- lme(EQ5D.HUS.imp0 ~ studyfeed*sub_admission +
               EQ5D.HUS.proxy.0,
               data = dl, na.action = na.omit,
               correlation = corSymm(form = ~ incr | Institute/Participant.Id),
               random = list(Institute = ~ 1 + fupdaym,
                             Participant.Id = ~ 1 + fupdaym),
               control = list(opt = "optim"))
summary(fit_m_s) # At the 0.1 level

dl.medical  <- dl[dl$sub_admission == 0,]
dl.surgical <- dl[dl$sub_admission == 1,]

# Define fits per subset
fit_3b_medical <- lme(EQ5D.HUS.imp0 ~ studyfeed + EQ5D.HUS.proxy.0,
                      data = dl.medical, na.action = na.omit,
                      correlation = corSymm(form = ~ incr | Institute/Participant.Id),
                      random = list(Institute = ~ 1,
                                    Participant.Id = ~ 1 + fupdaym),
                      control = list(opt = "optim"))
summary(fit_3b_medical)      # Effect: -0.0747, p = 0.0095

fit_3b_surgical <- lme(EQ5D.HUS.imp0 ~ studyfeed + EQ5D.HUS.proxy.0,
                       data = dl.surgical, na.action = na.omit,
                       correlation = corSymm(form = ~ incr | Institute/Participant.Id),
                       random = list(Institute = ~ 1,
                                     Participant.Id = ~ 1),
                       control = list(opt = "optim"))
summary(fit_3b_surgical)    # Effect: 0.011, p = 0.781

rm(list=setdiff(ls(), c("dl")))

#### Subgroup: low vs high nutr risk ----

# Grouping factor (sub_nutrrisk) has some missings. Had to remove correlation
# structure for both sets due to convergence issues. Low risk: N = 51 observations!!!

# Conclusion: both subsets get non-significant results

fit_nr <- lme(EQ5D.HUS.imp0 ~ studyfeed*sub_nutrrisk +
              EQ5D.HUS.proxy.0,
              data = dl, na.action = na.omit,
              #correlation = corSymm(form = ~ incr | Institute/Participant.Id),
              random = list(Institute = ~ 1 + fupdaym,
                            Participant.Id = ~ 1 + fupdaym),
              control = list(opt = "optim"))
summary(fit_nr) # At the 0.1 level

dl.highrisk <- dl[dl$sub_nutrrisk %in% 1,] # Waarom %in%, vanwege omgang met NA?
dl.lowrisk <- dl[dl$sub_nutrrisk %in% 0,]

# Define fits per subset
fit_3b_highrisk <- lme(EQ5D.HUS.imp0 ~ studyfeed + EQ5D.HUS.proxy.0,
                       data = dl.highrisk, na.action = na.omit,
                       #correlation = corSymm(form = ~ incr | Institute/Participant.Id),
                       random = list(Institute = ~ 1,
                                     Participant.Id = ~ 1 + fupdaym),
                       control = list(opt = "optim"))
summary(fit_3b_highrisk)      # Effect: -0.043, p = 0.063

fit_3b_lowrisk <- lme(EQ5D.HUS.imp0 ~ studyfeed + EQ5D.HUS.proxy.0,
                      data = dl.lowrisk, na.action = na.omit,
                      #correlation = corSymm(form = ~ incr | Institute/Participant.Id),
                      random = list(Institute = ~ 1,
                                    Participant.Id = ~ 1 + fupdaym),
                     control = list(opt = "optim"))
summary(fit_3b_lowrisk)    # Effect: -0.379, p = 0.0721

rm(list=setdiff(ls(), c("dl")))

#### Subgroup: frail vs non-frail ----

# Had to remove correlation structure for frail patients due to convergence issues. 

# Conclusion: only sign. difference in non-frail patients (much higher N ...)

fit_frail <- lme(EQ5D.HUS.imp0 ~ studyfeed*sub_frailty +
                 EQ5D.HUS.proxy.0,
                 data = dl, na.action = na.omit,
                 correlation = corSymm(form = ~ incr | Institute/Participant.Id),
                 random = list(Institute = ~ 1,
                               Participant.Id = ~ 1 + fupdaym),
                 control = list(opt = "optim"))
summary(fit_frail) # No evidence

dl.frail <- dl[dl$sub_frailty %in% 1,]        #low N (432 observations)
dl.nonfrail <- dl[dl$sub_frailty %in% 0,]

# Define fits per subset
fit_3b_frail <- lme(EQ5D.HUS.imp0 ~ studyfeed + EQ5D.HUS.proxy.0,
                    data = dl.frail, na.action = na.omit,
                    #correlation = corSymm(form = ~ incr | Institute/Participant.Id),
                    random = list(Institute = ~ 1 + fupdaym,
                                  Participant.Id = ~ 1 + fupdaym),
                    control = list(opt = "optim"))
summary(fit_3b_frail)      # Effect: -0.033, p = 0.521

fit_3b_nonfrail <- lme(EQ5D.HUS.imp0 ~ studyfeed + EQ5D.HUS.proxy.0,
                      data = dl.nonfrail, na.action = na.omit,
                      #correlation = corSymm(form = ~ incr | Institute/Participant.Id),
                      random = list(Institute = ~ 1 + fupdaym,
                                    Participant.Id = ~ 1 + fupdaym),
                      control = list(opt = "optim"))
summary(fit_3b_nonfrail)    # Effect: -0.06, p = 0.0279

rm(list=setdiff(ls(), c("dl")))

#### Subgroup: multimorbidity vs no multimorbidity ----

# Had to remove correlation structure for no multimorbidity due to convergence 
# issues. Low risk: N = 705 observations.

# Conclusion: both non-sign., trend in multimorbidity group
fit_com <- lme(EQ5D.HUS.imp0 ~ studyfeed*sub_comorbidity +
               EQ5D.HUS.proxy.0,
               data = dl, na.action = na.omit,
               correlation = corSymm(form = ~ incr | Institute/Participant.Id),
               random = list(Institute = ~ 1 + fupdaym,
                             Participant.Id = ~ 1 + fupdaym),
               control = list(opt = "optim"))
summary(fit_com) # No evidence

dl.highCCI <- dl[dl$sub_comorbidity %in% 1,]
dl.lowCCI  <- dl[dl$sub_comorbidity %in% 0,]


# Define fits per subset
fit_3b_highCCI <- lme(EQ5D.HUS.imp0 ~ studyfeed + EQ5D.HUS.proxy.0,
                      data = dl.highCCI, na.action = na.omit,
                      correlation = corSymm(form = ~ incr | Institute/Participant.Id),
                      random = list(Institute = ~ 1,
                                    Participant.Id = ~ 1 + fupdaym),
                      control = list(opt = "optim"))
summary(fit_3b_highCCI)

fit_3b_lowCCI <- lme(EQ5D.HUS.imp0 ~ studyfeed + EQ5D.HUS.proxy.0,
                     data = dl.lowCCI, na.action = na.omit,
                     correlation = corSymm(form = ~ incr | Institute/Participant.Id),
                     random = list(Institute = ~ 1,
                                   Participant.Id = ~ 1 + fupdaym),
                     control = list(opt = "optim"))
summary(fit_3b_lowCCI)

rm(list=setdiff(ls(), c("dl")))

#### Subgroup: sepsis vs no sepsis ----

# Conclusion: both non-sign., trend in non-sepsis group.
fit_sep <- lme(EQ5D.HUS.imp0 ~ studyfeed*sub_sepsis +
               EQ5D.HUS.proxy.0,
               data = dl, na.action = na.omit,
               correlation = corSymm(form = ~ incr | Institute/Participant.Id),
               random = list(Institute = ~ 1 + fupdaym,
                             Participant.Id = ~ 1 + fupdaym),
               control = list(opt = "optim"))
summary(fit_sep) # No evidence

dl.sepsis <- dl[dl$sub_sepsis %in% 1,]
dl.nosepsis <- dl[dl$sub_sepsis %in% 0,]

# Define fits per subset
fit_3b_sepsis <- lme(EQ5D.HUS.imp0 ~ studyfeed + EQ5D.HUS.proxy.0,
                     data = dl.sepsis, na.action = na.omit,
                     correlation = corSymm(form = ~ incr | Institute/Participant.Id),
                     random = list(Institute = ~ 1,
                                   Participant.Id = ~ 1 + fupdaym),
                     control = list(opt = "optim"))
summary(fit_3b_sepsis)

fit_3b_nosepsis <- lme(EQ5D.HUS.imp0 ~ studyfeed + EQ5D.HUS.proxy.0,
                     data = dl.nosepsis, na.action = na.omit,
                     correlation = corSymm(form = ~ incr | Institute/Participant.Id),
                     random = list(Institute = ~ 1,
                                   Participant.Id = ~ 1 + fupdaym),
                     control = list(opt = "optim"))
summary(fit_3b_nosepsis)

rm(list=setdiff(ls(), c("dl")))

#### Subgroup: high vs low APACHE II score ----

# Conclusion: only sign. in high APACHE II score group
fit_apa <- lme(EQ5D.HUS.imp0 ~ studyfeed*sub_disseverity +
               EQ5D.HUS.proxy.0,
               data = dl, na.action = na.omit,
               correlation = corSymm(form = ~ incr | Institute/Participant.Id),
               random = list(Institute = ~ 1 + fupdaym,
                             Participant.Id = ~ 1 + fupdaym),
               control = list(opt = "optim"))
summary(fit_apa) # No evidence

dl.highAPACHE <- dl[dl$sub_disseverity %in% 1,]
dl.lowAPACHE <- dl[dl$sub_disseverity %in% 0,]

# Define fits per subset
fit_3b_highAPACHE <- lme(EQ5D.HUS.imp0 ~ studyfeed + EQ5D.HUS.proxy.0,
                     data = dl.highAPACHE, na.action = na.omit,
                     correlation = corSymm(form = ~ incr | Institute/Participant.Id),
                     random = list(Institute = ~ 1 + fupdaym,
                                   Participant.Id = ~ 1 + fupdaym),
                     control = list(opt = "optim"))
summary(fit_3b_highAPACHE)

fit_3b_lowAPACHE <- lme(EQ5D.HUS.imp0 ~ studyfeed + EQ5D.HUS.proxy.0,
                       data = dl.lowAPACHE, na.action = na.omit,
                       correlation = corSymm(form = ~ incr | Institute/Participant.Id),
                       random = list(Institute = ~ 1 + fupdaym,
                                     Participant.Id = ~ 1 + fupdaym),
                       control = list(opt = "optim"))
summary(fit_3b_lowAPACHE)

rm(list=setdiff(ls(), c("dl")))

#### Subgroup: AKI vs non-AKI ----

# Conclusion: only sign. in non-AKI group, which is surprising ....
fit_aki <- lme(EQ5D.HUS.imp0 ~ studyfeed*sub_AKI +
               EQ5D.HUS.proxy.0,
               data = dl, na.action = na.omit,
               correlation = corSymm(form = ~ incr | Institute/Participant.Id),
               random = list(Institute = ~ 1 + fupdaym,
                             Participant.Id = ~ 1 + fupdaym),
               control = list(opt = "optim"))
summary(fit_aki) # No evidence

dl.AKI    <- dl[dl$sub_AKI %in% 1,]
dl.nonAKI <- dl[dl$sub_AKI %in% 0,]

# Define fits per subset
fit_3b_AKI <- lme(EQ5D.HUS.imp0 ~ studyfeed + EQ5D.HUS.proxy.0,
                         data = dl.AKI, na.action = na.omit,
                         #correlation = corSymm(form = ~ incr | Institute/Participant.Id),
                         random = list(Institute = ~ 1 + fupdaym,
                                       Participant.Id = ~ 1 + fupdaym),
                         control = list(opt = "optim"))
summary(fit_3b_AKI)

fit_3b_nonAKI <- lme(EQ5D.HUS.imp0 ~ studyfeed + EQ5D.HUS.proxy.0,
                        data = dl.nonAKI, na.action = na.omit,
                        #correlation = corSymm(form = ~ incr | Institute/Participant.Id),
                        random = list(Institute = ~ 1 + fupdaym,
                                      Participant.Id = ~ 1 + fupdaym),
                        control = list(opt = "optim"))
summary(fit_3b_nonAKI)

rm(list=setdiff(ls(), c("dl")))

#### Subgroup: high vs low SOFA ----

# Conclusion: only sign. in low SOFA, which is again surprising
fit_sof <- lme(EQ5D.HUS.imp0 ~ studyfeed*sub_organfailure +
               EQ5D.HUS.proxy.0,
               data = dl, na.action = na.omit,
               correlation = corSymm(form = ~ incr | Institute/Participant.Id),
               random = list(Institute = ~ 1 + fupdaym,
                             Participant.Id = ~ 1 + fupdaym),
               control = list(opt = "optim"))
summary(fit_sof) # No evidence

dl.highSOFA <- dl[dl$sub_organfailure %in% 1,]
dl.lowSOFA  <- dl[dl$sub_organfailure %in% 0,]

# Define fits per subset
fit_3b_highSOFA <- lme(EQ5D.HUS.imp0 ~ studyfeed + EQ5D.HUS.proxy.0,
                       data = dl.highSOFA, na.action = na.omit,
                       correlation = corSymm(form = ~ incr | Institute/Participant.Id),
                       random = list(Institute = ~ 1 + fupdaym,
                                     Participant.Id = ~ 1 + fupdaym),
                       control = list(opt = "optim"))
summary(fit_3b_highSOFA)

fit_3b_lowSOFA <- lme(EQ5D.HUS.imp0 ~ studyfeed + EQ5D.HUS.proxy.0,
                      data = dl.lowSOFA, na.action = na.omit,
                      correlation = corSymm(form = ~ incr | Institute/Participant.Id),
                      random = list(Institute = ~ 1 + fupdaym,
                                    Participant.Id = ~ 1 + fupdaym),
                      control = list(opt = "optim"))
summary(fit_3b_lowSOFA)

rm(list=setdiff(ls(), c("dl")))

#### Subgroup: TBI vs non-TBI ----

# Conclusion: no difference in either subset
fit_tbi <- lme(EQ5D.HUS.imp0 ~ studyfeed*sub_TBI +
               EQ5D.HUS.proxy.0,
               data = dl, na.action = na.omit,
               correlation = corSymm(form = ~ incr | Institute/Participant.Id),
               random = list(Institute = ~ 1 + fupdaym,
                             Participant.Id = ~ 1 + fupdaym),
               control = list(opt = "optim"))
summary(fit_tbi) # No evidence

dl.TBI    <- dl[dl$sub_TBI %in% 1,]
dl.nonTBI <- dl[dl$sub_TBI %in% 0,]

# Define fits per subset
fit_3b_TBI <- lme(EQ5D.HUS.imp0 ~ studyfeed + EQ5D.HUS.proxy.0,
                       data = dl.TBI, na.action = na.omit,
                       correlation = corSymm(form = ~ incr | Institute/Participant.Id),
                       random = list(Institute = ~ 1 + fupdaym,
                                     Participant.Id = ~ 1 + fupdaym),
                       control = list(opt = "optim"))
summary(fit_3b_TBI)

fit_3b_nonTBI <- lme(EQ5D.HUS.imp0 ~ studyfeed + EQ5D.HUS.proxy.0,
                      data = dl.nonTBI, na.action = na.omit,
                      correlation = corSymm(form = ~ incr | Institute/Participant.Id),
                      random = list(Institute = ~ 1 + fupdaym,
                                    Participant.Id = ~ 1 + fupdaym),
                      control = list(opt = "optim"))
summary(fit_3b_nonTBI)

rm(list=setdiff(ls(), c("dl")))

#### Subgroup: COVID-19 vs non-COVID-19 ----

# Conclusion: no difference in either subset
fit_cov <- lme(EQ5D.HUS.imp0 ~ studyfeed*sub_COVID +
               EQ5D.HUS.proxy.0,
               data = dl, na.action = na.omit,
               #correlation = corSymm(form = ~ incr | Institute/Participant.Id),
               random = list(Institute = ~ 1 + fupdaym,
                             Participant.Id = ~ 1 + fupdaym),
               control = list(opt = "optim"))
summary(fit_cov) # No evidence

dl.COVID    <- dl[dl$sub_COVID %in% 1,]
dl.nonCOVID <- dl[dl$sub_COVID %in% 0,]

# Define fits per subset
fit_3b_COVID <- lme(EQ5D.HUS.imp0 ~ studyfeed + EQ5D.HUS.proxy.0,
                  data = dl.COVID, na.action = na.omit,
                  #correlation = corSymm(form = ~ incr | Institute/Participant.Id),
                  random = list(Institute = ~ 1 + fupdaym,
                                Participant.Id = ~ 1 + fupdaym),
                  control = list(opt = "optim"))
summary(fit_3b_COVID)

fit_3b_nonCOVID <- lme(EQ5D.HUS.imp0 ~ studyfeed + EQ5D.HUS.proxy.0,
                     data = dl.nonCOVID, na.action = na.omit,
                     #correlation = corSymm(form = ~ incr | Institute/Participant.Id),
                     random = list(Institute = ~ 1 + fupdaym,
                                   Participant.Id = ~ 1 + fupdaym),
                     control = list(opt = "optim"))
summary(fit_3b_nonCOVID)

rm(list=setdiff(ls(), c("dl")))

### Apply fit_3b to post-randomization groups ----

# First, define subgroups
dl$sub_LoS <- ifelse(dl$ICU_LoS > 7, 1, 0)          # long-stay = 1     short-stay = 0
dl$sub_RRT <- ifelse(dl$RR_YN == 1, 1, 0)               # RRT = 1           no RRT = 0

table(dl$sub_LoS, useNA = "always")

#### Subgroup: long-stay vs short-stay ----

# Longstay -> had to remove correlation structure due to convergence issues.
# Shortstay -> absolute value of AIC is smaller of correlation structure is
# removed but does converge with it.

# Conclusion: no difference in either subset; trend in longstay.
fit_sta <- lme(EQ5D.HUS.imp0 ~ studyfeed*sub_LoS +
               EQ5D.HUS.proxy.0,
               data = dl, na.action = na.omit,
               correlation = corSymm(form = ~ incr | Institute/Participant.Id),
               random = list(Institute = ~ 1 + fupdaym,
                             Participant.Id = ~ 1 + fupdaym),
               control = list(opt = "optim"))
summary(fit_sta) # No evidence

dl.longstay  <- dl[dl$sub_LoS %in% 1,]
dl.shortstay <- dl[dl$sub_LoS %in% 0,]

# Define fits per subset
fit_3b_longstay <- lme(EQ5D.HUS.imp0 ~ studyfeed + EQ5D.HUS.proxy.0,
                    data = dl.longstay, na.action = na.omit,
                    correlation = corSymm(form = ~ incr | Institute/Participant.Id),
                    random = list(Institute = ~ 1 + fupdaym,
                                  Participant.Id = ~ 1 + fupdaym),
                    control = list(opt = "optim"))
summary(fit_3b_longstay)

fit_3b_shortstay <- lme(EQ5D.HUS.imp0 ~ studyfeed + EQ5D.HUS.proxy.0,
                       data = dl.shortstay, na.action = na.omit,
                       correlation = corSymm(form = ~ incr | Institute/Participant.Id),
                       random = list(Institute = ~ 1 + fupdaym,
                                     Participant.Id = ~ 1 + fupdaym),
                       control = list(opt = "optim"))
summary(fit_3b_shortstay)

rm(list=setdiff(ls(), c("dl")))

#### Subgroup: RRT vs no RRT ----

# Conclusion: median EQ-5D HUS in RRT group is 0 (>50% dead), only sign. effect
# in no RRT group.
fit_rrt <- lme(EQ5D.HUS.imp0 ~ studyfeed*sub_RRT +
               EQ5D.HUS.proxy.0,
               data = dl, na.action = na.omit,
               #correlation = corSymm(form = ~ incr | Institute/Participant.Id),
               random = list(Institute = ~ 1 + fupdaym,
                             Participant.Id = ~ 1 + fupdaym),
               control = list(opt = "optim"))
summary(fit_rrt) # No evidence

dl.RRT   <- dl[dl$sub_RRT %in% 1,]
dl.noRRT <- dl[dl$sub_RRT %in% 0,]

# Define fits per subset
fit_3b_RRT <- lme(EQ5D.HUS.imp0 ~ studyfeed + EQ5D.HUS.proxy.0,
                       data = dl.RRT, na.action = na.omit,
                       #correlation = corSymm(form = ~ incr | Institute/Participant.Id),
                       random = list(Institute = ~ 1 + fupdaym,
                                     Participant.Id = ~ 1 + fupdaym),
                       control = list(opt = "optim"))
summary(fit_3b_RRT)

fit_3b_noRRT <- lme(EQ5D.HUS.imp0 ~ studyfeed + EQ5D.HUS.proxy.0,
                        data = dl.noRRT, na.action = na.omit,
                        #correlation = corSymm(form = ~ incr | Institute/Participant.Id),
                        random = list(Institute = ~ 1 + fupdaym,
                                      Participant.Id = ~ 1 + fupdaym),
                        control = list(opt = "optim"))
summary(fit_3b_noRRT)

rm(list=setdiff(ls(), c("dl")))

### End of file.
###############################################################################|
### PRECISe Analysis of overall survival in prespecified subgroups
### Julia Bels & Sander van Kuijk, February 2024
###
### This script is to model overall survivall in prespecified subgroups.
###
### This script is run after data prepping.
###
###############################################################################|

options(scipen = 999)


## Install packages if not installed previously ----

list.of.packages <- c("survival", "rms", "coxme")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

## Load packages ----

lapply(list.of.packages, library, character.only = TRUE)
rm(list = ls())

## Helper function ----

# Extract results form coxme object, including p-value in 3 decimals
extract_coxme_table <- function (x){
  beta <- x$coefficients
  nvar <- length(beta)
  nfrail <- nrow(x$var) - nvar
  se <- sqrt(diag(x$var)[nfrail + 1:nvar])
  z <- round(beta/se, 2)
  p <- signif(1 - pchisq((beta/se)^2, 1), 3)
  table = data.frame(cbind(beta, se, z, p))
  return(table)
}

## Read in environment ----

setwd("L:/SPEC/ICU/RESEARCH/PRECISe/Analyses")
load("PRECISeobjects_V1.0.RData")
rm(list=setdiff(ls(), c("p", "extract_coxme_table")))

## Preparation ----

### Analyses ----
p$Institute <- factor(p$Institute)

## Data exploration
table(p$studyfeed)
p$KM_period

# Make survival objects for figures and analyses
p$KM_period_m <- round(p$KM_period/30.25, 2)
S_mort_d      <- Surv(p$KM_period, event = p$KM_mortality)
S_mort_m      <- Surv(p$KM_period_m, event = p$KM_mortality)

# Define final fit for overall survival in main analysis
fit <- coxme(S_mort_d ~ studyfeed + (1|Institute), data = p)

extract_coxme_table(fit)
paste("HR: ", round(exp(fit$coefficients), 2), ", 95% CI: ",
      round(exp(confint(fit))[1], 2), " - ",
      round(exp(confint(fit))[2], 2), sep = "")


## Analyses ----

### Apply fit to predefined subgroups if possible ----

#### Subgroup: males vs females ----
fit_sex <- coxme(S_mort_d ~ studyfeed*sub_sex + (1|Institute), data = p)
extract_coxme_table(fit_sex)

p.male   <- p[p$sub_sex == 1,]
p.female <- p[p$sub_sex == 0,]

# Define fits per subset
fit_male <- coxme(S_mort_d[p$sub_sex == 1] ~ studyfeed + (1|Institute),
                  data = p.male)
paste("HR: ", round(exp(fit_male$coefficients), 2), ", 95% CI: ",
      round(exp(confint(fit_male))[1], 2), " - ",
      round(exp(confint(fit_male))[2], 2), sep = "")

fit_female <- coxme(S_mort_d[p$sub_sex == 0] ~ studyfeed + (1|Institute),
                    data = p.female)
paste("HR: ", round(exp(fit_female$coefficients), 2), ", 95% CI: ",
      round(exp(confint(fit_female))[1], 2), " - ",
      round(exp(confint(fit_female))[2], 2), sep = "")

rm(list=setdiff(ls(), c("p", "S_mort_d", "S_mort_m", "extract_coxme_table")))

#### Subgroup: older vs younger ----
fit_age <-  coxme(S_mort_d ~ studyfeed*sub_age + (1|Institute), data = p)
extract_coxme_table(fit_age)

p.old   <- p[p$sub_age == 1,]
p.young <- p[p$sub_age != 1,]

# Define fits per subset
fit_old <- coxme(S_mort_d[p$sub_age == 1] ~ studyfeed + (1|Institute),
                 data = p.old)
paste("HR: ", round(exp(fit_old$coefficients), 2), ", 95% CI: ",
      round(exp(confint(fit_old))[1], 2), " - ",
      round(exp(confint(fit_old))[2], 2), sep = "")

fit_young <- coxme(S_mort_d[p$sub_age != 1] ~ studyfeed + (1|Institute),
                   data = p.young)
paste("HR: ", round(exp(fit_young$coefficients), 2), ", 95% CI: ",
      round(exp(confint(fit_young))[1], 2), " - ",
      round(exp(confint(fit_young))[2], 2), sep = "")

rm(list=setdiff(ls(), c("p", "S_mort_d", "S_mort_m", "extract_coxme_table")))

#### Subgroup: obese vs non-obese ----
fit_obe <- coxme(S_mort_d ~ studyfeed*sub_obesity + (1|Institute), data = p)
extract_coxme_table(fit_obe)

p.obese    <- p[p$sub_obesity == 1,]
p.nonobese <- p[p$sub_obesity != 1,]

# Define fits per subset
fit_obese <- coxme(S_mort_d[p$sub_obesity == 1] ~ studyfeed + (1|Institute),
                   data = p.obese)
paste("HR: ", round(exp(fit_obese$coefficients), 2), ", 95% CI: ",
      round(exp(confint(fit_obese))[1], 2), " - ",
      round(exp(confint(fit_obese))[2], 2), sep = "")

fit_nonobese <- coxme(S_mort_d[p$sub_obesity != 1] ~ studyfeed + (1|Institute),
                      data = p.nonobese)
paste("HR: ", round(exp(fit_nonobese$coefficients), 2), ", 95% CI: ",
      round(exp(confint(fit_nonobese))[1], 2), " - ",
      round(exp(confint(fit_nonobese))[2], 2), sep = "")

rm(list=setdiff(ls(), c("p", "S_mort_d", "S_mort_m", "extract_coxme_table")))

#### Subgroup: medical vs surgical admission ----
fit_adm <- coxme(S_mort_d ~ studyfeed*sub_admission + (1|Institute), data = p)
extract_coxme_table(fit_adm)

p.medical  <- p[p$sub_admission == 0,]
p.surgical <- p[p$sub_admission == 1,]

# Define fits per subset
fit_medical <- coxme(S_mort_d[p$sub_admission == 0] ~ studyfeed + (1|Institute),
                     data = p.medical)
paste("HR: ", round(exp(fit_medical$coefficients), 2), ", 95% CI: ",
      round(exp(confint(fit_medical))[1], 2), " - ",
      round(exp(confint(fit_medical))[2], 2), sep = "")

fit_surgical <- coxme(S_mort_d[p$sub_admission == 1] ~ studyfeed + (1|Institute),
                      data = p.surgical)
paste("HR: ", round(exp(fit_surgical$coefficients), 2), ", 95% CI: ",
      round(exp(confint(fit_surgical))[1], 2), " - ",
      round(exp(confint(fit_surgical))[2], 2), sep = "")

rm(list=setdiff(ls(), c("p", "S_mort_d", "S_mort_m", "extract_coxme_table")))

#### Subgroup: low vs high nutr risk ----

table(p$sub_nutrrisk, p$KM_mortality) # Too few events in subgroup, no model fit

fit_nutr_f <- coxme(S_mort_d ~ studyfeed*sub_nutrrisk + (1|Institute), data = p)
fit_nutr_n <- coxph(S_mort_d ~ studyfeed*sub_nutrrisk, data = p)

p.highrisk <- p[p$sub_nutrrisk == 1,]
p.lowrisk  <- p[p$sub_nutrrisk == 0,]

# Define fits per subset
fit_highrisk <- coxme(S_mort_d[p$sub_nutrrisk == 1] ~ studyfeed + (1|Institute),
                      data = p.highrisk)
paste("HR: ", round(exp(fit_highrisk$coefficients), 2), ", 95% CI: ",
      round(exp(confint(fit_highrisk))[1], 2), " - ",
      round(exp(confint(fit_highrisk))[2], 2), sep = "")

fit_lowrisk_f <- coxme(S_mort_d[p$sub_nutrrisk == 0] ~ studyfeed + (1|Institute),
                       data = p.lowrisk)
fit_lowrisk_n <- coxph(S_mort_d[p$sub_nutrrisk == 0] ~ studyfeed,
                       data = p.lowrisk)
# No estimation possible: too few events

rm(list=setdiff(ls(), c("p", "S_mort_d", "S_mort_m", "extract_coxme_table")))

#### Subgroup: frail vs non-frail ----
fit_frailty <- coxme(S_mort_d ~ studyfeed*sub_frailty + (1|Institute), data = p)
extract_coxme_table(fit_frailty)

p.frail    <- p[p$sub_frailty == 1,]
p.nonfrail <- p[p$sub_frailty == 0,]

# Define fits per subset
fit_frail <- coxme(S_mort_d[p$sub_frailty == 1] ~ studyfeed + (1|Institute),
                  data = p.frail)
paste("HR: ", round(exp(fit_frail$coefficients), 2), ", 95% CI: ",
      round(exp(confint(fit_frail))[1], 2), " - ",
      round(exp(confint(fit_frail))[2], 2), sep = "")

fit_nonfrail <- coxme(S_mort_d[p$sub_frailty == 0] ~ studyfeed + (1|Institute),
                      data = p.nonfrail)
paste("HR: ", round(exp(fit_nonfrail$coefficients), 2), ", 95% CI: ",
      round(exp(confint(fit_nonfrail))[1], 2), " - ",
      round(exp(confint(fit_nonfrail))[2], 2), sep = "")

rm(list=setdiff(ls(), c("p", "S_mort_d", "S_mort_m", "extract_coxme_table")))

#### Subgroup: multimorbidity vs no multimorbidity ----
fit_CCI <- coxme(S_mort_d ~ studyfeed*sub_comorbidity + (1|Institute), data = p)
extract_coxme_table(fit_CCI)

p.highCCI <- p[p$sub_comorbidity == 1,]
p.lowCCI  <- p[p$sub_comorbidity == 0,]

# Define fits per subset
fit_highCCI <- coxme(S_mort_d[p$sub_comorbidity == 1] ~ studyfeed + (1|Institute),
                     data = p.highCCI)
paste("HR: ", round(exp(fit_highCCI$coefficients), 2), ", 95% CI: ",
      round(exp(confint(fit_highCCI))[1], 2), " - ",
      round(exp(confint(fit_highCCI))[2], 2), sep = "")

fit_lowCCI <- coxme(S_mort_d[p$sub_comorbidity == 0] ~ studyfeed + (1|Institute),
                    data = p.lowCCI)
paste("HR: ", round(exp(fit_lowCCI$coefficients), 2), ", 95% CI: ",
      round(exp(confint(fit_lowCCI))[1], 2), " - ",
      round(exp(confint(fit_lowCCI))[2], 2), sep = "")

rm(list=setdiff(ls(), c("p", "S_mort_d", "S_mort_m", "extract_coxme_table")))

#### Subgroup: sepsis vs no sepsis ----
fit_sepsis <- coxme(S_mort_d ~ studyfeed*sub_sepsis + (1|Institute), data = p)
extract_coxme_table(fit_sepsis)

p.sepsis   <- p[p$sub_sepsis == 1,]
p.nosepsis <- p[p$sub_sepsis == 0,]

# Define fits per subset
fit_sepsisY <- coxme(S_mort_d[p$sub_sepsis == 1] ~ studyfeed + (1|Institute),
                     data = p.sepsis)
paste("HR: ", round(exp(fit_sepsisY$coefficients), 2), ", 95% CI: ",
      round(exp(confint(fit_sepsisY))[1], 2), " - ",
      round(exp(confint(fit_sepsisY))[2], 2), sep = "")

fit_sepsisN <- coxme(S_mort_d[p$sub_sepsis == 0] ~ studyfeed + (1|Institute),
                     data = p.nosepsis)
paste("HR: ", round(exp(fit_sepsisN$coefficients), 2), ", 95% CI: ",
      round(exp(confint(fit_sepsisN))[1], 2), " - ",
      round(exp(confint(fit_sepsisN))[2], 2), sep = "")

rm(list=setdiff(ls(), c("p", "S_mort_d", "S_mort_m", "extract_coxme_table")))

#### Subgroup: high vs low APACHE II score ----
fit_APACHE <- coxme(S_mort_d ~ studyfeed*sub_disseverity + (1|Institute), data = p)
extract_coxme_table(fit_APACHE)

p.highAPACHE <- p[p$sub_disseverity == 1,]
p.lowAPACHE  <- p[p$sub_disseverity == 0,]

# Define fits per subset
fit_highAPACHE <- coxme(S_mort_d[p$sub_disseverity == 1] ~ studyfeed + (1|Institute),
                        data = p.highAPACHE)
paste("HR: ", round(exp(fit_highAPACHE$coefficients), 2), ", 95% CI: ",
      round(exp(confint(fit_highAPACHE))[1], 2), " - ",
      round(exp(confint(fit_highAPACHE))[2], 2), sep = "")

fit_lowAPACHE <- coxme(S_mort_d[p$sub_disseverity == 0] ~ studyfeed + (1|Institute),
                       data = p.lowAPACHE)
paste("HR: ", round(exp(fit_lowAPACHE$coefficients), 2), ", 95% CI: ",
      round(exp(confint(fit_lowAPACHE))[1], 2), " - ",
      round(exp(confint(fit_lowAPACHE))[2], 2), sep = "")

rm(list=setdiff(ls(), c("p", "S_mort_d", "S_mort_m", "extract_coxme_table")))

#### Subgroup: AKI vs non-AKI ----
fit_AKI <- coxme(S_mort_d ~ studyfeed*sub_AKI + (1|Institute), data = p)
extract_coxme_table(fit_AKI)

p.AKI    <- p[p$sub_AKI == 1,]
p.nonAKI <- p[p$sub_AKI == 0,]

# Define fits per subset
fit_AKIY <- coxme(S_mort_d[p$sub_AKI == 1] ~ studyfeed + (1|Institute),
                  data = p.AKI)
paste("HR: ", round(exp(fit_AKIY$coefficients), 2), ", 95% CI: ",
      round(exp(confint(fit_AKIY))[1], 2), " - ",
      round(exp(confint(fit_AKIY))[2], 2), sep = "")

fit_AKYN <- coxme(S_mort_d[p$sub_AKI == 0] ~ studyfeed + (1|Institute),
                  data = p.nonAKI)
paste("HR: ", round(exp(fit_AKYN$coefficients), 2), ", 95% CI: ",
      round(exp(confint(fit_AKYN))[1], 2), " - ",
      round(exp(confint(fit_AKYN))[2], 2), sep = "")

#### Subgroup: high vs low SOFA ----
fit_SOFA <- coxme(S_mort_d ~ studyfeed*sub_organfailure + (1|Institute), data = p)
extract_coxme_table(fit_SOFA)

p.highSOFA <- p[p$sub_organfailure == 1,]
p.lowSOFA <- p[p$sub_organfailure == 0,]

# Define fits per subset
fit_highSOFA <- coxme(S_mort_d[p$sub_organfailure == 1] ~ studyfeed + (1|Institute),
                      data = p.highSOFA)
paste("HR: ", round(exp(fit_highSOFA$coefficients), 2), ", 95% CI: ",
      round(exp(confint(fit_highSOFA))[1], 2), " - ",
      round(exp(confint(fit_highSOFA))[2], 2), sep = "")

fit_lowSOFA <- coxme(S_mort_d[p$sub_organfailure == 0] ~ studyfeed + (1|Institute),
                     data = p.lowSOFA)
paste("HR: ", round(exp(fit_lowSOFA$coefficients), 2), ", 95% CI: ",
      round(exp(confint(fit_lowSOFA))[1], 2), " - ",
      round(exp(confint(fit_lowSOFA))[2], 2), sep = "")

#### Subgroup: TBI vs non-TBI ----
fit_TBI <- coxme(S_mort_d ~ studyfeed*sub_TBI + (1|Institute), data = p)
extract_coxme_table(fit_TBI)

p.TBI <- p[p$sub_TBI == 1,]
p.nonTBI <- p[p$sub_TBI == 0,]

# Define fits per subset
fit_TBIY <- coxme(S_mort_d[p$sub_TBI == 1] ~ studyfeed + (1|Institute),
                  data = p.TBI)
paste("HR: ", round(exp(fit_TBIY$coefficients), 2), ", 95% CI: ",
      round(exp(confint(fit_TBIY))[1], 2), " - ",
      round(exp(confint(fit_TBIY))[2], 2), sep = "")

fit_TBIN <- coxme(S_mort_d[p$sub_TBI == 0] ~ studyfeed + (1|Institute),
                  data = p.nonTBI)
paste("HR: ", round(exp(fit_TBIN$coefficients), 2), ", 95% CI: ",
      round(exp(confint(fit_TBIN))[1], 2), " - ",
      round(exp(confint(fit_TBIN))[2], 2), sep = "")

#### Subgroup: COVID-19 vs non-COVID-19 ----
fit_COVID <- coxme(S_mort_d ~ studyfeed*sub_COVID + (1|Institute), data = p)
extract_coxme_table(fit_COVID)

p.COVID <- p[p$sub_COVID == 1,]
p.nonCOVID <- p[p$sub_COVID == 0,]

# Define fits per subset
fit_COVIDY <- coxme(S_mort_d[p$sub_COVID == 1] ~ studyfeed + (1|Institute),
                    data = p.COVID)
paste("HR: ", round(exp(fit_COVIDY$coefficients), 2), ", 95% CI: ",
      round(exp(confint(fit_COVIDY))[1], 2), " - ",
      round(exp(confint(fit_COVIDY))[2], 2), sep = "")

fit_COVIDN <- coxme(S_mort_d[p$sub_COVID == 0] ~ studyfeed + (1|Institute),
                    data = p.nonCOVID)
paste("HR: ", round(exp(fit_COVIDN$coefficients), 2), ", 95% CI: ",
      round(exp(confint(fit_COVIDN))[1], 2), " - ",
      round(exp(confint(fit_COVIDN))[2], 2), sep = "")

### End of file.
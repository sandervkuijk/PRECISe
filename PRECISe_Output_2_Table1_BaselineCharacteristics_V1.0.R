###############################################################################|
### PRECISe Table 1 - Baseline table
### Julia Bels, October 2023
### R version 4.3.1
###
### This script is meant for the synthesis of Table 1. Baseline Characteristics.
###
###############################################################################|

# 1. Import & general adjustments ----
library(gtsummary)
library(finalfit)

# 2. Prepare variables for import into table ----
p$ADM_TYPEOPT <- ifelse(p$ADM_TYPEOPT == "1", "Medical",
                        ifelse(p$ADM_TYPEOPT == "2", "Emergency surgery", "Elective post-surgery"))

p$DEM_AGE <- as.numeric(p$DEM_AGE)

# 3. Make table ----
TABLE <-
p %>% select(studyfeed,DEM_AGE, DEM_SEX,BMI,ADM_TYPEOPT,ADM_SYSTEMDIAGNOSIS,MH_CHCOPT.Diabetes.mellitus,ADM_COVIDYN,ADM_RFYN,
             ADM_SEPSYN,GCS,APACHEscore,APACHEIVscore,SOFA_score,SAPSscore,EQ5D.HUS.proxy.0,CCI,NRSscore,FRAIL_D0) %>% 
  tbl_summary(
    by = studyfeed, #change to variable assigned to intervention
    label = list(DEM_AGE ~ "Age (years)", DEM_SEX ~ "Sex, female", BMI ~ "BMI", 
                 ADM_TYPEOPT  ~ "Type of admission",ADM_SYSTEMDIAGNOSIS ~ "Admission system diagnosis",
                 MH_CHCOPT.Diabetes.mellitus ~ "Diabetes Mellitus", ADM_COVIDYN ~ "COVID-19", ADM_RFYN ~ "Acute Kidney Injury",
                 ADM_SEPSYN ~ "Sepsis", GCS ~ "Glasgow Coma Scale",APACHEscore ~ "APACHE II",APACHEIVscore ~"APACHE IV", 
                 SOFA_score ~ "SOFA score", SAPSscore ~ "SAPS II score", EQ5D.HUS.proxy.0 ~"EQ-5D-5L Health Utility Score", 
                 CCI ~"Charlson Comorbidity Index", NRSscore ~ "NRS-2002 score", FRAIL_D0 ~ "Rockwood Clinical Frailty Score"),
    digits = c(DEM_AGE, SOFA_score, APACHEscore, GCS, BMI, NRSscore, CCI) ~ 0,
    sort= c(ADM_TYPEOPT, ADM_SYSTEMDIAGNOSIS) ~ "frequency",
    missing = "no",
    type = list(DEM_AGE ~ "continuous", BMI ~ "continuous", DEM_SEX ~ "dichotomous", 
                ADM_TYPEOPT ~ "categorical", ADM_SYSTEMDIAGNOSIS ~ "categorical", MH_CHCOPT.Diabetes.mellitus ~ "dichotomous",
                ADM_COVIDYN ~ "dichotomous", ADM_RFYN ~ "dichotomous", ADM_SEPSYN ~ "dichotomous",GCS ~ "continuous", 
                APACHEscore ~ "continuous", SOFA_score ~ "continuous", SAPSscore ~ "continuous", 
                EQ5D.HUS.proxy.0 ~ "continuous",CCI ~ "continuous", NRSscore ~ "continuous", FRAIL_D0 ~"continuous"),
    statistic = c(DEM_AGE,FRAIL_D0,NRSscore, EQ5D.HUS.proxy.0, SOFA_score, APACHEscore, BMI, SAPSscore, APACHEIVscore) ~ "{mean} ± {sd}",
    value = list(DEM_SEX = "2", MH_CHCOPT.Diabetes.mellitus = "1", ADM_COVIDYN = "1", ADM_RFYN = "1", ADM_SEPSYN = "1"),
    include = everything(),) %>% bold_labels() %>% modify_header(label ~ "**Variable**") %>% modify_caption("**Table 1. Characteristics of the Patients at Baseline**")

TABLE %>% as_flex_table() %>% flextable::save_as_docx(path = "BaselineTable.docx")

## End of file. ##
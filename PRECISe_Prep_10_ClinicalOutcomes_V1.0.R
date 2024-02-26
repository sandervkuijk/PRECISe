###############################################################################|
### PRECISe - Clinical outcomes
### Julia Bels, January 2024
### R version 4.3.1
###
### This script is meant for calculation of clinical outcomes.
###
###############################################################################|

# 1. Clinical outcomes ----

## 1.1 ICU mortality   ----

#Check whether death date occurred on ICU discharge date, if 0 -> died in ICU
#This was left open in Castor in case of death in ICU, therefore recoding is necessary.
p$TERM_DEATHDT <- as.Date(p$TERM_DEATHDT, format = "%d-%m-%Y")
p$days <- as.numeric(difftime(p$TERM_DEATHDT, p$ICU_dischargedate, units = "days"))
p$ICUmortality <- ifelse(p$days == 0, 1, 0) #283 patients died in ICU

#Assign 0 if patient completed trial (alive)
p$ICUmortality <- ifelse(is.na(p$TERM_REASOPT), 0, p$ICUmortality)

#Assign 0 if early discontinuation date lies after ICU discharge date (earlytermdays is 0 or higher)
p$earlytermdaysICU <- as.numeric(difftime(p$TERMDATEwithoutdeath, p$ICU_dischargedate, units = "days"))
p$ICUmortality <- ifelse(!is.na(p$earlytermdaysICU), 0, p$ICUmortality) #27 NAs -> consent withdrawal before ICU discharge

#Make these NAs into variable for future data quality checks
p$termbeforeICUdis <- ifelse(is.na(p$ICUmortality), 1, 0)

## 1.2 Hospital mortality   ----

# Defined as death during index hospitalization period.
p$hospitalmortality <- ifelse(p$DIS_SURVOPT == 2 | p$DIS_HOSPSURVOPT == 2, "Dead", "Alive")

# Three patients have "DIS_HOSPSURVOPT" coded as NA (due to transfer to other ICU), 
# but actually died during index hospitalization. Code as "Dead".
p$hospitalmortality <- ifelse(p$Participant.Id == "01-PRECISe-095" | 
                              p$Participant.Id == "01-PRECISe-119" | 
                              p$Participant.Id == "05-PRECISe-025", "Dead", p$hospitalmortality)
 
# Two patients had an ongoing hospitalization at trial termination 
# 02-155 -> hosp admission ongoing at trial term -> remains missing
# 09-019 -> ICU readmission ongoing at trial term -> remains missing

# The remaining NAs (N = 41), are true NAs due to consent withdrawal (proxy = 31,
# patient = 9) or LTFU (N = 1). Code these as such for future checks.
p$termbeforehospdis <- ifelse(is.na(p$hospitalmortality) & p$TERM_REASOPT == 1 |
                              is.na(p$hospitalmortality) & p$TERM_REASOPT == 4 |
                              is.na(p$hospitalmortality) & p$TERM_REASOPT == 7, 1, 0)

## 1.3 30-day, 60-day and 90-day mortality   ----
#Define days until death (ICU admission -> death date)
p$daysuntildeath <- as.numeric(difftime(as.Date(p$TERM_DEATHDT, format = "%d-%m-%Y"), p$ICU_admissiondate, units = "days")) + 1

#Define days until trial termination (ICU admission -> trial termination), assign 999 to missings to avoid issues with ifelse
p$daysuntiltrialterm <- as.numeric(difftime(p$TERMDATEwithoutdeath, p$ICU_admissiondate, units = "days")) + 1
p$daysuntiltrialterm <- ifelse(is.na(p$daysuntiltrialterm), 999, p$daysuntiltrialterm)

table(p$daysuntiltrialterm)

#All non-survivors that died on or before D30/60/90 = 1, all other non-survivors = 0
p$mortality.D30 <- ifelse(p$daysuntildeath <= 30, 1, 0)
p$mortality.D60 <- ifelse(p$daysuntildeath <= 60, 1, 0)
p$mortality.D90 <- ifelse(p$daysuntildeath <= 90, 1, 0)

#All survivors that were observed until D30/60/90 = 0, all other NA
p$mortality.D30 <- ifelse(is.na(p$mortality.D30) & p$daysuntiltrialterm > 30, 0,
                          ifelse(is.na(p$mortality.D30) & p$daysuntiltrialterm <= 30, NA, p$mortality.D30)) #59 consent withdrawals before D30
p$mortality.D60 <- ifelse(is.na(p$mortality.D60) & p$daysuntiltrialterm > 60, 0,
                          ifelse(is.na(p$mortality.D60) & p$daysuntiltrialterm <= 60, NA, p$mortality.D60)) #75 consent withdrawals before D30
p$mortality.D90 <- ifelse(is.na(p$mortality.D90) & p$daysuntiltrialterm > 90, 0,
                          ifelse(is.na(p$mortality.D90) & p$daysuntiltrialterm <= 90, NA, p$mortality.D90))#88 consent withdrawals before D30

## 1.4 Time-to-discharge (TTD) alive from hospital ----

# Number of days until live hospital discharge. Data of non-survivors will be 
# censored at a time point beyond that of the last surviving patient to account 
# for death as a competing risk.

# Variable hosp_dischargedate includes deathdate as discharge date. 
# Variable DIS_HOSPDT does not.
p$TTD_alive <- as.numeric(difftime(as.Date(p$DIS_HOSPDT, format = "%d-%m-%Y"), p$ICU_admissiondate, units = "days")) + 1

table(p$TTD_alive, p$studyfeed, useNA = "always") #379 NAs, of whom 45 are survivors (2 more than hospital mortality, where status was known but date was not)

## 1.5 DAAH90 ----
# Data not complete yet.

## 1.6 Duration of mechanical ventilation ----
#Calculate ventilator days of each ventilation episode
p$IMVduration.ep1 <- as.numeric(difftime(as.Date(p$VENT_ENDT, format = "%d-%m-%Y"), 
                                         as.Date(p$VENT_STDT, format = "%d-%m-%Y"), units = "days")) + 1
p$IMVduration.ep2 <- as.numeric(difftime(as.Date(p$VENT_Ventilation.episode.2_End.date, format = "%d-%m-%Y"), 
                                         as.Date(p$VENT_Ventilation.episode.2_Start.date, format = "%d-%m-%Y"), units = "days")) + 1
p$IMVduration.ep3 <- as.numeric(difftime(as.Date(p$VENT_Ventilation.episode.3_End.date, format = "%d-%m-%Y"), 
                                         as.Date(p$VENT_Ventilation.episode.3_Start.date, format = "%d-%m-%Y"), units = "days")) + 1
p$IMVduration.ep4 <- as.numeric(difftime(as.Date(p$VENT_Ventilation.episode.4_End.date, format = "%d-%m-%Y"), 
                                         as.Date(p$VENT_Ventilation.episode.4_Start.date, format = "%d-%m-%Y"), units = "days")) + 1
p$IMVduration.ep5 <- as.numeric(difftime(as.Date(p$VENT_Ventilation.episode.5_End.date, format = "%d-%m-%Y"), 
                                         as.Date(p$VENT_Ventilation.episode.5_Start.date, format = "%d-%m-%Y"), units = "days")) + 1

#Calculate total number of ventilator days (of all ventilation episodes)
p$IMVdurationtotal <- rowSums(data.frame(p$IMVduration.ep1, p$IMVduration.ep2,
                                         p$IMVduration.ep3, p$IMVduration.ep4,
                                         p$IMVduration.ep5),na.rm=TRUE) #gives 0 for all NAs; cannot be 0 otherwise

p$IMVdurationtotal <- ifelse(p$IMVdurationtotal %in% 0, NA, p$IMVdurationtotal) 

## 1.7	Duration of index ICU stay ----
p$ICU_LoS <- as.numeric(difftime(p$ICU_dischargedate, p$ICU_admissiondate, units = "days")) + 1

## 1.8	ICU readmissions ----
# SAP: Number of patients readmitted to  ICU during index hospital stay 
# and number of readmissions per patient.

### 1.8.1 Incidence of ICU readmissions ----

#Code readmissions as 0 (now "NA") if patient died during index ICU admission
p$READ_YN <- ifelse(is.na(p$READ_YN) & p$ICUmortality == 1, 0, p$READ_YN)

### 1.8.2 Number of readmission per patient ----

# 01-035, 02-069, 02-187, 07-020, 09-019, 09-030 have 2 admissions; 01-141 has (see object re in first script)
p$READ_count <- ifelse(p$READ_YN %in% 1 & p$Participant.Id == "01-PRECISe-035" | 
                       p$READ_YN %in% 1 & p$Participant.Id == "02-PRECISe-069" | 
                       p$READ_YN %in% 1 & p$Participant.Id == "02-PRECISe-187" |
                       p$READ_YN %in% 1 & p$Participant.Id == "07-PRECISe-020" | 
                       p$READ_YN %in% 1 & p$Participant.Id == "09-PRECISe-019" | 
                       p$READ_YN %in% 1 & p$Participant.Id == "09-PRECISe-030", 2,
                ifelse(p$READ_YN %in% 1 & p$Participant.Id == "01-PRECISe-141", 3,
                ifelse(p$READ_YN %in% 1, 1, NA)))

## 1.9	Administration of prokinetics ----

## 1.10	Incidence of gastrointestinal intolerance/symptoms ----
p$GI_symptoms <- coalesce(p$GI_OPT.Abdominal.distension, p$GI_OPT.Bleeding.ulcer, 
                          p$GI_OPT.Diarrhoea, p$GI_OPT.Gastric.paresis, p$GI_OPT.Ischaemia,
                          p$GI_OPT.Vomiting)

## 1.11 Incidence of ICU-acquired infections ----
table(p$EI_OPT.ICU.acquired.infection, useNA = "always")

## 1.12	Incidence of acute kidney injury ----
names(p)[names(p) == "EI_OPT.Acute.Kidney.Injury..defined.as.creatinine.level.higher.than.2.times.baseline.level..including.the.start.of.Renal.Replacement.Therapy."] <- "newAKI"

table(p$newAKI, useNA = "always") #no missings ... this is not correct. Make new variable summing all options, it should at least be 1.

#Make sure missing is incorporated
p$eventsvar <- rowSums(data.frame(p$EI_OPT.Hepatic.dysfunction..cholestasis.and.liver.dysfunction..bilirubin.level.higher.than.3.mg.dl.or.51.3.µmol.l.,
                                  p$EI_OPT.ICU.acquired.infection,
                                  p$EI_OPT.Refeeding.hypophosphatemia..phosphate.levels.below..0.65.mmol.l..a.drop..0.16.mmol.L.from.previous.level.in.ICU.and.no.other.explanation.for.hypophosphatemia.,
                                  p$EI_OPT.Ventilator.acquired.pneumonia..VAP.,
                                  p$newAKI,
                                  p$EI_OPT.None.of.the.above),na.rm=TRUE) #all NAs gives 0

table(p$eventsvar, useNA = "always") #one 0 --> 02-PRECISe-197 = correct; missing on all these variables

#Recode options to NA
p[p$Participant.Id == "02-PRECISe-197", "EI_OPT.Hepatic.dysfunction..cholestasis.and.liver.dysfunction..bilirubin.level.higher.than.3.mg.dl.or.51.3.µmol.l."] <- NA
p[p$Participant.Id == "02-PRECISe-197", "EI_OPT.ICU.acquired.infection"] <- NA
p[p$Participant.Id == "02-PRECISe-197", "EI_OPT.Refeeding.hypophosphatemia..phosphate.levels.below..0.65.mmol.l..a.drop..0.16.mmol.L.from.previous.level.in.ICU.and.no.other.explanation.for.hypophosphatemia."] <- NA
p[p$Participant.Id == "02-PRECISe-197", "EI_OPT.Ventilator.acquired.pneumonia..VAP."] <- NA
p[p$Participant.Id == "02-PRECISe-197", "newAKI"] <- NA
p[p$Participant.Id == "02-PRECISe-197", "EI_OPT.None.of.the.above"] <- NA

## 1.13 Incidence and duration of renal replacement therapy ----

#Incidence
table(p$RR_YN, p$studyfeed, useNA = "always")

#Duration
p$RRTduration.ep1 <- as.numeric(difftime(as.Date(p$RRTHER_Renal.replacement.therapy.episode.1_End.date, format = "%d-%m-%Y"), 
                                         as.Date(p$RRTHER_Renal.replacement.therapy.episode.1_Start.date, format = "%d-%m-%Y"), units = "days")) + 1
p$RRTduration.ep2 <- as.numeric(difftime(as.Date(p$RRTHER_Renal.replacement.therapy.episode.2_End.date, format = "%d-%m-%Y"), 
                                         as.Date(p$RRTHER_Renal.replacement.therapy.episode.2_Start.date, format = "%d-%m-%Y"), units = "days")) + 1
p$RRTduration.ep3 <- as.numeric(difftime(as.Date(p$RRTHER_Renal.replacement.therapy.episode.3_End.date, format = "%d-%m-%Y"), 
                                         as.Date(p$RRTHER_Renal.replacement.therapy.episode.3_Start.date, format = "%d-%m-%Y"), units = "days")) + 1
p$RRTduration.ep4 <- as.numeric(difftime(as.Date(p$RRTHER_Renal.replacement.therapy.episode.4_End.date, format = "%d-%m-%Y"), 
                                         as.Date(p$RRTHER_Renal.replacement.therapy.episode.4_Start.date, format = "%d-%m-%Y"), units = "days")) + 1
p$RRTduration.ep5 <- as.numeric(difftime(as.Date(p$RRTHER_Renal.replacement.therapy.episode.5_End.date, format = "%d-%m-%Y"), 
                                         as.Date(p$RRTHER_Renal.replacement.therapy.episode.5_Start.date, format = "%d-%m-%Y"), units = "days")) + 1

#Calculate total number of ventilator days (of all ventilation episodes)
p$RRTdurationtotal <- rowSums(data.frame(p$RRTduration.ep1, p$RRTduration.ep2,
                                         p$RRTduration.ep3, p$RRTduration.ep4,
                                         p$RRTduration.ep5),na.rm=TRUE) #gives 0 for all NAs 

p$RRTdurationtotal <- ifelse(p$RRTdurationtotal %in% 0, NA, p$RRTdurationtotal) 

## 1.14 Incidence of hepatic dysfunction ----
#Simplify name
names(p)[names(p) == "EI_OPT.Hepatic.dysfunction..cholestasis.and.liver.dysfunction..bilirubin.level.higher.than.3.mg.dl.or.51.3.µmol.l."] <- "hepaticdysfunction"

## 1.15 Sequential Organ Failure Assessment (SOFA) score ----
# Calculate mean and maximum SOFA score during first two weeks of index ICU admission

# Select rows where SOFA is known 
tt <- t[!is.na(t$SOFA_score_trt),]

sofa <- tt %>% 
  select(Participant.Id, SOFA_score_trt) %>%
  group_by(Participant.Id) %>%
  summarise(
    SOFA.mean = mean(SOFA_score_trt, na.rm = TRUE),
    SOFA.max = max(SOFA_score_trt, na.rm = TRUE),) %>%
  as.data.frame()

# Add to main study object
p <- merge(p, sofa, by=c("Participant.Id"), all = TRUE)

## 1.16	Duration of index hospital stay ----
#Includes LoS of non-survivors; add one day to include last day itself
p$hosp_LoS <- as.numeric(difftime(p$hosp_dischargedate, p$hosp_admissiondate, units = "days")) + 1
table(p$hosp_LoS, useNA = "always")

## End of file. ##
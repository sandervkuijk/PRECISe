###############################################################################|
### PRECISe concomitant medication 
### Julia Bels, May 2023
### R version 4.2.2
###
### This prepares the concomitant medication records. No clear goal has been
### described yet in terms of incorporation of these outcomes into the manuscript.
###
### There are no duplicates.
###
###############################################################################|

# 1. Concomitant medication |   med  ----

## 1.1 Import & general adjustments ----

#Import data
med <- read.csv("~/PRECISe/PRECISe_FINAL_ANALYSES/data/raw/PRECISe_study_Concomitant_medication_export_20240115.csv", sep=";", 
              na.strings = list("", -99, -98, -97, -96, -95, "01-01-2995", 
                                "01-01-2996", "01-01-2997", "01-01-2998", 
                                "01-01-2999", "##USER_MISSING_95##",
                                "##USER_MISSING_96##", "##USER_MISSING_97##",
                                "##USER_MISSING_98##", "##USER_MISSING_99##")) 

#Delete records of Amphotericin B (is anti-fungal; should not have been collected; option "5")
med <- med[med$CM_NAMEOPT != '5',]

## 1.2 Check which records contain administration of prokinetics ----
# The following options of variable "CM_NAMEOPT" include prokinetics:
# Alizapride = 1
# Domperidone = 34
# Erythromycin = 38
# Metoclopramide = 56

med$prokinetic <- ifelse(med$CM_NAMEOPT == 1 | med$CM_NAMEOPT == 34 | 
                         med$CM_NAMEOPT == 38 | med$CM_NAMEOPT == 56, 1, 0)

## 1.3 Slim down to only reports on prokinetics & delete duplicates ----
#Select records where prokinetic was given
med <- med[med$prokinetic == 1,]

#Delete duplicates
medd <- med[!duplicated(med$Participant.Id),]

## 1.4 Merge with main study object p & recode NAs ----
medd <- medd[,c("Participant.Id", "prokinetic")]
p <- merge(p, medd, by=c("Participant.Id"), all = TRUE)

p$prokinetic <- ifelse(is.na(p$prokinetic) & p$Participant.Id != "02-PRECISe-197", 0, p$prokinetic)

# 2. Calculate number of prokinetic days ----

## 2.1 Import initial ICU discharge date from main study object ----
pp <- p[,c("Participant.Id", "DIS_ICUDT")]
medd <- merge(med, pp, by=c("Participant.Id"), all = FALSE)

## 2.2 Define correct dates ----

#Make final enddate for concomitant medication (is left open if patient was discharged from ICU)
medd$CM_ENDT <- coalesce(medd$CM_ENDT, medd$DIS_ICUDT) #2 remain missing

#Impute missing enddate for 06-PRECISe-023. The other one (03-001) is a true missing. Checked with source.
medd$CM_ENDT <- ifelse(medd$Participant.Id == "06-PRECISe-023", "18-05-2023", medd$CM_ENDT)

## 2.3 Calculate prokinetic days ----

#Calculate days per time a prokinetic was given (i.e., per row)
medd$prokineticdays <- as.numeric(difftime(as.Date(medd$CM_ENDT, format = "%d-%m-%Y"), as.Date(medd$CM_STDT, format = "%d-%m-%Y"), units = "days")) + 1

#Make sum of all days on prokinetics per participant
meddays <- aggregate(prokineticdays ~ Participant.Id, medd, sum)

## 2.4 Merge with main study object p ----
p <- merge(p, meddays, by=c("Participant.Id"), all = TRUE)

# Remove surplus objects ----
rm(list=ls()[! ls() %in% c("p","re","questLong","t","o","pn","med", "i")])

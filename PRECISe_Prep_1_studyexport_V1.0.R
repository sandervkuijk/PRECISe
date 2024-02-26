###############################################################################|
### PRECISe Data prepping - study export (main study export) 
### Julia Bels, January 2024
### R version 4.3.2
###
### This script is meant for prepping the data of the main PRECISe study
### export, including (re)calculation of lab values, Castor calculations and 
### all relevant scores (e.g. APACHE II).
###
### This is the first script to be run. There are no duplicates.
###
###############################################################################|

#Load libraries
library("plyr")
library("tidyverse")
library("lubridate")
library("ggplot2")
library("viridis")
library("eq5d")
library("EQ5D.be")
library("naniar") #replacewithna
library("writexl")

# 1. Import & general adjustments   ----

#Import CSV (check date) - Castor: "Export all"> "Display options as Numbers (values)" > "Entire study"
p <- read.csv("~/PRECISe/PRECISe_FINAL_ANALYSES/data/raw/PRECISe_study_export_20240115.csv", sep=";", 
              na.strings = list("", -99, -98, -97, -96, "01-01-2995", 
                                "01-01-2996", "01-01-2997", "01-01-2998", 
                                "01-01-2999", "##USER_MISSING_95##",
                                "##USER_MISSING_96##", "##USER_MISSING_97##",
                                "##USER_MISSING_98##", "##USER_MISSING_99##")) #-95, -99 at later stage

# Make new variable Institute with correct naming of sites
p$Institute <- ifelse(p$Site.Abbreviation == "MUMC", "Site 01 - MUMC",
               ifelse(p$Site.Abbreviation == "ZOL", "Site 02 - ZOL",
               ifelse(p$Site.Abbreviation == "ZGV", "Site 03 - Gelderse Vallei",
               ifelse(p$Site.Abbreviation == "MST", "Site 04 - MS Twente",
               ifelse(p$Site.Abbreviation == "ZMC", "Site 05 - Zuyderland MC",
               ifelse(p$Site.Abbreviation == "CHU", "Site 06 - CHU Liege",
               ifelse(p$Site.Abbreviation == "CHR", "Site 07 - CHR Liege",
               ifelse(p$Site.Abbreviation == "UZB", "Site 08 - UZ Brussel",
               ifelse(p$Site.Abbreviation == "AZG", "Site 09 - AZ Groeninge",
               ifelse(p$Site.Abbreviation == "CZE", "Site 10 - Catharina",
               ifelse(p$Site.Abbreviation == "UMCZ", "Site 01 - MUMC", NA)))))))))))

# Impute scores for missing values in MRC-SUM score

# Manual: When muscle strength cannot be evaluated due to orthopedic, neurological 
# or other reasons, results of the contralateral muscle group will be substituted to 
# calculate the MRC-sum score. The only exception is paraplegia. The values of 
# the arm are then extrapolated to the leg (ipsilateral limb here). When there are 
# more than two extrapolations the MRC sum score cannot be used! The reason of 
# extrapolation must be reported at the time of the measurement.

#Imputation of ipsilateral limb in case of paraplegia
p$MRC_D30_Flexion.leg.hip_Left <- ifelse(p$MRC_PARA_D30 == 1, p$MRC_D30_Abduction.arm.shoulder_Left, p$MRC_D30_Flexion.leg.hip_Left)
p$MRC_D30_Flexion.leg.hip_Right <- ifelse(p$MRC_PARA_D30 == 1, p$MRC_D30_Abduction.arm.shoulder_Right, p$MRC_D30_Flexion.leg.hip_Right)
p$MRC_D30_Extension.knee_Left <- ifelse(p$MRC_PARA_D30 == 1, p$MRC_D30_Flexion.forearm.elbow_Left, p$MRC_D30_Extension.knee_Left)
p$MRC_D30_Extension.knee_Right <- ifelse(p$MRC_PARA_D30 == 1, p$MRC_D30_Flexion.forearm.elbow_Right, p$MRC_D30_Extension.knee_Right)
p$MRC_D30_Dorsal.flexion.foot.ankle_Left <- ifelse(p$MRC_PARA_D30 == 1, p$MRC_D30_Extension.wrist_Left, p$MRC_D30_Dorsal.flexion.foot.ankle_Left)
p$MRC_D30_Dorsal.flexion.foot.ankle_Right <- ifelse(p$MRC_PARA_D30 == 1, p$MRC_D30_Extension.wrist_Right, p$MRC_D30_Dorsal.flexion.foot.ankle_Right)

p$MRC_D90_Flexion.leg.hip_Left <- ifelse(p$MRC_PARA_D90 == 1, p$MRC_D90_Abduction.arm.shoulder_Left, p$MRC_D90_Flexion.leg.hip_Left)
p$MRC_D90_Flexion.leg.hip_Right <- ifelse(p$MRC_PARA_D90 == 1, p$MRC_D90_Abduction.arm.shoulder_Right, p$MRC_D90_Flexion.leg.hip_Right)
p$MRC_D90_Extension.knee_Left <- ifelse(p$MRC_PARA_D90 == 1, p$MRC_D90_Flexion.forearm.elbow_Left, p$MRC_D90_Extension.knee_Left)
p$MRC_D90_Extension.knee_Right <- ifelse(p$MRC_PARA_D90 == 1, p$MRC_D90_Flexion.forearm.elbow_Right, p$MRC_D90_Extension.knee_Right)
p$MRC_D90_Dorsal.flexion.foot.ankle_Left <- ifelse(p$MRC_PARA_D90 == 1, p$MRC_D90_Extension.wrist_Left, p$MRC_D90_Dorsal.flexion.foot.ankle_Left)
p$MRC_D90_Dorsal.flexion.foot.ankle_Right <- ifelse(p$MRC_PARA_D90 == 1, p$MRC_D90_Extension.wrist_Right, p$MRC_D90_Dorsal.flexion.foot.ankle_Right)

p$MRC_D180_Flexion.leg.hip_Left <- ifelse(p$MRC_PARA_D180 == 1, p$MRC_D180_Abduction.arm.shoulder_Left, p$MRC_D180_Flexion.leg.hip_Left)
p$MRC_D180_Flexion.leg.hip_Right <- ifelse(p$MRC_PARA_D180 == 1, p$MRC_D180_Abduction.arm.shoulder_Right, p$MRC_D180_Flexion.leg.hip_Right)
p$MRC_D180_Extension.knee_Left <- ifelse(p$MRC_PARA_D180 == 1, p$MRC_D180_Flexion.forearm.elbow_Left, p$MRC_D180_Extension.knee_Left)
p$MRC_D180_Extension.knee_Right <- ifelse(p$MRC_PARA_D180 == 1, p$MRC_D180_Flexion.forearm.elbow_Right, p$MRC_D180_Extension.knee_Right)
p$MRC_D180_Dorsal.flexion.foot.ankle_Left <- ifelse(p$MRC_PARA_D180 == 1, p$MRC_D180_Extension.wrist_Left, p$MRC_D180_Dorsal.flexion.foot.ankle_Left)
p$MRC_D180_Dorsal.flexion.foot.ankle_Right <- ifelse(p$MRC_PARA_D180 == 1, p$MRC_D180_Extension.wrist_Right, p$MRC_D180_Dorsal.flexion.foot.ankle_Right)

#Imputation of contralateral limb in case of lack of evaluation due to orthopedic, neurologic or other reasons (-95)
p$MRC_D30_Abduction.arm.shoulder_Left <- ifelse(p$MRC_D30_Abduction.arm.shoulder_Left == -95, p$MRC_D30_Abduction.arm.shoulder_Right, p$MRC_D30_Abduction.arm.shoulder_Left)
p$MRC_D30_Abduction.arm.shoulder_Right <- ifelse(p$MRC_D30_Abduction.arm.shoulder_Right == -95, p$MRC_D30_Abduction.arm.shoulder_Left, p$MRC_D30_Abduction.arm.shoulder_Right)
p$MRC_D30_Flexion.forearm.elbow_Left <- ifelse(p$MRC_D30_Flexion.forearm.elbow_Left == -95, p$MRC_D30_Flexion.forearm.elbow_Right, p$MRC_D30_Flexion.forearm.elbow_Left)
p$MRC_D30_Flexion.forearm.elbow_Right <- ifelse(p$MRC_D30_Flexion.forearm.elbow_Right == -95, p$MRC_D30_Flexion.forearm.elbow_Left, p$MRC_D30_Flexion.forearm.elbow_Right)
p$MRC_D30_Extension.wrist_Left <- ifelse(p$MRC_D30_Extension.wrist_Left == -95, p$MRC_D30_Extension.wrist_Right, p$MRC_D30_Extension.wrist_Left)
p$MRC_D30_Extension.wrist_Right <- ifelse(p$MRC_D30_Extension.wrist_Right == -95, p$MRC_D30_Extension.wrist_Left, p$MRC_D30_Extension.wrist_Right)
p$MRC_D30_Flexion.leg.hip_Left <- ifelse(p$MRC_D30_Flexion.leg.hip_Left == -95, p$MRC_D30_Flexion.leg.hip_Right, p$MRC_D30_Flexion.leg.hip_Left)
p$MRC_D30_Flexion.leg.hip_Right <- ifelse(p$MRC_D30_Flexion.leg.hip_Right == -95, p$MRC_D30_Flexion.leg.hip_Left, p$MRC_D30_Flexion.leg.hip_Right)
p$MRC_D30_Extension.knee_Left <- ifelse(p$MRC_D30_Extension.knee_Left == -95, p$MRC_D30_Extension.knee_Right, p$MRC_D30_Extension.knee_Left)
p$MRC_D30_Extension.knee_Right <- ifelse(p$MRC_D30_Extension.knee_Right == -95, p$MRC_D30_Extension.knee_Left, p$MRC_D30_Extension.knee_Right)
p$MRC_D30_Dorsal.flexion.foot.ankle_Left <- ifelse(p$MRC_D30_Dorsal.flexion.foot.ankle_Left == -95, p$MRC_D30_Dorsal.flexion.foot.ankle_Right, p$MRC_D30_Dorsal.flexion.foot.ankle_Left)
p$MRC_D30_Dorsal.flexion.foot.ankle_Right <- ifelse(p$MRC_D30_Dorsal.flexion.foot.ankle_Right == -95, p$MRC_D30_Dorsal.flexion.foot.ankle_Left, p$MRC_D30_Dorsal.flexion.foot.ankle_Right)

p$MRC_D90_Abduction.arm.shoulder_Left <- ifelse(p$MRC_D90_Abduction.arm.shoulder_Left == -95, p$MRC_D90_Abduction.arm.shoulder_Right, p$MRC_D90_Abduction.arm.shoulder_Left)
p$MRC_D90_Abduction.arm.shoulder_Right <- ifelse(p$MRC_D90_Abduction.arm.shoulder_Right == -95, p$MRC_D90_Abduction.arm.shoulder_Left, p$MRC_D90_Abduction.arm.shoulder_Right)
p$MRC_D90_Flexion.forearm.elbow_Left <- ifelse(p$MRC_D90_Flexion.forearm.elbow_Left == -95, p$MRC_D90_Flexion.forearm.elbow_Right, p$MRC_D90_Flexion.forearm.elbow_Left)
p$MRC_D90_Flexion.forearm.elbow_Right <- ifelse(p$MRC_D90_Flexion.forearm.elbow_Right == -95, p$MRC_D90_Flexion.forearm.elbow_Left, p$MRC_D90_Flexion.forearm.elbow_Right)
p$MRC_D90_Extension.wrist_Left <- ifelse(p$MRC_D90_Extension.wrist_Left == -95, p$MRC_D90_Extension.wrist_Right, p$MRC_D90_Extension.wrist_Left)
p$MRC_D90_Extension.wrist_Right <- ifelse(p$MRC_D90_Extension.wrist_Right == -95, p$MRC_D90_Extension.wrist_Left, p$MRC_D90_Extension.wrist_Right)
p$MRC_D90_Flexion.leg.hip_Left <- ifelse(p$MRC_D90_Flexion.leg.hip_Left == -95, p$MRC_D90_Flexion.leg.hip_Right, p$MRC_D90_Flexion.leg.hip_Left)
p$MRC_D90_Flexion.leg.hip_Right <- ifelse(p$MRC_D90_Flexion.leg.hip_Right == -95, p$MRC_D90_Flexion.leg.hip_Left, p$MRC_D90_Flexion.leg.hip_Right)
p$MRC_D90_Extension.knee_Left <- ifelse(p$MRC_D90_Extension.knee_Left == -95, p$MRC_D90_Extension.knee_Right, p$MRC_D90_Extension.knee_Left)
p$MRC_D90_Extension.knee_Right <- ifelse(p$MRC_D90_Extension.knee_Right == -95, p$MRC_D90_Extension.knee_Left, p$MRC_D90_Extension.knee_Right)
p$MRC_D90_Dorsal.flexion.foot.ankle_Left <- ifelse(p$MRC_D90_Dorsal.flexion.foot.ankle_Left == -95, p$MRC_D90_Dorsal.flexion.foot.ankle_Right, p$MRC_D90_Dorsal.flexion.foot.ankle_Left)
p$MRC_D90_Dorsal.flexion.foot.ankle_Right <- ifelse(p$MRC_D90_Dorsal.flexion.foot.ankle_Right == -95, p$MRC_D90_Dorsal.flexion.foot.ankle_Left, p$MRC_D90_Dorsal.flexion.foot.ankle_Right)

p$MRC_D180_Abduction.arm.shoulder_Left <- ifelse(p$MRC_D180_Abduction.arm.shoulder_Left == -95, p$MRC_D180_Abduction.arm.shoulder_Right, p$MRC_D180_Abduction.arm.shoulder_Left)
p$MRC_D180_Abduction.arm.shoulder_Right <- ifelse(p$MRC_D180_Abduction.arm.shoulder_Right == -95, p$MRC_D180_Abduction.arm.shoulder_Left, p$MRC_D180_Abduction.arm.shoulder_Right)
p$MRC_D180_Flexion.forearm.elbow_Left <- ifelse(p$MRC_D180_Flexion.forearm.elbow_Left == -95, p$MRC_D180_Flexion.forearm.elbow_Right, p$MRC_D180_Flexion.forearm.elbow_Left)
p$MRC_D180_Flexion.forearm.elbow_Right <- ifelse(p$MRC_D180_Flexion.forearm.elbow_Right == -95, p$MRC_D180_Flexion.forearm.elbow_Left, p$MRC_D180_Flexion.forearm.elbow_Right)
p$MRC_D180_Extension.wrist_Left <- ifelse(p$MRC_D180_Extension.wrist_Left == -95, p$MRC_D180_Extension.wrist_Right, p$MRC_D180_Extension.wrist_Left)
p$MRC_D180_Extension.wrist_Right <- ifelse(p$MRC_D180_Extension.wrist_Right == -95, p$MRC_D180_Extension.wrist_Left, p$MRC_D180_Extension.wrist_Right)
p$MRC_D180_Flexion.leg.hip_Left <- ifelse(p$MRC_D180_Flexion.leg.hip_Left == -95, p$MRC_D180_Flexion.leg.hip_Right, p$MRC_D180_Flexion.leg.hip_Left)
p$MRC_D180_Flexion.leg.hip_Right <- ifelse(p$MRC_D180_Flexion.leg.hip_Right == -95, p$MRC_D180_Flexion.leg.hip_Left, p$MRC_D180_Flexion.leg.hip_Right)
p$MRC_D180_Extension.knee_Left <- ifelse(p$MRC_D180_Extension.knee_Left == -95, p$MRC_D180_Extension.knee_Right, p$MRC_D180_Extension.knee_Left)
p$MRC_D180_Extension.knee_Right <- ifelse(p$MRC_D180_Extension.knee_Right == -95, p$MRC_D180_Extension.knee_Left, p$MRC_D180_Extension.knee_Right)
p$MRC_D180_Dorsal.flexion.foot.ankle_Left <- ifelse(p$MRC_D180_Dorsal.flexion.foot.ankle_Left == -95, p$MRC_D180_Dorsal.flexion.foot.ankle_Right, p$MRC_D180_Dorsal.flexion.foot.ankle_Left)
p$MRC_D180_Dorsal.flexion.foot.ankle_Right <- ifelse(p$MRC_D180_Dorsal.flexion.foot.ankle_Right == -95, p$MRC_D180_Dorsal.flexion.foot.ankle_Left, p$MRC_D180_Dorsal.flexion.foot.ankle_Right)

#Assign all other -95 as NA --> these will be imputed via multiple imputation? (important: only IF p$MRC_YN_D30/90/180 = 1)
p[p == -95] <- NA

# 2. Recalculate lab values (baseline)   ----

#Convert lab values to standard units: Hb, Ht, Urea, Creat, Lact, Bili, gluc
p$Haemoglobin      <- ifelse(p$Institute == 'Site 01 - MUMC'| p$Institute == "Site 05 - Zuyderland MC" |
                             p$Institute == "Site 03 - Gelderse Vallei"| p$Institute == "Site 04 - MS Twente" |
                             p$Institute == "Site 10 - Catharina", p$LB_HB/0.6206, p$LB_HB) #in g/dl
p$Hematocrit       <- ifelse(p$Institute == 'Site 01 - MUMC' | p$Institute == "Site 05 - Zuyderland MC" |
                             p$Institute == "Site 03 - Gelderse Vallei"| p$Institute == "Site 04 - MS Twente" |
                             p$Institute == "Site 10 - Catharina", p$LB_HCT*100, p$LB_HCT) #in %
p$Urea_mmoll       <- ifelse(p$Institute == 'Site 02 - ZOL' | p$Institute == "Site 06 - CHU Liege" |
                             p$Institute == "Site 07 - CHR Liege"| p$Institute == "Site 08 - UZ Brussel" |
                             p$Institute == "Site 09 - AZ Groeninge", p$LB_UREA*0.1665, p$LB_UREA) #in mmol/l
p$Urea_mgdl        <- ifelse(p$Institute == 'Site 01 - MUMC' | p$Institute == "Site 05 - Zuyderland MC" |
                             p$Institute == "Site 03 - Gelderse Vallei"| p$Institute == "Site 04 - MS Twente" |
                             p$Institute == "Site 10 - Catharina", p$LB_UREA*6.006, p$LB_UREA) #in mg/dl
p$Creatinine_umoll <- ifelse(p$Institute == 'Site 02 - ZOL' | p$Institute == "Site 06 - CHU Liege" |
                             p$Institute == "Site 07 - CHR Liege"| p$Institute == "Site 08 - UZ Brussel" |
                             p$Institute == "Site 09 - AZ Groeninge", p$LB_CREA_D1*88.42, p$LB_CREA_D1) #in umol/l
p$Creatinine_mgdl  <- ifelse(p$Institute == 'Site 01 - MUMC' | p$Institute == "Site 03 - Gelderse Vallei" |
                             p$Institute == "Site 04 - MS Twente"| p$Institute == "Site 05 - Zuyderland MC" |
                             p$Institute == "Site 10 - Catharina", p$LB_CREA_D1/88.42, p$LB_CREA_D1) #in mg/dl
p$Bilirubin_umoll  <- ifelse(p$Institute == 'Site 02 - ZOL' | p$Institute == "Site 06 - CHU Liege" |
                             p$Institute == "Site 07 - CHR Liege"| p$Institute == "Site 08 - UZ Brussel" |
                             p$Institute == "Site 09 - AZ Groeninge", p$LB_BILI*17.1, p$LB_BILI) #umol/l
p$Bilirubin_mgdl   <- ifelse(p$Institute == 'Site 01 - MUMC' | p$Institute == "Site 05 - Zuyderland MC" |
                             p$Institute == "Site 03 - Gelderse Vallei"| p$Institute == "Site 04 - MS Twente" |
                             p$Institute == "Site 10 - Catharina", p$LB_BILI/17.1, p$LB_BILI) #mg/dl
p$Gluc_low_mmoll   <- ifelse(p$Institute == 'Site 02 - ZOL' | p$Institute == "Site 06 - CHU Liege" |
                             p$Institute == "Site 07 - CHR Liege"| p$Institute == "Site 08 - UZ Brussel" |
                             p$Institute == "Site 09 - AZ Groeninge", p$LB_GLUCLOW*0.0555, p$LB_GLUCLOW) #in mmol/l
p$Gluc_high_mmoll  <- ifelse(p$Institute == 'Site 02 - ZOL' | p$Institute == "Site 06 - CHU Liege" |
                             p$Institute == "Site 07 - CHR Liege"| p$Institute == "Site 08 - UZ Brussel" |
                             p$Institute == "Site 09 - AZ Groeninge", p$LB_GLUCHIGH*0.0555, p$LB_GLUCHIGH) #in mmol/l
p$Gluc_low_mgdl    <- ifelse(p$Institute == 'Site 01 - MUMC' | p$Institute == "Site 05 - Zuyderland MC" |
                             p$Institute == "Site 03 - Gelderse Vallei"| p$Institute == "Site 04 - MS Twente" |
                             p$Institute == "Site 10 - Catharina", p$LB_GLUCLOW/0.0555, p$LB_GLUCLOW) #in mg/dl
p$Gluc_high_mgdl   <- ifelse(p$Institute == 'Site 01 - MUMC' | p$Institute == "Site 05 - Zuyderland MC" |
                             p$Institute == "Site 03 - Gelderse Vallei"| p$Institute == "Site 04 - MS Twente" |
                             p$Institute == "Site 10 - Catharina", p$LB_GLUCHIGH/0.0555, p$LB_GLUCHIGH) #in mg/dl
p$Lactate_mmoll    <- ifelse(p$Institute == "Site 06 - CHU Liege", p$LB_LACT*0.0111, 
                             ifelse(p$Institute == "Site 07 - CHR Liege", p$LB_LACT*0.111, p$LB_LACT)) #mmol/l
p$PaCO2            <- ifelse(p$Institute == 'Site 01 - MUMC' | p$Institute == "Site 05 - Zuyderland MC" |
                             p$Institute == 'Site 04 - MS Twente' | p$Institute == 'Site 03 - Gelderse Vallei', 
                             p$VAS_FIO2_1/0.1333, p$VAS_FIO2_1)          #in mmHg
p$PFratio          <- ifelse(p$Institute == 'Site 01 - MUMC' | p$Institute == "Site 05 - Zuyderland MC", 
                             p$VAS_PFRATIO/0.1333, p$VAS_PFRATIO)        #in mmHg

p$Albumin          <- p$LB_ALB/10                                        #in g/dl
p$Magnesium_mgdl   <- p$LB_MG/0.4114                                     #in mg/dl
p$Magnesium_mmoll  <- p$LB_MG                                            #in mmol/l
p$Phosphate_mgdl   <- p$LB_PO4/0.3229                                    #in mg/dl
p$Phosphate_mmoll  <- p$LB_PO4                                           #in mmol/l
p$Lactate_mgdl     <- p$Lactate_mmoll/0.111                              #in mg/dl
p$PaO2             <- (p$PFratio * p$VAS_FIO2) / 100                     #in mmHg
p$Aagradient       <- (((p$VAS_FIO2/100)*713 - (p$PaCO2/0.8)) - p$PaO2)  #in mmHg


# 3. Recalculate automatic Castor calculations   ----

## 3.1 BMI   ----
p$BMI <- p$DEM_WEIGHT / ((p$DEM_HEIGHT / 100)^2)

## 3.2 Ideal body weight   ----
p$IBW <- 27 * ((p$DEM_HEIGHT/100)^2)

## 3.3 Correct weight (for calculation of targets)   ----
p$WEIGHTUSED <- ifelse(p$BMI > 27, p$IBW, p$DEM_WEIGHT)

## 3.4 Glasgow Coma Scale   ----
p$GCS <- as.numeric(p$GCS_EOPT + p$GCS_MOPT + p$GCS_VOPT)

## 3.5 Definitive ICU admission date ----
# In case of previous ICU admission at other ICU, takes other ICU  admission 
# date first (if available), otherwise takes the ICU admission date at study site.
p$ICU_admissiondate <- as.Date(coalesce(p$ADM_ICUDT, p$ADM_DT),format = "%d-%m-%Y")

## 3.6 Definitive hospital admission date ----
# Hospital admission date is only left open in case a patient is admitted from 
# the ER on the same day (N=393). First take the ICU admission date as hospital 
# admission date if available, otherwise take filled in hospital admission date.
p$ADM_HOSPDT <- as.Date(p$ADM_HOSPDT, format = "%d-%m-%Y")
p$hosp_admissiondate <- as.Date(coalesce(p$ADM_HOSPDT, p$ICU_admissiondate), format = "%d-%m-%Y")

## 3.7 Definitive hospital discharge date ----
# Hospital discharge date is left open in case of death. Since they either are 
# discharged, or die during hospitalization, impute death date if available
p$hosp_dischargedate <- as.Date(coalesce(p$DIS_HOSPDT,p$TERM_DEATHDT),format = "%d-%m-%Y")

## 3.8 pre-ICU length of stay ----
#Calculate pre-ICU length of stay
p$preICU_LoS <- as.numeric(difftime(p$ICU_admissiondate, p$hosp_admissiondate, units = "days"))

## 3.9 Days between ICU admission and return to work ----
p$ICUtowork <- as.numeric(difftime(as.Date(p$TERM_WORKDT, format = "%d-%m-%Y"), p$ICU_admissiondate, units = "days"))

# 4 Calculate relevant scores   ----

## 4.1 SOFA score   ----
p$SOFA_resp   <- ifelse(p$PFratio < 100 & p$VAS_VENTYN == 1, 4,
                 ifelse(p$PFratio < 200 & p$VAS_VENTYN == 1, 3,
                 ifelse(p$PFratio < 200 & p$VAS_VENTYN == 0, 2,
                 ifelse(p$PFratio < 300, 2,
                 ifelse(p$PFratio < 400, 1,
                 ifelse(p$PFratio >= 400, 0, NA))))))

p$SOFA_coag   <- ifelse(p$LB_PLT < 20, 4,
                 ifelse(p$LB_PLT < 50, 3,
                 ifelse(p$LB_PLT <100, 2,
                 ifelse(p$LB_PLT <150, 1, 
                 ifelse(p$LB_PLT >= 150, 0, NA)))))

p$SOFA_liver  <- ifelse(p$Bilirubin_umoll > 204, 4,
                 ifelse(p$Bilirubin_umoll >= 102, 3,
                 ifelse(p$Bilirubin_umoll >= 33, 2,
                 ifelse(p$Bilirubin_umoll >= 20, 1,
                 ifelse(p$Bilirubin_umoll < 20, 0, NA)))))

p$SOFA_cardio <- ifelse(p$VAS_OPT.Noradrenaline == 1 & p$VAS_NOR > 0.1, 4,
                 ifelse(p$VAS_OPT.Noradrenaline == 1 & p$VAS_NOR > 0, 3,
                 ifelse(p$VAS_MAPLOW < 70, 1,
                 ifelse(p$VAS_MAPLOW >= 70, 0, NA))))

p$SOFA_neuro  <- ifelse(p$GCS <= 5, 4,
                 ifelse(p$GCS <= 9, 3,
                 ifelse(p$GCS <= 12, 2,
                 ifelse(p$GCS <= 14, 1,
                 ifelse(p$GCS == 15, 0, NA)))))

p$SOFA_renal  <- ifelse(p$ADM_RFYN == 1, 4,
                 ifelse(p$Creatinine_umoll > 440 | p$VAS_URINE < 200, 4,
                 ifelse(p$Creatinine_umoll >= 300 | p$VAS_URINE < 500, 3,
                 ifelse(p$Creatinine_umoll >= 171, 2,
                 ifelse(p$Creatinine_umoll >= 110, 1,
                 ifelse(p$Creatinine_umoll < 110, 0, NA))))))

#Add all subscores together
p$SOFA_score <- rowSums(data.frame(p$SOFA_resp, p$SOFA_coag, p$SOFA_liver,
                                   p$SOFA_cardio, p$SOFA_neuro, p$SOFA_renal))

## 4.2 APACHE II score   ----
p$AP_AGE   <- ifelse(p$DEM_AGE > 74 , 6,
              ifelse(p$DEM_AGE > 64, 5,
              ifelse(p$DEM_AGE > 54, 3,
              ifelse(p$DEM_AGE > 44, 2, 
              ifelse(p$DEM_AGE <= 44, 0, NA)))))

p$AP_HIS   <- ifelse(p$MH_OFYN == 1 & p$ADM_TYPEOPT == 1 | p$MH_OFYN == 1 & p$ADM_TYPEOPT == 2, 5,
              ifelse(p$MH_OFYN == 1 & p$ADM_TYPEOPT == 3, 2, 0))

p$AP_TEMP  <- ifelse(p$VAS_TEMPLOW < 30 | p$VAS_TEMPHIGH >= 41 , 4,
              ifelse(p$VAS_TEMPLOW < 32 | p$VAS_TEMPHIGH >= 39, 3,
              ifelse(p$VAS_TEMPLOW < 34, 2,
              ifelse(p$VAS_TEMPLOW < 36 | p$VAS_TEMPHIGH >= 38.5, 1, 0))))

p$AP_HR    <- ifelse(p$VAS_HRLOW <= 39 | p$VAS_HRHIGH >= 180 , 4,
              ifelse(p$VAS_HRLOW <= 54 | p$VAS_HRHIGH >= 140, 3,
              ifelse(p$VAS_HRLOW <= 69 | p$VAS_HRHIGH >= 110, 2,0)))

p$AP_MAP   <- ifelse(p$VAS_MAPLOW <= 49 | p$VAS_MAPHIGH >= 160 , 4,
              ifelse(p$VAS_MAPHIGH >= 130, 3,
              ifelse(p$VAS_MAPLOW <= 69 | p$VAS_MAPHIGH >= 110, 2,0)))

p$AP_RR    <- ifelse(p$VAS_RRLOW <= 5 | p$VAS_RRHIGH >= 50 , 4,
              ifelse(p$VAS_RRHIGH >= 35, 3,
              ifelse(p$VAS_RRLOW <= 9, 2,
              ifelse(p$VAS_RRLOW <= 11 | p$VAS_RRHIGH >= 25, 1,0))))

p$AP_OXY   <- ifelse(p$VAS_FIO2 < 50 & p$PaO2 < 55 | p$VAS_FIO2 >= 50 & p$Aagradient >= 500, 4,
              ifelse(p$VAS_FIO2 < 50 & p$PaO2 < 60 | p$VAS_FIO2 >= 50 & p$Aagradient >= 350, 3,
              ifelse(p$VAS_FIO2 >= 50 & p$Aagradient >= 200 , 2,
              ifelse(p$VAS_FIO2 < 50 & p$PaO2 < 70 , 1, 0))))

p$AP_PH    <- ifelse(p$VAS_PH >= 7.7 | p$VAS_PH < 7.15 , 4,
              ifelse(p$VAS_PH >= 7.6 | p$VAS_PH <= 7.24, 3,
              ifelse(p$VAS_PH <= 7.32, 2,
              ifelse(p$VAS_PH >= 7.5, 1, 0))))

p$AP_Na    <- ifelse(p$LB_NA >= 180 | p$LB_NA <= 110 , 4,
              ifelse(p$LB_NA >= 160 | p$LB_NA < 120, 3,
              ifelse(p$LB_NA >= 155 | p$LB_NA < 130, 2,
              ifelse(p$LB_NA >= 150, 1, 0))))

p$AP_K     <- ifelse(p$LB_K >= 7 | p$LB_K <2.5 , 4,
              ifelse(p$LB_K >= 6, 3,
              ifelse(p$LB_K < 3, 2,
              ifelse(p$LB_K >= 5.5 | p$LB_K <= 3.4 , 1, 0))))

p$AP_CREAT <- ifelse(p$Creatinine_mgdl >= 3.5 , 4,
              ifelse(p$Creatinine_mgdl >= 2, 3,
              ifelse(p$Creatinine_mgdl >= 1.5 | p$Creatinine_mgdl <= 0.6, 2, 0)))

p$AP_CREAT <- ifelse(p$ADM_RFYN == 1 , p$AP_CREAT*2, p$AP_CREAT)

p$AP_HT    <- ifelse(p$Hematocrit >= 60 | p$Hematocrit < 20, 4,
              ifelse(p$Hematocrit >= 50 | p$Hematocrit <= 29.9, 2,
              ifelse(p$Hematocrit >= 46, 1, 0)))

p$AP_WBC   <- ifelse(p$LB_WBC >= 40 | p$LB_WBC < 1 , 4,
              ifelse(p$LB_WBC >= 20 | p$LB_WBC < 3, 2,
              ifelse(p$LB_WBC >= 15, 1, 0)))

p$AP_GCS   <- 15 - p$GCS

#Add all subscores together
p$APACHEscore <- rowSums(data.frame(p$AP_AGE, p$AP_HIS, p$AP_TEMP, p$AP_HR, 
                                    p$AP_MAP,p$AP_RR, p$AP_OXY, p$AP_PH,
                                    p$AP_Na, p$AP_K, p$AP_CREAT, p$AP_HT,
                                    p$AP_WBC,p$AP_GCS),na.rm=FALSE) #gives NA if one or more are NA (N=23)

p$APACHEscorepartly <- rowSums(data.frame(p$AP_AGE, p$AP_HIS, p$AP_TEMP, p$AP_HR, 
                                    p$AP_MAP,p$AP_RR, p$AP_OXY, p$AP_PH,
                                    p$AP_Na, p$AP_K, p$AP_CREAT, p$AP_HT,
                                    p$AP_WBC,p$AP_GCS),na.rm=TRUE) #This score is calculated to aid in NRS score calculation. No NAs.


## 4.3 APACHE IV score   ----
ap4 <- read.csv("~/PRECISe/PRECISe_FINAL_ANALYSES/data/processed/MUMC-ap4test-20240104-Scored-20240116.csv", sep=";", 
              na.strings = list(""))
p$APACHEIVscore <- ap4$APACHEIV_Score
p$APACHEIVmortpred <- ap4$APACHEIV_Predictie

## 4.4 SAPS II   ----
p$SAPS_age    <- ifelse(p$DEM_AGE >= 80, 18,
                 ifelse(p$DEM_AGE >=75, 16,
                 ifelse(p$DEM_AGE >= 70, 15,
                 ifelse(p$DEM_AGE >= 60, 12,
                 ifelse(p$DEM_AGE >= 40, 7, 0)))))

p$SAPS_HR     <- ifelse(p$VAS_HRLOW < 40, 11, 
                 ifelse(p$VAS_HRHIGH >= 160,7,
                 ifelse(p$VAS_HRHIGH >= 120, 4,
                 ifelse(p$VAS_HRLOW < 70,2,0))))

p$SAPS_BP     <- ifelse(p$VAS_SBPLOW < 70, 13,
                 ifelse(p$VAS_SBPLOW <100, 5,
                 ifelse(p$VAS_SBPHIGH >= 200,2,0)))

p$SAPS_temp   <- ifelse(p$VAS_TEMPHIGH >= 39, 3, 0)

p$SAPS_GCS    <- ifelse(p$GCS < 6, 26,
                 ifelse(p$GCS <= 8, 13,
                 ifelse(p$GCS <= 10, 7,
                 ifelse(p$GCS <= 13, 5, 0))))

p$SAPS_PF     <- ifelse(p$VAS_VENTYN == 1 & p$PFratio < 100, 11,
                 ifelse(p$VAS_VENTYN == 1 & p$PFratio < 200, 9,
                 ifelse(p$VAS_VENTYN == 1 & p$PFratio >= 200, 6,
                 ifelse(p$VAS_VENTYN == 0, 0, NA)))) 

p$SAPS_urea   <- ifelse(p$Urea_mmoll >= 30, 10,
                 ifelse(p$Urea_mmoll >= 10, 6,0))

p$SAPS_urine  <- ifelse(p$VAS_URINE < 500, 11,
                 ifelse(p$VAS_URINE <1000, 4,0))

p$SAPS_Na     <- ifelse(p$LB_NA < 125, 5, 
                 ifelse(p$LB_NA >= 145,1,0))

p$SAPS_K      <- ifelse(p$LB_K >= 5 | p$LB_K <3, 3,0)

p$SAPS_Bic    <- ifelse(p$VAS_HCO3 < 15, 6,
                 ifelse(p$VAS_HCO3 <= 19, 3,0))

p$SAPS_Bili   <- ifelse(p$Bilirubin_mgdl >= 6, 9, 
                 ifelse(p$Bilirubin_mgdl >= 4, 4,0))

p$SAPS_WBC    <- ifelse(p$LB_WBC < 1, 12,
                 ifelse(p$LB_WBC >= 20, 3,0))

#impute 0 for NA (if question was not applicable, NA is default answer)
p$MH_MALIG_OPT[is.na(p$MH_MALIG_OPT)] <- 0

p$SAPS_chron     <- ifelse(p$MH_CHCOPT.AIDS == 1, 17,
                    ifelse(p$MH_MALIG_OPT == 3 | p$MH_MALIG_OPT == 4, 10,
                    ifelse(p$MH_MALIG_OPT == 2, 9, 0)))

p$SAPS_ADMTYPE   <- ifelse(p$ADM_TYPEOPT == 2, 8,
                    ifelse(p$ADM_TYPEOPT == 1, 6, 0))

#Add all subscores together
p$SAPSscore      <- rowSums(data.frame(p$SAPS_age, p$SAPS_HR, p$SAPS_BP, p$SAPS_temp,
                                  p$SAPS_GCS, p$SAPS_PF, p$SAPS_urea, p$SAPS_urine, 
                                  p$SAPS_Na, p$SAPS_K, p$SAPS_Bic, p$SAPS_Bili, 
                                  p$SAPS_WBC, p$SAPS_chron, p$SAPS_ADMTYPE), na.rm = FALSE)

## 4.5 NRS (age-adjusted score)   ----
p$NRS_QUEST1    <- as.numeric(p$NRS_NIOPT)
p$NRS_QUEST2    <- ifelse(p$APACHEscore > 10 | p$APACHEscorepartly > 10, 3, p$NRS_DISOPT)
p$NRSagescore   <- ifelse(p$DEM_AGE >= 70, 1, 0)
p$NRSscore      <- p$NRS_QUEST1 + p$NRS_QUEST2 + p$NRSagescore #gives NA if at least one is NA

## 4.6 Charlson Comorbidity Index   ----
p$CCI <- 
  ifelse(p$MH_CHCOPT.Myocardial.infarction == 1, 1, 0) +
  ifelse(p$MH_CHCOPT.Congestive.Heart.Failure == 1, 1, 0) +
  ifelse(p$MH_CHCOPT.Peripheral.vascular.disease == 1, 1, 0) +
  ifelse(p$MH_CHCOPT.CVA.or.TIA == 1, 1, 0) +
  ifelse(p$MH_CHCOPT.Dementia == 1, 1, 0) +
  ifelse(p$MH_CHCOPT.Chronic.lung.disease == 1, 1, 0) +
  ifelse(p$MH_CHCOPT.Peptic.ulcer.disease == 1, 1, 0) +
  ifelse(p$MH_CHCOPT.Connective.tissue.disease == 1, 1, 0) +
  ifelse(p$MH_CHCOPT.Liver.disease == 1 & p$MH_LIVER_OPT == 1, 1, 0) +
  ifelse(p$MH_CHCOPT.Liver.disease == 1 & p$MH_LIVER_OPT == 2, 1, 0) +
  ifelse(p$MH_CHCOPT.Liver.disease == 1 & p$MH_LIVER_OPT == 3, 3, 0) +
  ifelse(p$MH_CHCOPT.Liver.disease == 1 & p$MH_LIVER_OPT == 4, 3, 0) +
  ifelse(p$MH_CHCOPT.Diabetes.mellitus == 1 & p$MH_DMOPT == 1, 1, 0) +
  ifelse(p$MH_CHCOPT.Diabetes.mellitus == 1 & p$MH_DMOPT == 2, 2, 0) +
  ifelse(p$MH_CHCOPT.Chronic.Kidney.Disease == 1 & p$MH_CKD_OPT.None.of.the.above != 1, 2, 0) +
  ifelse(p$MH_CHCOPT.Hemiplegia == 1, 2, 0) +
  ifelse(p$MH_CHCOPT.Current.malignancy == 1 & p$MH_MALIG_OPT == 1, 2, 0) +
  ifelse(p$MH_CHCOPT.Current.malignancy == 1 & p$MH_MALIG_OPT == 2, 6, 0) +
  ifelse(p$MH_CHCOPT.Current.malignancy == 1 & p$MH_MALIG_OPT == 4, 2, 0) +
  ifelse(p$MH_CHCOPT.Current.malignancy == 1 & p$MH_MALIG_OPT == 3, 2, 0) +  
  ifelse(p$MH_CHCOPT.AIDS == 1, 6, 0) +
  ifelse(p$DEM_AGE > 80, 4, 
         ifelse(p$DEM_AGE > 70, 3,
         ifelse(p$DEM_AGE > 60, 2,
         ifelse(p$DEM_AGE > 50, 1, 0))))

p$CCI <- as.numeric(p$CCI) #no NAs

p$CCI_class <- ifelse(p$CCI >= 5, "5 or more", 
               ifelse(p$CCI == 3 | p$CCI == 4, "3 - 4",
               ifelse(p$CCI == 1 | p$CCI == 2, "1 - 2", '0'))) #no NAs


# 5. Variable "Admission diagnosis system"  ----

# For category "post-operative", the following options
# exist: Cardiovascular, Respiratory, Gastrointestinal, Neurologic,
# Trauma, Genitourinary, Miscellaneous.

# For category "non-operative", the following options exist:
# Cardiovascular, Respiratory, Gastrointestinal, Neurologic,
# Trauma, Genitourinary, Miscellaneous AND Sepsis, Metabolic/endocrine 
# and Hematologic.

# One and only one option of all these 17 options can be ticked per row.
table(p$ADM_TYPEOPT, useNA = "always")
#Rename answer options
p$ADM_POPDIAGOPT <- ifelse(p$ADM_POPDIAGOPT == 1, "Cardiovascular",
                    ifelse(p$ADM_POPDIAGOPT == 2, "Respiratory",
                    ifelse(p$ADM_POPDIAGOPT == 3, "Gastrointestinal",
                    ifelse(p$ADM_POPDIAGOPT == 4, "Neurologic",
                    ifelse(p$ADM_POPDIAGOPT == 5, "Trauma",
                    ifelse(p$ADM_POPDIAGOPT == 6, "Genitourinary",
                    ifelse(p$ADM_POPDIAGOPT == 7, "Miscellaneous",p$ADM_POPDIAGOPT)))))))

p$ADM_NOPDIAGOPT <- ifelse(p$ADM_NOPDIAGOPT == 1, "Cardiovascular",
                    ifelse(p$ADM_NOPDIAGOPT == 2, "Sepsis",
                    ifelse(p$ADM_NOPDIAGOPT == 3, "Respiratory",
                    ifelse(p$ADM_NOPDIAGOPT == 4, "Gastrointestinal",
                    ifelse(p$ADM_NOPDIAGOPT == 5, "Neurologic",
                    ifelse(p$ADM_NOPDIAGOPT == 6, "Trauma",
                    ifelse(p$ADM_NOPDIAGOPT == 7, "Metabolic/endocrine",
                    ifelse(p$ADM_NOPDIAGOPT == 8, "Hematologic",
                    ifelse(p$ADM_NOPDIAGOPT == 9, "Genitourinary",
                    ifelse(p$ADM_NOPDIAGOPT == 10, "Miscellaneous",p$ADM_NOPDIAGOPT))))))))))

p$ADM_SYSTEMDIAGNOSIS <- coalesce(p$ADM_NOPDIAGOPT,p$ADM_POPDIAGOPT)

# 6. Impute 0 for NAs in COVIDYN  ----
p$ADM_COVIDYN[is.na(p$ADM_COVIDYN)] <- 0

# 7. Determine study feed: high protein vs standard protein ----

# Make variable, after unblinding, to identify study feed 
# !!!!!!!!!!!!! IMPORTANT:  CHANGE TO CORRECT LABELING AFTER UNBLINDING !!!!!!!!!!!!!!!!!!!!!

# Now, after partial unblinding, only Group 1 vs Group 2 is known
p$studyfeed <- as.factor(ifelse(p$RAND_KITOPT == "1" | p$RAND_KITOPT == "4", "Group 1", "Group 2"))


# 8. Determine certain (sub)populations ----
# Subgroups based on muscle mass not yet possible to define (lacking data).

p$sub_sex <- ifelse(p$DEM_SEX == 1, 1, 0)                                               # male = 1            female = 0
p$sub_age <- ifelse(p$DEM_AGE >= 65, 1, 0)                                              # >= 65 yrs = 1       < 65 yrs = 0
p$sub_obesity <- ifelse(p$BMI >= 30, 1, 0)                                              # >= 30 = 1           <30 = 0
p$sub_admission <- ifelse(p$ADM_TYPEOPT == 1, 0, 1)                                     # operative = 1       non-operative = 0
p$sub_nutrrisk <- ifelse(p$NRSscore >= 3, 1, 0)                                         # high risk = 1       low risk = 0
p$sub_frailty <- ifelse(p$FRAIL_D0 >= 5, 1, 0)                                          # frail = 1           non frail = 0
p$sub_comorbidity <- ifelse(p$CCI >= 2, 1, 0)                                           # multimorbidity = 1  no multimorbidity = 0
p$sub_sepsis <- ifelse(p$ADM_SEPSYN == 1, 1, 0)                                         # sepsis = 1          non-sepsis = 0
p$sub_disseverity <- ifelse(p$APACHEscore >= median(p$APACHEscore, na.rm = TRUE), 1, 0) # high severity = 1   low severity = 0
p$sub_AKI <- ifelse(p$ADM_RFYN == 1, 1, 0)                                              # AKI = 1             non-AKI = 0
p$sub_organfailure <- ifelse(p$SOFA_score >= median(p$SOFA_score, na.rm = TRUE), 1, 0)  # high SOFA = 1       low SOFA = 0
p$sub_COVID <- ifelse(p$ADM_COVIDYN == 1, 1, 0)                                         # COVID-19 = 1        no COVID-19 = 0
p$sub_TBI <- ifelse(p$ADM_NOPTRAUOPT %in% 1 | p$ADM_NOPTRAUOPT %in% 2 |                 # TBI = 1             no TBI = 0
                      p$ADM_NOPTRAUOPT %in% 3 | p$ADM_NOPTRAUOPT %in% 4 | 
                      p$ADM_POPTRAUOPT %in% 1 | p$ADM_POPTRAUOPT %in% 2, 1, 0)


# 9. Determine variables for survival analysis ----
#Correct the wrongly entered trial termination date ("TERM_INELIGDT") for 04-PRECISe-004
p[p$Participant.Id == "04-PRECISe-004", "TERM_INELIGDT"] <- "28-01-2021"

#Determine survival status for kaplan meier. 1 = death, 0 = censored
p$KM_mortality <- ifelse(p$TERM_REASOPT %in% 3, 1, 0)

#Combine all trial termination dates
p$TERMDATE <- as.Date(coalesce(p$TERM_LOSTDT, p$TERM_DEATHDT, p$TERM_MDDT,
                               p$TERM_WITHDRDT, p$TERM_INELIGDT, p$TERM_OTHDT, 
                               p$TERM_WITHDRPROXDT), format = "%d-%m-%Y")

#Combine all trial termination dates EXCEPT death date --> needed for quantifying nutritional intake
p$TERMDATEwithoutdeath <- as.Date(coalesce(p$TERM_LOSTDT, p$TERM_MDDT, p$TERM_WITHDRDT, 
                                           p$TERM_INELIGDT, p$TERM_OTHDT, 
                                           p$TERM_WITHDRPROXDT), format = "%d-%m-%Y")

#For patients with early discontinuation (due to death or other), difference is calculated between TERMDATE & ICU admission date
p$KM_period <- as.numeric(difftime(p$TERMDATE, p$ICU_admissiondate))+ 1

#For patients with normal completion, the observed period is 184 days (i.e., max. follow-up duration)
p$KM_period <- ifelse(p$TERM_OPT == 0, 184, p$KM_period)


# 10. Definitive ICU discharge date ----

# In case a patient was transferred to another ICU, the discharge date there is 
# the ICU discharge date, otherwise it's the primary ICU discharge date.
# In case a patient was discharged & readmitted to the ICU within 48 hours,
# the ICU discharge date of that readmission is considered the ICU discharge date.

#Import readmissions datafile & slim down
re <- read.csv("~/PRECISe/PRECISe_FINAL_ANALYSES/data/raw/PRECISe_study_ICU_readmissions_export_20240115.csv", sep=";", 
               na.strings = list("", -99, -98, -97, -96, -95, "01-01-2995", 
                                 "01-01-2996", "01-01-2997", "01-01-2998", 
                                 "01-01-2999", "##USER_MISSING_95##",
                                 "##USER_MISSING_96##", "##USER_MISSING_97##",
                                 "##USER_MISSING_98##", "##USER_MISSING_99##"))
re <- re[,c("Participant.Id","READ_TXT","READ_STDT","READ_ONGYN","READ_ENDT")]

#Combine with p to get ICU discharge date
pp <- p[,c("Participant.Id","DIS_ICUDT", "TERM_DEATHDT", "studyfeed")]
re <- merge(re, pp, by=c("Participant.Id"), all = FALSE) 

# Patient 09-PRECISe-032 has readmission report that should have been archived.
re <- re[!(re$Participant.Id == "09-PRECISe-032"),]

#Calculate difference between ICU readmission date & ICU discharge date
re$days <- as.numeric(difftime(as.Date(re$READ_STDT, format = "%d-%m-%Y"), as.Date(re$DIS_ICUDT, format = "%d-%m-%Y"), units = "days"))

#In case it's 2 or smaller, this readmission is considered part of index ICU admission.
re$indexICU <- ifelse(re$days <= 2, 1, 0)

#However, some are <=2 days but >48 hours --> there are not considered part 
#of index ICU admission (checked with source)
re$indexICU <- ifelse(re$indexICU == 1 & re$Participant.Id == "01-PRECISe-035" |
                      re$indexICU == 1 & re$Participant.Id == "02-PRECISe-004" | 
                      re$indexICU == 1 & re$Participant.Id == "02-PRECISe-012" | 
                      re$indexICU == 1 & re$Participant.Id == "02-PRECISe-024" | 
                      re$indexICU == 1 & re$Participant.Id == "02-PRECISe-167" |
                      re$indexICU == 1 & re$Participant.Id == "03-PRECISe-073" | 
                      re$indexICU == 1 & re$Participant.Id == "09-PRECISe-059", 0, re$indexICU)

#For the selected patients, the readmission discharge date is the ICU discharge date.
#Import this date back into main export.
ree <- re[re$indexICU == 1,]
ree <- ree[,c("Participant.Id","READ_ENDT","indexICU" )]

p <- merge(p, ree, by=c("Participant.Id"), all = TRUE) 

#Define actual ICU discharge date: first takes selected ICU readmission discharge date
p$ICU_dischargedate <- as.Date(coalesce(p$READ_ENDT, p$DIS_ICUDISDT, p$DIS_ICUDT, p$TERM_DEATHDT),format = "%d-%m-%Y")


# Remove surplus objects ----
rm(list=ls()[! ls() %in% c("p","re","questLong","t","o", "pn", "med", "h","i","sf")])
###############################################################################|
### PRECISe Treatment reports (t)
### Julia Bels, January 2024
### R version 4.3.2
###
### This script:
### - recalculates lab values on assessment days (days 1, 3, 5, 7, 9, 11, and 13)
### - recalculates the GCS score on assessment days (including imputation of
###   GCS on admission in case patient is still sedated/ventilated)
###
### There are no duplicate treatment days nor day 0 records.
###
###############################################################################|

# 1. Import & general adjustments ----

# Load data
t <- read.csv("~/PRECISe/PRECISe_FINAL_ANALYSES/data/raw/PRECISe_study_Treatment_day_X_export_20240115.csv", sep=";", 
              na.strings = list("", -98, -97, -96, -95, "01-01-2995", 
                                "01-01-2996", "01-01-2997", "01-01-2998", 
                                "01-01-2999", "##USER_MISSING_95##",
                                "##USER_MISSING_96##", "##USER_MISSING_97##",
                                "##USER_MISSING_98##", "##USER_MISSING_99##")) #code -99 ("not done") not added yet due to use in mobilisation status

# Define study site
t$Institute = substr(t$Participant.Id,1,nchar(as.character(t$Participant.Id))-12)

# Import some (wide format) study export data to long format treatment report data
pp <- p[,c("Participant.Id","RAND_KITOPT", "RAND_DT","ICU_admissiondate", "WEIGHTUSED", "GCS", "studyfeed", "DEM_SEX")]
t <- merge(t, pp, all.x=TRUE, by="Participant.Id")

# Adjust dates of 2 treatment reports due to overlapping date (but separate treatment days)

# This was agreed upon and documented by the study team & data management.
# Date of first treatment day was adjusted to 1 day earlier; this should have no effect other than
# that no duplicate treatment reports exist anymore. Other than that, the dates of the treatment
# reports are not used for analyses or calculations.

t$NUTR_DT <- ifelse(t$Participant.Id == "02-PRECISe-176" 
                    & t$Repeating.data.Name.Custom == "Treatment day 1 - 20-07-2022 08:45:53", 
                    "19-07-2022", t$NUTR_DT)

t$NUTR_DT <- ifelse(t$Participant.Id == "05-PRECISe-045"
                    & t$Repeating.data.Name.Custom == "Treatment day1 - 10-05-2022  (tot 10:00)", 
                    "09-05-2022", t$NUTR_DT)

# 2. Calculate treatment days  ----

# Convert relevant variables to dates
t$NUTR_DT <- as.Date(t$NUTR_DT,"%d-%m-%Y")
t$RAND_DT <- as.Date(t$RAND_DT,"%d-%m-%Y")
t$ICU_admissiondate <- as.Date(t$ICU_admissiondate,"%d-%m-%Y")

#make new variable, calculating treatment day from randomisation
t$RANDDAYCALC <- as.numeric(difftime(t$NUTR_DT, t$RAND_DT, units = "days")) + 1

#make new variable, calculating treatment day from ICU admission
t$ICUDAYCALC <- as.numeric(difftime(t$NUTR_DT, t$ICU_admissiondate, units = "days")) + 1

# 3. Define mobilization status ----

# Field options: -99 = Mobilization not done; 0 = Mobilization physically not possible
# Recode -99 (not done) and option 0 (not done) into 0 for new mobilization variable
t$mobilization <- ifelse(t$MOB_OPT %in% -99 | t$MOB_OPT %in% 0, 0, t$MOB_OPT)

#Define which days a patient was mobilized & count them per patient
t$mobilizedYN <- ifelse(t$mobilization != 0, 1, 0)
mobdays <- aggregate(mobilizedYN ~ Participant.Id, t, sum)

#Add information to primary export & rename
p <- merge(p, mobdays, by=c("Participant.Id"), all = TRUE)
names(p)[names(p) == 'mobilizedYN'] <- 'mobilizationDays'

# 4. Recode -99 into NA ----
t[t == -99] <- NA

# 5. Recalculate lab values ----
t$Haemoglobin        <- ifelse(t$Institute == '01' |t$Institute == "05" |
                               t$Institute == "03"| t$Institute == "04" |
                               t$Institute == "010", t$ASS_HB/0.6206, t$ASS_HB) #in g/dl
t$Hematocrit         <- ifelse(t$Institute == '01' |t$Institute == "05" |
                               t$Institute == "03"| t$Institute == "04" |
                               t$Institute == "010", t$ASS_HCT*100, t$ASS_HCT) #in %
t$Urea_mmoll         <- ifelse(t$Institute == '02' |t$Institute == "06" |
                               t$Institute == "07"| t$Institute == "08" |
                               t$Institute == '09', t$ASS_UREA*0.16652174, t$ASS_UREA) #in mmol/l
t$Urea_mgdl          <- ifelse(t$Institute == '01' |t$Institute == "05" |
                               t$Institute == "03"| t$Institute == "04" |
                               t$Institute == "010", t$ASS_UREA/0.16652174, t$ASS_UREA) #in mg/dl
t$Creatinine_umoll   <- ifelse(t$Institute == '02' |t$Institute == "06" |
                               t$Institute == "07"| t$Institute == "08" |
                               t$Institute == "09", t$ASS_CREA*88.42, t$ASS_CREA) #in umol/l
t$Creatinine_mgdl    <- ifelse(t$Institute == '01' |t$Institute == "03" |
                               t$Institute == "04"| t$Institute == "05" |
                               t$Institute == "010", t$ASS_CREA/88.42, t$ASS_CREA) #in mg/dl
t$Bilirubin_umoll    <- ifelse(t$Institute == '02' |t$Institute == "06" |
                               t$Institute == "07"| t$Institute == "08" |
                               t$Institute == "09", t$ASS_BILI*17.1, t$ASS_BILI) #umol/l
t$Bilirubin_mgdl     <- ifelse(t$Institute == '01' |t$Institute == "05" |
                               t$Institute == "03"| t$Institute == "04" |
                               t$Institute == "010", t$ASS_BILI/17.1, t$ASS_BILI) #mg/dl
t$Gluc_low_mmoll     <- ifelse(t$Institute == '02' |t$Institute == "06" |
                               t$Institute == "07"| t$Institute == "08" |
                               t$Institute == "09", t$ASS_GLUCLOW*0.0555, t$ASS_GLUCLOW) #in mmol/l
t$Gluc_high_mmoll    <- ifelse(t$Institute == '02' |t$Institute == "06" |
                               t$Institute == "07"| t$Institute == "08" |
                               t$Institute == "09", t$ASS_GLUCHIGH*0.0555, t$ASS_GLUCHIGH) #in mmol/l
t$Gluc_low_mgdl      <- ifelse(t$Institute == '01' |t$Institute == "05" |
                               t$Institute == "03"| t$Institute == "04" |
                               t$Institute == "010", t$ASS_GLUCLOW/0.0555, t$ASS_GLUCLOW) #in mg/dl
t$Gluc_high_mgdl     <- ifelse(t$Institute == '01' |t$Institute == "05" |
                               t$Institute == "03"| t$Institute == "04" |
                               t$Institute == "010", t$ASS_GLUCHIGH/0.0555, t$ASS_GLUCHIGH) #in mg/dl
t$PFratio            <- ifelse(t$Institute == '01' |t$Institute == "05", 
                               t$ASS_RATIO/0.1333, t$ASS_RATIO)  #in mmHg

t$Albumin            <- t$ASS_ALB/10      #in g/dl
t$Magnesium_mgdl     <- t$ASS_MG/0.4114   #in mg/dl
t$Magnesium_mmoll    <- t$ASS_MG          #in mmol/l
t$Phosphate_mgdl     <- t$ASS_PO4/0.3229  #in mg/dl
t$Phosphate_mmoll    <- t$ASS_PO4         #in mmol/l

# 6. (re)Calculate relevant variables ----

### 6.1 Calculate Glasgow Coma Score  ----
t$GCStrt <- as.numeric(t$ASS_GCSEOPT + t$ASS_GCSMOPT + t$ASS_GCSVOPT)

### 6.2 Calculate imputed Glasgow Coma Score  ----
# Impute pre-admission GCS if patient is still sedated or ventilated
t$GCSimputed <- ifelse(t$ASS_SEDYN == "1" | t$ASS_VENTYN == "1", t$GCS, t$GCStrt)

### 6.3 Calculate SOFA scores ----
t$SOFA_resp_trt    <- ifelse(t$PFratio < 100 & t$ASS_VENTYN == 1, 4,
                      ifelse(t$PFratio < 200 & t$ASS_VENTYN == 1, 3,
                      ifelse(t$PFratio <200 & t$ASS_VENTYN == 0, 2,
                      ifelse(t$PFratio < 300, 2,
                      ifelse(t$PFratio < 400, 1,
                      ifelse(t$PFratio >= 400, 0, NA))))))

t$SOFA_coag_trt    <- ifelse(t$ASS_PLT < 20, 4,
                      ifelse(t$ASS_PLT < 50, 3,
                      ifelse(t$ASS_PLT <100, 2,
                      ifelse(t$ASS_PLT <150, 1, 
                      ifelse(t$ASS_PLT >= 150, 0, NA)))))

t$SOFA_liver_trt   <- ifelse(t$Bilirubin_umoll > 204, 4,
                      ifelse(t$Bilirubin_umoll >= 102, 3,
                      ifelse(t$Bilirubin_umoll >= 33, 2,
                      ifelse(t$Bilirubin_umoll >= 20, 1,
                      ifelse(t$Bilirubin_umoll < 20, 0, NA)))))

t$SOFA_cardio_trt  <- ifelse(t$ASS_OPT.Noradrenaline == 1 & t$ASS_NOR > 0.1, 4,
                      ifelse(t$ASS_OPT.Noradrenaline == 1 & t$ASS_NOR > 0, 3,
                      ifelse(t$ASS_MAP <70, 1,
                      ifelse(t$ASS_MAP >= 70, 0, NA))))

t$SOFA_neuro_trt   <- ifelse(t$GCSimputed <= 5, 4,
                      ifelse(t$GCSimputed <= 9, 3,
                      ifelse(t$GCSimputed <= 12, 2,
                      ifelse(t$GCSimputed <= 14, 1,
                      ifelse(t$GCSimputed == 15, 0, NA)))))

t$SOFA_renal_trt   <- ifelse(t$Creatinine_umoll >440 | t$ASS_URINE < 200, 4,
                      ifelse(t$Creatinine_umoll >=300 | t$ASS_URINE <500, 3,
                      ifelse(t$Creatinine_umoll >= 171, 2,
                      ifelse(t$Creatinine_umoll >= 110, 1,
                      ifelse(t$Creatinine_umoll < 110, 0, NA)))))

sofatrt <- t[,c("SOFA_resp_trt", "SOFA_coag_trt", "SOFA_liver_trt",
                "SOFA_cardio_trt", "SOFA_neuro_trt", "SOFA_renal_trt")]

t$SOFA_score_trt <- rowSums(sofatrt, na.rm = FALSE)

# Remove surplus objects ----
rm(list=ls()[! ls() %in% c("p","re","questLong","t","o", "pn", "med", "h","i","sf")])

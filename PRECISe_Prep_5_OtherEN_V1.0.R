###############################################################################|
### PRECISe other enteral nutrition 
### Julia Bels, October 2023
### R version 4.3.1
###
### This script serves to calculate the total ingested amount of calories
### and protein of other enteral nutrition given per day during the study.
###
### Caloric and protein content was added from nutrition data of manufacturers.
###
### This contains no duplicates nor day 0 records.
###
###############################################################################|

# 1. Other enteral nutrition reports |  o  ----

## 1.1 Import & general adjustments ----

#Import data
o <- read.csv("~/PRECISe/PRECISe_FINAL_ANALYSES/data/raw/PRECISe_study_Other_enteral_nutrition_export_20240115.csv", sep=";", 
              na.strings = list("", -99, -98, -97, -96, -95, "01-01-2995", 
                                "01-01-2996", "01-01-2997", "01-01-2998", 
                                "01-01-2999", "##USER_MISSING_95##",
                                "##USER_MISSING_96##", "##USER_MISSING_97##",
                                "##USER_MISSING_98##", "##USER_MISSING_99##")) 

#Delete records from test site
o$studysite = substr(o$Participant.Id,1,nchar(as.character(o$Participant.Id))-12)

#Import relevant data from main study export (p)
pp <- p[,c("Participant.Id","RAND_KITOPT", "RAND_DT","ICU_admissiondate" ,"WEIGHTUSED")]
o <- merge(o, pp, all.x=TRUE, by="Participant.Id")

#cnvert relevant variables to dates
o$OTHNUTR_DT <- as.Date(o$OTHNUTR_DT,"%d-%m-%Y")
o$RAND_DT <- as.Date(o$RAND_DT,"%d-%m-%Y")
o$ICU_admissiondate <- as.Date(o$ICU_admissiondate,"%d-%m-%Y")

#make new variable, calculating treatment day from randomisation
o$RANDDAYCALC <- as.numeric(difftime(o$OTHNUTR_DT, o$RAND_DT, units = "days")) + 1

#make new variable, calculating treatment day from randomisation
o$ICUDAYCALC <- as.numeric(difftime(o$OTHNUTR_DT, o$ICU_admissiondate, units = "days")) + 1

## 1.2 Define conversion factors for calories  (66 = other) ----
o$caloricfactorOEN <- ifelse(o$OTHNUTR_TYPEOPT == "36" | o$OTHNUTR_TYPEOPT == "47" | 
                             o$OTHNUTR_TYPEOPT == "52" | o$OTHNUTR_TYPEOPT == "62" |
                             o$OTHNUTR_TYPEOPT == "20", 1,
                      ifelse(o$OTHNUTR_TYPEOPT == "42", 1.04,
                      ifelse(o$OTHNUTR_TYPEOPT == "4" | o$OTHNUTR_TYPEOPT == "34", 1.2,
                      ifelse(o$OTHNUTR_TYPEOPT == "57", 1.25,
                      ifelse(o$OTHNUTR_TYPEOPT == "56", 1.26,
                      ifelse(o$OTHNUTR_TYPEOPT == "55" | o$OTHNUTR_TYPEOPT == "60", 1.28,
                      ifelse(o$OTHNUTR_TYPEOPT == "35", 1.3,
                      ifelse(o$OTHNUTR_TYPEOPT == "16" | o$OTHNUTR_TYPEOPT == "29" | 
                             o$OTHNUTR_TYPEOPT == "49" | o$OTHNUTR_TYPEOPT == "50" | 
                             o$OTHNUTR_TYPEOPT == "53", 1.5,
                      ifelse(o$OTHNUTR_TYPEOPT == "69", 1.52,
                      ifelse(o$OTHNUTR_TYPEOPT == "37", 1.8,
                      ifelse(o$OTHNUTR_TYPEOPT == "7" | o$OTHNUTR_TYPEOPT == "24" | 
                             o$OTHNUTR_TYPEOPT == "48", 2, NA)))))))))))

o$caloricfactorOEN <- ifelse(o$OTHNUTR_TYPEOPT == "66" & o$Participant.Id == "01-PRECISe-042"| 
                             o$OTHNUTR_TYPEOPT == "66" & o$Participant.Id == "01-PRECISe-222", 1.2,
                      ifelse(o$OTHNUTR_TYPEOPT == "66" & o$Participant.Id == "01-PRECISe-046", 3.33, o$caloricfactorOEN))

## 1.3 Define conversion factors for protein (66 = other) ----
o$proteinfactorOEN <- ifelse(o$OTHNUTR_TYPEOPT == "20", 0.038,
                      ifelse(o$OTHNUTR_TYPEOPT == "47" | o$OTHNUTR_TYPEOPT == "52" |
                             o$OTHNUTR_TYPEOPT == "62", 0.04,
                      ifelse(o$OTHNUTR_TYPEOPT == "34" | o$OTHNUTR_TYPEOPT == "36" |
                             o$OTHNUTR_TYPEOPT == "42", 0.055,
                      ifelse(o$OTHNUTR_TYPEOPT == "29", 0.058,
                      ifelse(o$OTHNUTR_TYPEOPT == "4" | o$OTHNUTR_TYPEOPT == "49" |
                             o$OTHNUTR_TYPEOPT == "49" | o$OTHNUTR_TYPEOPT == "50", 0.06,
                      ifelse(o$OTHNUTR_TYPEOPT == "57" | o$OTHNUTR_TYPEOPT == "60", 0.063,
                      ifelse(o$OTHNUTR_TYPEOPT == "16" | o$OTHNUTR_TYPEOPT == "48" | 
                             o$OTHNUTR_TYPEOPT == "53" | o$OTHNUTR_TYPEOPT == "55", 0.075,
                      ifelse(o$OTHNUTR_TYPEOPT == "35" | o$OTHNUTR_TYPEOPT == "37", 0.081,
                      ifelse(o$OTHNUTR_TYPEOPT == "69", 0.094,
                      ifelse(o$OTHNUTR_TYPEOPT == "7" | o$OTHNUTR_TYPEOPT == "24" | 
                             o$OTHNUTR_TYPEOPT == "56", 0.1, NA))))))))))

o$proteinfactorOEN <- ifelse(o$OTHNUTR_TYPEOPT == "66" & o$Participant.Id == "01-PRECISe-042" | 
                             o$OTHNUTR_TYPEOPT == "66" & o$Participant.Id == "01-PRECISe-222", 0.044,
                      ifelse(o$OTHNUTR_TYPEOPT == "66" & o$Participant.Id == "01-PRECISe-046", 0.5, o$proteinfactorOEN))

## 1.4 Calculate total caloric intake  ----
o$caloriesOEN <- o$caloricfactorOEN * o$OTHNUTR_VOL

# These patients had a combination of other EN ("1") -> totals calculated manually
o$caloriesOEN <-  ifelse(o$Participant.Id == "01-PRECISe-062" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "30", 280, 
                  ifelse(o$Participant.Id == "01-PRECISe-222" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "12", 688.8, 
                  ifelse(o$Participant.Id == "01-PRECISe-222" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "25", 2100, 
                  ifelse(o$Participant.Id == "01-PRECISe-222" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "26", 2790,
                  ifelse(o$Participant.Id == "02-PRECISe-038" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "28", 1342,
                  ifelse(o$Participant.Id == "02-PRECISe-162" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "69", 1230,
                  ifelse(o$Participant.Id == "02-PRECISe-162" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "83", 1682,
                  ifelse(o$Participant.Id == "03-PRECISe-018" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "18", 2128.8,
                  ifelse(o$Participant.Id == "03-PRECISe-018" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "29", 1930.8,
                  ifelse(o$Participant.Id == "03-PRECISe-018" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "30", 2020.3,
                  ifelse(o$Participant.Id == "03-PRECISe-018" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "32", 1904.6,
                  ifelse(o$Participant.Id == "03-PRECISe-072" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "23", 1578,
                  ifelse(o$Participant.Id == "03-PRECISe-072" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "24", 2414,
                  ifelse(o$Participant.Id == "03-PRECISe-072" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "25", 2396,
                  ifelse(o$Participant.Id == "03-PRECISe-072" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "26", 2417,
                  ifelse(o$Participant.Id == "03-PRECISe-072" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "27", 1771,
                  ifelse(o$Participant.Id == "03-PRECISe-072" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "28", 1847,
                  ifelse(o$Participant.Id == "03-PRECISe-072" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "29", 1790,
                  ifelse(o$Participant.Id == "03-PRECISe-072" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "30", 1667,
                  ifelse(o$Participant.Id == "03-PRECISe-072" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "31", 1833,
                  ifelse(o$Participant.Id == "03-PRECISe-072" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "32", 2009,
                  ifelse(o$Participant.Id == "03-PRECISe-072" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "33", 2518,
                  ifelse(o$Participant.Id == "03-PRECISe-072" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "34", 2323,
                  ifelse(o$Participant.Id == "03-PRECISe-072" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "35", 864,
                  ifelse(o$Participant.Id == "03-PRECISe-072" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "36", 1140,
                  ifelse(o$Participant.Id == "04-PRECISe-049" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "24", 1011.8,
                  ifelse(o$Participant.Id == "04-PRECISe-049" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "35", 842,
                  ifelse(o$Participant.Id == "04-PRECISe-049" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "36", 1350,
                  ifelse(o$Participant.Id == "08-PRECISe-033" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "37", 2045.6, o$caloriesOEN)))))))))))))))))))))))))))))

## 1.5 Calculate total protein intake  ----
o$proteinOEN <- o$proteinfactorOEN * o$OTHNUTR_VOL

# These patients had a combination of other EN ("1") -> totals calculated manually
o$proteinOEN <-  ifelse(o$Participant.Id == "01-PRECISe-062" & o$OTHNUTR_TYPEOPT == "1" & o$ICUDAYCALC == "30", 12.56, 
                 ifelse(o$Participant.Id == "01-PRECISe-222" & o$OTHNUTR_TYPEOPT == "1" & o$ICUDAYCALC == "12", 26.80, 
                 ifelse(o$Participant.Id == "01-PRECISe-222" & o$OTHNUTR_TYPEOPT == "1" & o$ICUDAYCALC == "25", 91.6, 
                 ifelse(o$Participant.Id == "01-PRECISe-222" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "26", 94,
                 ifelse(o$Participant.Id == "02-PRECISe-038" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "28", 51.7,
                 ifelse(o$Participant.Id == "02-PRECISe-162" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "69", 48.8,
                 ifelse(o$Participant.Id == "02-PRECISe-162" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "83", 67.2,
                 ifelse(o$Participant.Id == "03-PRECISe-018" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "18", 98.50,
                 ifelse(o$Participant.Id == "03-PRECISe-018" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "29", 96.47,
                 ifelse(o$Participant.Id == "03-PRECISe-018" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "30", 106.11,
                 ifelse(o$Participant.Id == "03-PRECISe-018" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "32", 98.90,
                 ifelse(o$Participant.Id == "03-PRECISe-072" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "23", 82,
                 ifelse(o$Participant.Id == "03-PRECISe-072" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "24", 144,
                 ifelse(o$Participant.Id == "03-PRECISe-072" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "25", 146,
                 ifelse(o$Participant.Id == "03-PRECISe-072" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "26", 140,
                 ifelse(o$Participant.Id == "03-PRECISe-072" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "27", 132,
                 ifelse(o$Participant.Id == "03-PRECISe-072" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "28", 139,
                 ifelse(o$Participant.Id == "03-PRECISe-072" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "29", 136,
                 ifelse(o$Participant.Id == "03-PRECISe-072" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "30", 131,
                 ifelse(o$Participant.Id == "03-PRECISe-072" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "31", 138,
                 ifelse(o$Participant.Id == "03-PRECISe-072" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "32", 146,
                 ifelse(o$Participant.Id == "03-PRECISe-072" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "33", 169,
                 ifelse(o$Participant.Id == "03-PRECISe-072" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "34", 129,
                 ifelse(o$Participant.Id == "03-PRECISe-072" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "35", 60,
                 ifelse(o$Participant.Id == "03-PRECISe-072" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "36", 62,
                 ifelse(o$Participant.Id == "04-PRECISe-049" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "24", 70.29,
                 ifelse(o$Participant.Id == "04-PRECISe-049" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "35", 58,
                 ifelse(o$Participant.Id == "04-PRECISe-049" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "36", 89.5,
                 ifelse(o$Participant.Id == "08-PRECISe-033" & o$OTHNUTR_TYPEOPT == "1" &  o$ICUDAYCALC == "37", 103.13, o$proteinOEN)))))))))))))))))))))))))))))

## 1.6 Input missing intake for 04-PRECISe-008 ----
#Make object lean
o <- o[,c("Participant.Id", "OTHNUTR_DT", "RANDDAYCALC","caloriesOEN", "proteinOEN")]

#Add 60ml of Nutrison Protein Intense for 04-PRECISe-008 --> 6g of protein & 75,6 kcal (checked with comment and source)
extraOEN = data.frame(Participant.Id = c("04-PRECISe-008"), 
                      OTHNUTR_DT = c("29-01-2021"),
                      RANDDAYCALC = c(1),
                      caloriesOEN = c(75.6),
                      proteinOEN = c(6))

o <- rbind(o, extraOEN)

# Remove surplus objects ----
rm(list=ls()[! ls() %in% c("p","re","questLong","t","o", "pn", "med", "h","i","sf")])

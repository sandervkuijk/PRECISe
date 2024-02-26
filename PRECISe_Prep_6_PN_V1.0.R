###############################################################################|
### PRECISe other enteral nutrition 
### Julia Bels, October 2023
### R version 4.3.1
###
### This script serves to calculate the total ingested amount of calories
### and protein of parenteral nutrition given per day during the study.
###
### Caloric and protein content added from nutrition information from manufacturers.
###
### There are no duplicate PN records nor day 0 records.
###
###############################################################################|

## 1.1 Prepare object ----

#select subset of treatment day records where PN was given (i.e., NUTR_SUPPYN = YES (1))
pn <- subset(t, t$NUTR_SUPPYN == 1)

#convert relevant variables to dates
pn$NUTR_DT <- as.Date(pn$NUTR_DT,"%d-%m-%Y")
pn$ICU_admissiondate <- as.Date(pn$ICU_admissiondate,"%d-%m-%Y")

#make new variable, calculating treatment day from nutrition start day
pn$RANDDAYCALC <- as.numeric(difftime(pn$NUTR_DT, pn$RAND_DT, units = "days")) + 1

#make new variable, calculating treatment day from randomisation
pn$ICUDAYCALC <- as.numeric(difftime(pn$NUTR_DT, pn$ICU_admissiondate, units = "days")) + 1

## 1.2 Define conversion factors for calories  (19 = other) ----
pn$caloricfactorPN <- ifelse(pn$NUTR_SUPPOPT == "2", 0.68,
                      ifelse(pn$NUTR_SUPPOPT == "4" | pn$NUTR_SUPPOPT == "5", 0.77,
                      ifelse(pn$NUTR_SUPPOPT == "6", 0.9,
                      ifelse(pn$NUTR_SUPPOPT == "15" | pn$NUTR_SUPPOPT == "16", 1.07,
                      ifelse(pn$NUTR_SUPPOPT == "20" | pn$NUTR_SUPPOPT == "21", 1.1,
                      ifelse(pn$NUTR_SUPPOPT == "22" | pn$NUTR_SUPPOPT == "23", 0.889,
                      ifelse(pn$NUTR_SUPPOPT == "25", 0.7,
                      ifelse(pn$NUTR_SUPPOPT == "28", 0.46,
                      ifelse(pn$NUTR_SUPPOPT == "33", 0.95, NA)))))))))

pn$caloricfactorPN <- ifelse(pn$NUTR_SUPPOPT == "19" & pn$Participant.Id == "09-PRECISe-013" | 
                             pn$NUTR_SUPPOPT == '19' & pn$Participant.Id == "09-PRECISe-017" |
                             pn$NUTR_SUPPOPT == '19' & pn$Participant.Id == "09-PRECISe-019" | 
                             pn$NUTR_SUPPOPT == '19' & pn$Participant.Id == "09-PRECISe-021", 0.32,
                      ifelse(pn$NUTR_SUPPOPT == "19" & pn$Participant.Id == "010-PRECISe-007", NA, pn$caloricfactorPN))

## 1.3 Define conversion factors for protein (19 = other) ----
pn$proteinfactorPN <- ifelse(pn$NUTR_SUPPOPT == "2", 0.05,
                      ifelse(pn$NUTR_SUPPOPT == "4" | pn$NUTR_SUPPOPT == "5", 0.0425,
                      ifelse(pn$NUTR_SUPPOPT == "6", 0.05,
                      ifelse(pn$NUTR_SUPPOPT == "15" | pn$NUTR_SUPPOPT == "16", 0.057,
                      ifelse(pn$NUTR_SUPPOPT == "20" | pn$NUTR_SUPPOPT == "21", 0.051,
                      ifelse(pn$NUTR_SUPPOPT == "22" | pn$NUTR_SUPPOPT == "23", 0.0655,
                      ifelse(pn$NUTR_SUPPOPT == "25", 0.032,
                      ifelse(pn$NUTR_SUPPOPT == "28", 0.114,
                      ifelse(pn$NUTR_SUPPOPT == "33", 0.0759, NA)))))))))

pn$proteinfactorPN <- ifelse(pn$NUTR_SUPPOPT == "19" & pn$Participant.Id == "09-PRECISe-013" | 
                             pn$NUTR_SUPPOPT == '19' & pn$Participant.Id == "09-PRECISe-017" |
                             pn$NUTR_SUPPOPT == '19' & pn$Participant.Id == "09-PRECISe-019" | 
                             pn$NUTR_SUPPOPT == '19' & pn$Participant.Id == "09-PRECISe-021", 0.08,
                      ifelse(pn$NUTR_SUPPOPT == "19" & pn$Participant.Id == "010-PRECISe-007", NA, pn$proteinfactorPN))


table(pn$NUTR_SUPPOPT) #19 = other | 8 = combination --> will be quantified manually

## 1.4 Calculate total caloric intake  ----
pn$caloriesPN <- pn$caloricfactorPN * pn$NUTR_SUPP

# These patients had a combination of PN (code 8) -> totals calculated manually
pn$caloriesPN <-  ifelse(pn$Participant.Id == "01-PRECISe-095" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "12", 1305, 
                  ifelse(pn$Participant.Id == "02-PRECISe-081" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "17", 885,
                  ifelse(pn$Participant.Id == "02-PRECISe-134" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "27", 1240,
                  ifelse(pn$Participant.Id == "02-PRECISe-138" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "14", 1331,
                  ifelse(pn$Participant.Id == "02-PRECISe-155" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "7", 1059,
                  ifelse(pn$Participant.Id == "02-PRECISe-155" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "31", 1892,
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "19", 1260,
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "20", 2049,
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "21", 2501,
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "22", 2501,
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "23", 2540,
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "24", 2480,
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "25", 2070,
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "26", 2048,
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "27", 2043,
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "28", 1976,
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "29", 1449,
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "30", 1669,
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "31", 1822,
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "32", 1799,
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "33", 1706,
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "34", 1660, 
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "35", 2011, 
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "36", 1701, 
                  ifelse(pn$Participant.Id == "04-PRECISe-049" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "15", 1613, 
                  ifelse(pn$Participant.Id == "04-PRECISe-049" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "16", 2201, 
                  ifelse(pn$Participant.Id == "04-PRECISe-049" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "17", 2255, 
                  ifelse(pn$Participant.Id == "04-PRECISe-049" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "18", 2273, 
                  ifelse(pn$Participant.Id == "04-PRECISe-049" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "19", 2371, 
                  ifelse(pn$Participant.Id == "04-PRECISe-049" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "20", 2448, 
                  ifelse(pn$Participant.Id == "04-PRECISe-049" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "21", 2452, 
                  ifelse(pn$Participant.Id == "04-PRECISe-049" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "22", 2520, 
                  ifelse(pn$Participant.Id == "04-PRECISe-049" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "23", 2368, 
                  ifelse(pn$Participant.Id == "04-PRECISe-049" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "24", 932, 
                  ifelse(pn$Participant.Id == "04-PRECISe-057" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "6", 1267, 
                  ifelse(pn$Participant.Id == "04-PRECISe-057" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "7", 888, 
                  ifelse(pn$Participant.Id == "04-PRECISe-057" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "8", 1525, 
                  ifelse(pn$Participant.Id == "04-PRECISe-057" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "9", 1470, 
                  ifelse(pn$Participant.Id == "04-PRECISe-057" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "10", 1572, 
                  ifelse(pn$Participant.Id == "04-PRECISe-057" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "11", 1614, 
                  ifelse(pn$Participant.Id == "04-PRECISe-057" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "12", 1535, 
                  ifelse(pn$Participant.Id == "04-PRECISe-057" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "13", 1519, 
                  ifelse(pn$Participant.Id == "04-PRECISe-057" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "14", 1440, 
                  ifelse(pn$Participant.Id == "04-PRECISe-057" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "15", 1578, 
                  ifelse(pn$Participant.Id == "04-PRECISe-057" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "16", 1801, 
                  ifelse(pn$Participant.Id == "04-PRECISe-057" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "17", 534, pn$caloriesPN))))))))))))))))))))))))))))))))))))))))))))))

## 1.4 Calculate total protein intake  ----
pn$proteinPN <- pn$proteinfactorPN * pn$NUTR_SUPP

# These patients had a combination of PN (code 8) -> totals calculated manually
pn$proteinPN <-   ifelse(pn$Participant.Id == "01-PRECISe-095" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "12", 71, 
                  ifelse(pn$Participant.Id == "02-PRECISe-081" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "17", 106,
                  ifelse(pn$Participant.Id == "02-PRECISe-134" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "27", 78,
                  ifelse(pn$Participant.Id == "02-PRECISe-138" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "14", 73,
                  ifelse(pn$Participant.Id == "02-PRECISe-155" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "7", 83,
                  ifelse(pn$Participant.Id == "02-PRECISe-155" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "31", 88,
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "19", 109,
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "20", 117,
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "21", 155,
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "22", 155,
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "23", 164,
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "24", 161,
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "25", 125,
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "26", 190,
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "27", 123,
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "28", 172,
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "29", 129,
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "30", 147,
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "31", 173,
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "32", 172,
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "33", 150,
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "34", 155, 
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "35", 181, 
                  ifelse(pn$Participant.Id == "04-PRECISe-021" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "36", 149, 
                  ifelse(pn$Participant.Id == "04-PRECISe-049" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "15", 93, 
                  ifelse(pn$Participant.Id == "04-PRECISe-049" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "16", 149,
                  ifelse(pn$Participant.Id == "04-PRECISe-049" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "17", 149, 
                  ifelse(pn$Participant.Id == "04-PRECISe-049" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "18", 154, 
                  ifelse(pn$Participant.Id == "04-PRECISe-049" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "19", 154, 
                  ifelse(pn$Participant.Id == "04-PRECISe-049" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "20", 160, 
                  ifelse(pn$Participant.Id == "04-PRECISe-049" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "21", 161, 
                  ifelse(pn$Participant.Id == "04-PRECISe-049" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "22", 163, 
                  ifelse(pn$Participant.Id == "04-PRECISe-049" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "23", 156, 
                  ifelse(pn$Participant.Id == "04-PRECISe-049" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "24", 61, 
                  ifelse(pn$Participant.Id == "04-PRECISe-057" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "6", 71, 
                  ifelse(pn$Participant.Id == "04-PRECISe-057" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "7", 87, 
                  ifelse(pn$Participant.Id == "04-PRECISe-057" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "8", 118, 
                  ifelse(pn$Participant.Id == "04-PRECISe-057" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "9", 112, 
                  ifelse(pn$Participant.Id == "04-PRECISe-057" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "10", 120, 
                  ifelse(pn$Participant.Id == "04-PRECISe-057" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "11", 129, 
                  ifelse(pn$Participant.Id == "04-PRECISe-057" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "12", 110, 
                  ifelse(pn$Participant.Id == "04-PRECISe-057" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "13", 115, 
                  ifelse(pn$Participant.Id == "04-PRECISe-057" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "14", 109, 
                  ifelse(pn$Participant.Id == "04-PRECISe-057" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "15", 120, 
                  ifelse(pn$Participant.Id == "04-PRECISe-057" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "16", 130, 
                  ifelse(pn$Participant.Id == "04-PRECISe-057" & pn$NUTR_SUPPOPT == "8" &  pn$ICUDAYCALC == "17", 47, pn$proteinPN))))))))))))))))))))))))))))))))))))))))))))))

# Remove surplus objects ----
rm(list=ls()[! ls() %in% c("p","re","questLong","t","o", "pn", "med", "h","i","sf")])

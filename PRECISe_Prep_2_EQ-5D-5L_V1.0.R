###############################################################################|
### PRECISe EQ-5D-5L calculations 
### Julia Bels, May 2023
### R version 4.2.3
###
###############################################################################|

#Install additional package 
#install.packages("remotes")
#remotes::install_github("brechtdv/EQ5D.be")

# 1. EQ-5D exports |  e ----
## 1.1 Calculation of EQ-5D HUS ----
### 1.1.1 Dutch sites (NL)   ----

#read dataset EQ5D NL sites
e <- read.csv("~/PRECISe/PRECISe_FINAL_ANALYSES/data/raw/PRECISe_study_EQ5D_-_NL_NL_export_20240115.csv", sep=";", na.strings = list("", -99, -98, -97, -96, -95, "01-01-2999", "01-01-2996"))[ ,c("Castor.Participant.ID", "Survey.Package.Name", "EQ5D_NL_MOBOPT", "EQ5D_NL_SCOPT",  "EQ5D_NL_ACTOPT", "EQ5D_NL_PAINOPT", "EQ5D_NL_ANXOPT", "EQ5D_NL_SCORESL")]

#Determine follow-up moment
e$fupday <- ifelse(e$Survey.Package.Name == "Follow-up 30 - Vragenlijsten - NL (NL)", 30,
            ifelse(e$Survey.Package.Name == "Follow-up 90 - Vragenlijsten - NL (NL)", 90,
            ifelse(e$Survey.Package.Name == "Follow-up 180 - Vragenlijsten - NL (NL)", 180, NA)))

#Rename column
names(e)[names(e) == 'Castor.Participant.ID'] <- 'Participant.Id'

#new object, e.nl
e.nl <- e[, c("Participant.Id","EQ5D_NL_MOBOPT","EQ5D_NL_SCOPT","EQ5D_NL_ACTOPT",
              "EQ5D_NL_PAINOPT","EQ5D_NL_ANXOPT","EQ5D_NL_SCORESL","fupday")]

#rename variables for EQ-5D HUS calculation
colnames(e.nl) <- c("Participant.Id","MO","SC","UA","PD","AD","EQ5D.VAS","fupday")

#recode answer options 
e.nl[,c("MO","SC","UA","PD","AD")] <- e.nl[,c("MO","SC","UA","PD","AD")] + 1

#Exclude EQ5D scores that include "NA" (excluding EQ-VAS score) and thus cannot be calculated. 
e.nl <- e.nl[complete.cases(e.nl[ , c('MO', 'SC', 'UA', 'PD', 'AD')]),] 

#Calculate EQ5D scores using Dutch tariff
e.nl$HUS <- eq5d(scores = e.nl[, c("MO", "SC", "UA", "PD", "AD")], version = '5L', country = 'Netherlands', type = 'CW', ignore.incomplete = TRUE)

#Reshape from long to wide format
e.nl_wide <- reshape(e.nl, 
                     timevar = "fupday",
                     idvar = c("Participant.Id"),
                     direction = "wide")

### 1.1.2 Belgian sites (NL)   ----

#read dataset EQ5D BE sites
e <- read.csv("~/PRECISe/PRECISe_FINAL_ANALYSES/data/raw/PRECISe_study_EQ5D_-_BE_NL_export_20240115.csv", sep=";", na.strings = list("", -99, -98, -97, -96, -95, "01-01-2999", "01-01-2996"))[ ,c("Castor.Participant.ID", "Survey.Package.Name", "EQ5D_BE_MOBOPT", "EQ5D_BE_SCOPT",  "EQ5D_BE_ACTOPT", "EQ5D_BE_PAINOPT", "EQ5D_BE_ANXOPT", "EQ5D_BE_SCORESL")]

#Determine follow-up moment
e$fupday <- ifelse(e$Survey.Package.Name == "Follow-up 30 - Vragenlijsten - BE (NL)", 30, 
            ifelse(e$Survey.Package.Name == "Follow-up 90 - Vragenlijsten - BE (NL)", 90,
            ifelse(e$Survey.Package.Name == "Follow-up 180 - Vragenlijsten - BE (NL)", 180, NA)))

#Rename column
names(e)[names(e) == 'Castor.Participant.ID'] <- 'Participant.Id'

#new object, e.be_nl
e.be_nl <- e[, c("Participant.Id","EQ5D_BE_MOBOPT","EQ5D_BE_SCOPT","EQ5D_BE_ACTOPT",
              "EQ5D_BE_PAINOPT","EQ5D_BE_ANXOPT","EQ5D_BE_SCORESL","fupday")]

#rename variables for EQ-5D HUS calculation
colnames(e.be_nl) <- c("Participant.Id","MO","SC","UA","PD","AD","EQ5D.VAS","fupday")

#recode answer options
e.be_nl[,c("MO","SC","UA","PD","AD")] <- e.be_nl[,c("MO","SC","UA","PD","AD")] + 1

#Delete EQ5D scores that include "NA" (excluding EQ-VAS score) and thus cannot be calculated.
e.be_nl <- e.be_nl[complete.cases(e.be_nl[ , c('MO', 'SC', 'UA', 'PD', 'AD')]), ] 

#Calculate EQ5D scores using Belgian tariff
HUS <- index5L(e.be_nl$MO, e.be_nl$SC, e.be_nl$UA, e.be_nl$PD, e.be_nl$AD)
e.be_nl$HUS <- HUS$index

#Reshape from long to wide format
e.be_nl_wide <- reshape(e.be_nl, 
                        timevar = "fupday",
                        idvar = c("Participant.Id"),
                        direction = "wide")

### 1.1.3 Belgian sites (FR)   ----

#read dataset EQ5D BE (FR)
e <- read.csv("~/PRECISe/PRECISe_FINAL_ANALYSES/data/raw/PRECISe_study_EQ5D_-_BE_FR_export_20240115.csv", sep=";", na.strings = list("", -99, -98, -97, -96, -95, "01-01-2999", "01-01-2996"))[ ,c("Castor.Participant.ID", "Survey.Package.Name", "EQ5D_FR_MOBOPT", "EQ5D_FR_SCOPT",  "EQ5D_FR_ACTOPT", "EQ5D_FR_PAINOPT", "EQ5D_FR_ANXOPT", "EQ5D_FR_SCORESL")]

#Determine follow-up moment
e$fupday <- ifelse(e$Survey.Package.Name == "Follow-up 30 - Questionnaires - BE (FR)", 30,
            ifelse(e$Survey.Package.Name == "Follow-up 90 - Questionnaires - BE (FR)", 90, 
            ifelse(e$Survey.Package.Name == "Follow-up 180 - Questionnaires - BE (FR)", 180, NA)))

#Rename column
names(e)[names(e) == 'Castor.Participant.ID'] <- 'Participant.Id'

#new object, e.be_nl
e.be_fr <- e[, c("Participant.Id","EQ5D_FR_MOBOPT","EQ5D_FR_SCOPT","EQ5D_FR_ACTOPT",
                 "EQ5D_FR_PAINOPT","EQ5D_FR_ANXOPT","EQ5D_FR_SCORESL","fupday")]

#rename variables for EQ-5D HUS calculation
colnames(e.be_fr) <- c("Participant.Id","MO","SC","UA","PD","AD","EQ5D.VAS","fupday")

#recode answer options
e.be_fr[,c("MO","SC","UA","PD","AD")] <- e.be_fr[,c("MO","SC","UA","PD","AD")] + 1

#Delete EQ5D scores that include "NA" (excluding EQ-VAS score!) and thus cannot be calculated.
e.be_fr <- e.be_fr[complete.cases(e.be_fr[ , c('MO', 'SC', 'UA', 'PD', 'AD')]), ] 

#Calculate EQ5D scores using Belgian tariff
HUS <- index5L(e.be_fr$MO, e.be_fr$SC, e.be_fr$UA, e.be_fr$PD, e.be_fr$AD)
e.be_fr$HUS <- HUS$index

#Reshape from long to wide format
e.be_fr_wide <- reshape(e.be_fr, 
                        timevar = "fupday",
                        idvar = c("Participant.Id"),
                        direction = "wide")

### 1.1.4 All sites (ENG) ( proxy + baseline)  ----

#read dataset EQ5D baseline proxy (FR)
e <- read.csv("~/PRECISe/PRECISe_FINAL_ANALYSES/data/raw/PRECISe_study_EQ5D_-_ENG_-_Proxy_export_20240115.csv", sep=";", na.strings = list("", -99, -98, -97, -96, -95, "01-01-2999", "01-01-2996"))[ ,c("Castor.Participant.ID", "Survey.Package.Name", "EQ5D_ENG_MOBOPT", "EQ5D_ENG_SCOPT",  "EQ5D_ENG_ACTOPT", "EQ5D_ENG_PAINOPT", "EQ5D_ENG_ANXOPT", "EQ5D_ENG_SCORESL")]

#Rename column
names(e)[names(e) == 'Castor.Participant.ID'] <- 'Participant.Id'

#Determine follow-up moment
e$fupday <- ifelse(e$Survey.Package.Name == "Screening - EQ5D - Proxy", 0,
            ifelse(e$Survey.Package.Name == "Follow-up 30 - EQ5D - Proxy", 30,
            ifelse(e$Survey.Package.Name == "Follow-up 90 - EQ5D - Proxy", 90,
            ifelse(e$Survey.Package.Name == "Follow-up 180 - EQ5D - Proxy", 180, NA))))

#Split dataset based on country of participant, in order to correctly calculate EQ5Ds
e$site <-  substr(e$Participant.Id,1,nchar(e$Participant.Id)-12)
e$country <- ifelse(e$site == "01" | e$site == "03" | e$site == "04" | e$site == "05" | e$site == "010", 0, 1)

#new object, e.eng
e.eng <- e[, c("Participant.Id", "EQ5D_ENG_MOBOPT","EQ5D_ENG_SCOPT","EQ5D_ENG_ACTOPT",
               "EQ5D_ENG_PAINOPT","EQ5D_ENG_ANXOPT","EQ5D_ENG_SCORESL","fupday","site","country" )]

#rename variables for EQ-5D HUS calculation
colnames(e.eng) <- c("Participant.Id","MO","SC","UA","PD","AD","EQ5D.VAS.proxy","fupday", "site", "country")

#recode answer options
e.eng[,c("MO","SC","UA","PD","AD")] <- e.eng[,c("MO","SC","UA","PD","AD")] + 1

#Delete EQ5D scores that include "NA" and thus cannot be calculated. 
# These are missing. --> important: 2 PD subscores are deleted; make separate variable to keep these.
e.eng <- e.eng[complete.cases(e.eng[ , c('MO', 'SC', 'UA', 'PD', 'AD')]), ] 

#Calculate EQ5D scores for Dutch participants
e.eng$HUS <- ifelse(e.eng$country == 0, (eq5d(scores = e.eng[, c("MO", "SC", "UA", "PD", "AD")], version = '5L', country = 'Netherlands', type = 'CW', ignore.incomplete = TRUE)), NA)

#Calculate EQ5D scores for Belgian participants
HUS <- index5L(e.eng$MO, e.eng$SC, e.eng$UA, e.eng$PD, e.eng$AD)
e.eng$HUS <- ifelse(e.eng$country == 1, HUS$index, e.eng$HUS)

#Reshape from long to wide format "EQ5D_proxy_wide"
EQ5D_proxy_wide <- reshape(e.eng, 
                           timevar = "fupday",
                           idvar = c("Participant.Id"),
                           direction = "wide")

## 1.2 Merge EQ5Ds ----

#Merge e.nl_wide, e.be_nl_wide & e.be_fr_wide
e <- merge(e.nl_wide,e.be_nl_wide,by=c("Participant.Id"), all = TRUE)
e <- merge(e,e.be_fr_wide,by=c("Participant.Id"), all = TRUE)

#Merging columns of three datasets into scores per follow-up moment, including EQ-VAS & EQ-5D HUS
e <- e %>% mutate(MO.fup.30 = coalesce(MO.30.x,MO.30.y,MO.30),SC.fup.30 = coalesce(SC.30.x,SC.30.y,SC.30),
                  UA.fup.30 = coalesce(UA.30.x,UA.30.y,UA.30),PD.fup.30 = coalesce(PD.30.x,PD.30.y,PD.30),
                  AD.fup.30 = coalesce(AD.30.x,AD.30.y,AD.30),EQ5D.VAS.30 = coalesce(EQ5D.VAS.30.x,EQ5D.VAS.30.y,EQ5D.VAS.30),
                  EQ5D.HUS.30 = coalesce(HUS.30.x,HUS.30.y,HUS.30),MO.fup.90 = coalesce(MO.90.x,MO.90.y,MO.90),
                  SC.fup.90 = coalesce(SC.90.x,SC.90.y,SC.90),UA.fup.90 = coalesce(UA.90.x,UA.90.y,UA.90),
                  PD.fup.90 = coalesce(PD.90.x,PD.90.y,PD.90),AD.fup.90 = coalesce(AD.90.x,AD.90.y,AD.90),
                  EQ5D.VAS.90 = coalesce(EQ5D.VAS.90.x,EQ5D.VAS.90.y,EQ5D.VAS.90),EQ5D.HUS.90 = coalesce(HUS.90.x,HUS.90.y,HUS.90),
                  MO.fup.180 = coalesce(MO.180.x,MO.180.y,MO.180),SC.fup.180 = coalesce(SC.180.x,SC.180.y,SC.180),
                  UA.fup.180 = coalesce(UA.180.x,UA.180.y,UA.180),PD.fup.180 = coalesce(PD.180.x,PD.180.y,PD.180),
                  AD.fup.180 = coalesce(AD.180.x,AD.180.y,AD.180),EQ5D.VAS.180 = coalesce(EQ5D.VAS.180.x,EQ5D.VAS.180.y,EQ5D.VAS.180),
                  EQ5D.HUS.180 = coalesce(HUS.180.x,HUS.180.y,HUS.180))

#remove surplus columns
e <- e[,c(1,49,56,63,65:82)]

# Merge with EQ-5D proxy scores, these will be added as extra columns at the end of the data file
e <- merge(e,EQ5D_proxy_wide,by=c("Participant.Id"), all = TRUE)

# Rearrange columns: patient - baseline - proxy
e <- e[ ,c("Participant.Id", 
           "EQ5D.HUS.30","EQ5D.VAS.30",
           "EQ5D.HUS.90","EQ5D.VAS.90",
           "EQ5D.HUS.180","EQ5D.VAS.180",
           "HUS.0", "EQ5D.VAS.proxy.0",
           "HUS.30","EQ5D.VAS.proxy.30",
           "HUS.90","EQ5D.VAS.proxy.90",
           "HUS.180","EQ5D.VAS.proxy.180")]

# Rename columns
colnames(e) <- c("Participant.Id", 
                 "EQ5D.HUS.30","EQ5D.VAS.30",
                 "EQ5D.HUS.90","EQ5D.VAS.90",
                 "EQ5D.HUS.180","EQ5D.VAS.180",
                 "EQ5D.HUS.proxy.0", "EQ5D.VAS.proxy.0",
                 "EQ5D.HUS.proxy.30","EQ5D.VAS.proxy.30",
                 "EQ5D.HUS.proxy.90","EQ5D.VAS.proxy.90",
                 "EQ5D.HUS.proxy.180","EQ5D.VAS.proxy.180")


# Combine EQ5D HUS with proxy EQ5D HUS, as proxy scores can be imputed for missing patient scores
e$EQ5D.HUS.30 <- coalesce(e$EQ5D.HUS.30, e$EQ5D.HUS.proxy.30)
e$EQ5D.HUS.90 <- coalesce(e$EQ5D.HUS.90, e$EQ5D.HUS.proxy.90)
e$EQ5D.HUS.180 <- coalesce(e$EQ5D.HUS.180,e$EQ5D.HUS.proxy.180)

# Combine EQ5D VAS with proxy EQ5D VAS
e$EQ5D.VAS.30 <- coalesce(e$EQ5D.VAS.30, e$EQ5D.VAS.proxy.30)
e$EQ5D.VAS.90 <- coalesce(e$EQ5D.VAS.90, e$EQ5D.VAS.proxy.90)
e$EQ5D.VAS.180 <- coalesce(e$EQ5D.VAS.180,e$EQ5D.VAS.proxy.180)

## 1.3 Impute 0 for dead ----

# Import study data for ICU admission date, mortality status, death date
ee <- read.csv(file = "~/PRECISe/PRECISe_FINAL_ANALYSES/data/raw/PRECISe_study_export_20240115.csv", sep=";", na.strings = list("", -99, -98, -97, -96, -95, "01-01-2999", "01-01-2996"))[ ,c('Participant.Id', 'TERM_REASOPT', 'ADM_DT', 'TERM_DEATHDT')]

# Define survival status
ee$survival <- ifelse(ee$TERM_REASOPT %in% 3, 0, NA)

# Calculate difference between death date and ADM_DT ("study days") | survivors get 1000 (needed for ifelse code later)
ee$ADM_DT <- as.Date(ee$ADM_DT, format="%d-%m-%Y")
ee$TERM_DEATHDT <- as.Date(ee$TERM_DEATHDT, format="%d-%m-%Y")

ee$studydays <- ifelse(ee$survival %in% 0, as.numeric(difftime(ee$TERM_DEATHDT, ee$ADM_DT, units = "days")), 999) + 1

# Merge this dataset (ee) to EQ5D data (e)
e <- merge(ee,e,by=c("Participant.Id"), all = TRUE)

#Impute impute 0 if HUS is missing (both patient and proxy) and death occurred before last day of follow-up window
e$EQ5D.HUS.imp0.30 <- ifelse(e$studydays<34, coalesce(e$EQ5D.HUS.30,e$survival), e$EQ5D.HUS.30)
e$EQ5D.HUS.imp0.90 <- ifelse(e$studydays<94, coalesce(e$EQ5D.HUS.90,e$survival), e$EQ5D.HUS.90)
e$EQ5D.HUS.imp0.180 <- ifelse(e$studydays<184, coalesce(e$EQ5D.HUS.180,e$survival), e$EQ5D.HUS.180)

# 2. Add relevant variables to main study object p ----

e <- e[,c(1,21,7,8,22,9,10,23,11,12,13,14)]
p <- merge(p,e,by=c("Participant.Id"), all = TRUE)

# 3. Convert to long format ----

# EQ5D with 0 imputed
wide1 <- e[,c("Participant.Id","EQ5D.HUS.imp0.30","EQ5D.HUS.imp0.90","EQ5D.HUS.imp0.180")]
long1 <- wide1 %>% 
  pivot_longer(
    cols = starts_with("EQ5D.HUS.imp0."),
    names_prefix = "EQ5D.HUS.imp0.",
    names_to = "fupday",
    values_to = "EQ5D.HUS.imp0"
  )
 
# EQ5D without 0 imputed    
wide2 <- e[,c("Participant.Id", "EQ5D.HUS.30", "EQ5D.HUS.90", "EQ5D.HUS.180")]
long2 <- wide2 %>% 
  pivot_longer(
    cols = starts_with("EQ5D.HUS."),
    names_prefix = "EQ5D.HUS.",
    names_to = "fupday",
    values_to = "EQ5D.HUS"
  )

# EQ5D VAS
wide3 <- e[,c("Participant.Id", "EQ5D.VAS.30", "EQ5D.VAS.90", "EQ5D.VAS.180")]
long3 <- wide3 %>% 
  pivot_longer(
    cols = starts_with("EQ5D.VAS."),
    names_prefix = "EQ5D.VAS.",
    names_to = "fupday",
    values_to = "EQ5D.VAS"
  )

# Merge together
questLong <- merge(long1, long2, by=c("Participant.Id", "fupday"), all = TRUE)
questLong <- merge(questLong, long3, by=c("Participant.Id", "fupday"), all = TRUE)

## End of file. ##

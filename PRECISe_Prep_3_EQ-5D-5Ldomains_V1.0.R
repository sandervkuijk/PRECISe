###############################################################################|
### PRECISe EQ-5D-5L subdomains 
### Julia Bels, January 2024
### R version 4.3.2
###
###############################################################################|

# 1. EQ-5D exports |  e ----
## 1.1 Calculation of EQ-5D HUS ----
### 1.1.1 Dutch sites (NL)   ----

#read dataset EQ5D NL sites
e <- read.csv("~/PRECISe/PRECISe_FINAL_ANALYSES/data/raw/PRECISe_study_EQ5D_-_NL_NL_export_20240115.csv", sep=";", na.strings = list("", -99, -98, -97, -96, -95, "01-01-2999", "01-01-2996"))[ ,c("Castor.Participant.ID", "Survey.Package.Name", "EQ5D_NL_MOBOPT", "EQ5D_NL_SCOPT",  "EQ5D_NL_ACTOPT", "EQ5D_NL_PAINOPT", "EQ5D_NL_ANXOPT")]

#Determine follow-up moment
e$fupday <- ifelse(e$Survey.Package.Name == "Follow-up 30 - Vragenlijsten - NL (NL)", 30,
            ifelse(e$Survey.Package.Name == "Follow-up 90 - Vragenlijsten - NL (NL)", 90,
            ifelse(e$Survey.Package.Name == "Follow-up 180 - Vragenlijsten - NL (NL)", 180, NA)))

#Rename column
names(e)[names(e) == 'Castor.Participant.ID'] <- 'Participant.Id'

#new object, e.nl
e.nl <- e[, c("Participant.Id","EQ5D_NL_MOBOPT","EQ5D_NL_SCOPT","EQ5D_NL_ACTOPT",
              "EQ5D_NL_PAINOPT","EQ5D_NL_ANXOPT","fupday")]

#rename variables for EQ-5D HUS calculation
colnames(e.nl) <- c("Participant.Id","MO","SC","UA","PD","AD","fupday")

#recode answer options 
e.nl[,c("MO","SC","UA","PD","AD")] <- e.nl[,c("MO","SC","UA","PD","AD")] + 1

#Reshape from long to wide format
e.nl_wide <- reshape(e.nl, 
                     timevar = "fupday",
                     idvar = c("Participant.Id"),
                     direction = "wide")

### 1.1.2 Belgian sites (NL)   ----

#read dataset EQ5D BE sites
e <- read.csv("~/PRECISe/PRECISe_FINAL_ANALYSES/data/raw/PRECISe_study_EQ5D_-_BE_NL_export_20240115.csv", sep=";", na.strings = list("", -99, -98, -97, -96, -95, "01-01-2999", "01-01-2996"))[ ,c("Castor.Participant.ID", "Survey.Package.Name", "EQ5D_BE_MOBOPT", "EQ5D_BE_SCOPT",  "EQ5D_BE_ACTOPT", "EQ5D_BE_PAINOPT", "EQ5D_BE_ANXOPT")]

#Determine follow-up moment
e$fupday <- ifelse(e$Survey.Package.Name == "Follow-up 30 - Vragenlijsten - BE (NL)", 30, 
            ifelse(e$Survey.Package.Name == "Follow-up 90 - Vragenlijsten - BE (NL)", 90,
            ifelse(e$Survey.Package.Name == "Follow-up 180 - Vragenlijsten - BE (NL)", 180, NA)))

#Rename column
names(e)[names(e) == 'Castor.Participant.ID'] <- 'Participant.Id'

#new object, e.be_nl
e.be_nl <- e[, c("Participant.Id","EQ5D_BE_MOBOPT","EQ5D_BE_SCOPT","EQ5D_BE_ACTOPT",
                 "EQ5D_BE_PAINOPT","EQ5D_BE_ANXOPT","fupday")]

#rename variables for EQ-5D HUS calculation
colnames(e.be_nl) <- c("Participant.Id","MO","SC","UA","PD","AD","fupday")

#recode answer options
e.be_nl[,c("MO","SC","UA","PD","AD")] <- e.be_nl[,c("MO","SC","UA","PD","AD")] + 1

#Reshape from long to wide format
e.be_nl_wide <- reshape(e.be_nl, 
                        timevar = "fupday",
                        idvar = c("Participant.Id"),
                        direction = "wide")

### 1.1.3 Belgian sites (FR)   ----

#read dataset EQ5D BE (FR)
e <- read.csv("~/PRECISe/PRECISe_FINAL_ANALYSES/data/raw/PRECISe_study_EQ5D_-_BE_FR_export_20240115.csv", sep=";", na.strings = list("", -99, -98, -97, -96, -95, "01-01-2999", "01-01-2996"))[ ,c("Castor.Participant.ID", "Survey.Package.Name", "EQ5D_FR_MOBOPT", "EQ5D_FR_SCOPT",  "EQ5D_FR_ACTOPT", "EQ5D_FR_PAINOPT", "EQ5D_FR_ANXOPT")]

#Determine follow-up moment
e$fupday <- ifelse(e$Survey.Package.Name == "Follow-up 30 - Questionnaires - BE (FR)", 30,
            ifelse(e$Survey.Package.Name == "Follow-up 90 - Questionnaires - BE (FR)", 90, 
            ifelse(e$Survey.Package.Name == "Follow-up 180 - Questionnaires - BE (FR)", 180, NA)))

#Rename column
names(e)[names(e) == 'Castor.Participant.ID'] <- 'Participant.Id'

#new object, e.be_nl
e.be_fr <- e[, c("Participant.Id","EQ5D_FR_MOBOPT","EQ5D_FR_SCOPT","EQ5D_FR_ACTOPT",
                 "EQ5D_FR_PAINOPT","EQ5D_FR_ANXOPT","fupday")]

#rename variables for EQ-5D HUS calculation
colnames(e.be_fr) <- c("Participant.Id","MO","SC","UA","PD","AD","fupday")

#recode answer options
e.be_fr[,c("MO","SC","UA","PD","AD")] <- e.be_fr[,c("MO","SC","UA","PD","AD")] + 1

#Reshape from long to wide format
e.be_fr_wide <- reshape(e.be_fr, 
                        timevar = "fupday",
                        idvar = c("Participant.Id"),
                        direction = "wide")

### 1.1.4 All sites (ENG) ( proxy + baseline)  ----

#read dataset EQ5D baseline proxy (FR)
e <- read.csv("~/PRECISe/PRECISe_FINAL_ANALYSES/data/raw/PRECISe_study_EQ5D_-_ENG_-_Proxy_export_20240115.csv", sep=";", na.strings = list("", -99, -98, -97, -96, -95, "01-01-2999", "01-01-2996"))[ ,c("Castor.Participant.ID", "Survey.Package.Name", "EQ5D_ENG_MOBOPT", "EQ5D_ENG_SCOPT",  "EQ5D_ENG_ACTOPT", "EQ5D_ENG_PAINOPT", "EQ5D_ENG_ANXOPT")]

#Rename column
names(e)[names(e) == 'Castor.Participant.ID'] <- 'Participant.Id'

#Determine follow-up moment
e$fupday <- ifelse(e$Survey.Package.Name == "Screening - EQ5D - Proxy", 0,
            ifelse(e$Survey.Package.Name == "Follow-up 30 - EQ5D - Proxy", 30,
            ifelse(e$Survey.Package.Name == "Follow-up 90 - EQ5D - Proxy", 90,
            ifelse(e$Survey.Package.Name == "Follow-up 180 - EQ5D - Proxy", 180, NA))))

#new object, e.eng
e.eng <- e[, c("Participant.Id", "EQ5D_ENG_MOBOPT","EQ5D_ENG_SCOPT","EQ5D_ENG_ACTOPT",
               "EQ5D_ENG_PAINOPT","EQ5D_ENG_ANXOPT","fupday" )]

#rename variables for EQ-5D HUS calculation
colnames(e.eng) <- c("Participant.Id","MO","SC","UA","PD","AD","fupday")

#recode answer options
e.eng[,c("MO","SC","UA","PD","AD")] <- e.eng[,c("MO","SC","UA","PD","AD")] + 1

#Reshape from long to wide format "EQ5D_proxy_wide"
EQ5D_proxy_wide <- reshape(e.eng, 
                           timevar = "fupday",
                           idvar = c("Participant.Id"),
                           direction = "wide")

## 1.2 Merge EQ5Ds ----

#Merge e.nl_wide, e.be_nl_wide & e.be_fr_wide
e <- full_join(e.nl_wide,e.be_nl_wide,by="Participant.Id")
e <- full_join(e,e.be_fr_wide,by="Participant.Id")

#Merging columns of three datasets into scores per follow-up moment, including EQ-VAS & EQ-5D HUS
e <- e %>% mutate(MO.fup.D30 = coalesce(MO.30.x,MO.30.y,MO.30),
                  SC.fup.D30 = coalesce(SC.30.x,SC.30.y,SC.30),
                  UA.fup.D30 = coalesce(UA.30.x,UA.30.y,UA.30),
                  PD.fup.D30 = coalesce(PD.30.x,PD.30.y,PD.30),
                  AD.fup.D30 = coalesce(AD.30.x,AD.30.y,AD.30),
                  MO.fup.D90 = coalesce(MO.90.x,MO.90.y,MO.90),
                  SC.fup.D90 = coalesce(SC.90.x,SC.90.y,SC.90),
                  UA.fup.D90 = coalesce(UA.90.x,UA.90.y,UA.90),
                  PD.fup.D90 = coalesce(PD.90.x,PD.90.y,PD.90),
                  AD.fup.D90 = coalesce(AD.90.x,AD.90.y,AD.90),
                  MO.fup.D180 = coalesce(MO.180.x,MO.180.y,MO.180),
                  SC.fup.D180 = coalesce(SC.180.x,SC.180.y,SC.180),
                  UA.fup.D180 = coalesce(UA.180.x,UA.180.y,UA.180),
                  PD.fup.D180 = coalesce(PD.180.x,PD.180.y,PD.180),
                  AD.fup.D180 = coalesce(AD.180.x,AD.180.y,AD.180))

#remove surplus columns
colnames(e)
e <- e[,c(1, 47:61)]

# Merge with EQ-5D proxy scores, these will be added as extra columns at the end of the data file
e <- full_join(e,EQ5D_proxy_wide,by="Participant.Id")

# Rearrange columns: patient - baseline - proxy
e <- e[ ,c("Participant.Id",
           "MO.fup.D30","SC.fup.D30","UA.fup.D30","PD.fup.D30","AD.fup.D30",
           "MO.fup.D90","SC.fup.D90","UA.fup.D90","PD.fup.D90","AD.fup.D90",
           "MO.fup.D180","SC.fup.D180","UA.fup.D180","PD.fup.D180","AD.fup.D180",
           "MO.30","SC.30","UA.30","PD.30","AD.30",
           "MO.90","SC.90","UA.90","PD.90","AD.90",
           "MO.180","SC.180","UA.180","PD.180","AD.180",
           "MO.0","SC.0","UA.0","PD.0","AD.0")]

# Rename columns
colnames(e) <- c("Participant.Id",
                 "MO.fup.D30","SC.fup.D30","UA.fup.D30","PD.fup.D30","AD.fup.D30",
                 "MO.fup.D90","SC.fup.D90","UA.fup.D90","PD.fup.D90","AD.fup.D90",
                 "MO.fup.D180","SC.fup.D180","UA.fup.D180","PD.fup.D180","AD.fup.D180",
                 "MO.proxy.30","SC.proxy.30","UA.proxy.30","PD.proxy.30","AD.proxy.30",
                 "MO.proxy.90","SC.proxy.90","UA.proxy.90","PD.proxy.90","AD.proxy.90",
                 "MO.proxy.180","SC.proxy.180","UA.proxy.180","PD.proxy.180","AD.proxy.180",
                 "MO.0","SC.0","UA.0","PD.0","AD.0")

#Combine EQ5D mobility question answers 
e$mobility.30 <- coalesce(e$MO.fup.D30, e$MO.proxy.30)
e$mobility.90 <- coalesce(e$MO.fup.D90, e$MO.proxy.90)
e$mobility.180 <- coalesce(e$MO.fup.D180, e$MO.proxy.180)

#Combine EQ5D selfcare question answers 
e$selfcare.30 <- coalesce(e$SC.fup.D30, e$SC.proxy.30)
e$selfcare.90 <- coalesce(e$SC.fup.D90, e$SC.proxy.90)
e$selfcare.180 <- coalesce(e$SC.fup.D180, e$SC.proxy.180)

#Combine EQ5D usual activity question answers 
e$activ.30 <- coalesce(e$UA.fup.D30, e$UA.proxy.30)
e$activ.90 <- coalesce(e$UA.fup.D90, e$UA.proxy.90)
e$activ.180 <- coalesce(e$UA.fup.D180, e$UA.proxy.180)

#Combine EQ5D pain & discomfort question answers
e$painquestion.30 <- coalesce(e$PD.fup.D30, e$PD.proxy.30)
e$painquestion.90 <- coalesce(e$PD.fup.D90, e$PD.proxy.90)
e$painquestion.180 <- coalesce(e$PD.fup.D180, e$PD.proxy.180)

#Combine EQ5D anxiety & depression question answers 
e$anxdepr.30 <- coalesce(e$AD.fup.D30, e$AD.proxy.30)
e$anxdepr.90 <- coalesce(e$AD.fup.D90, e$AD.proxy.90)
e$anxdepr.180 <- coalesce(e$AD.fup.D180, e$AD.proxy.180)

# 2. Combine with primary study export ----

#First, make object more lean
e <- e[,c("Participant.Id",
          "mobility.30","selfcare.30","activ.30","painquestion.30","anxdepr.30",
          "mobility.90","selfcare.90","activ.90","painquestion.90","anxdepr.90",
          "mobility.180","selfcare.180","activ.180","painquestion.180","anxdepr.180")]

p <- merge(p, e, by=c("Participant.Id"), all = TRUE) 

# 3. Convert long format ----

# EQ5D VAS
d <- e[,c("Participant.Id", "mobility.30", "mobility.90", "mobility.180")]
d <- d %>% 
  pivot_longer(
    cols = starts_with("mobility."),
    names_prefix = "mobility.",
    names_to = "fupday",
    values_to = "mobility"
  )

i <- e[,c("Participant.Id", "selfcare.30", "selfcare.90", "selfcare.180")]
i <- i %>% 
  pivot_longer(
    cols = starts_with("selfcare."),
    names_prefix = "selfcare.",
    names_to = "fupday",
    values_to = "selfcare"
  )

f <- e[,c("Participant.Id", "activ.30", "activ.90", "activ.180")]
f <- f %>% 
  pivot_longer(
    cols = starts_with("activ."),
    names_prefix = "activ.",
    names_to = "fupday",
    values_to = "activ"
  )

g <- e[,c("Participant.Id", "painquestion.30", "painquestion.90", "painquestion.180")]
g <- g %>% 
  pivot_longer(
    cols = starts_with("painquestion."),
    names_prefix = "painquestion.",
    names_to = "fupday",
    values_to = "painquestion"
  )

h <- e[,c("Participant.Id", "anxdepr.30", "anxdepr.90", "anxdepr.180")]
h <- h %>% 
  pivot_longer(
    cols = starts_with("anxdepr."),
    names_prefix = "anxdepr.",
    names_to = "fupday",
    values_to = "anxdepr"
  )

# Merge together
d <- merge(d, i, by=c("Participant.Id", "fupday"), all = TRUE)
d <- merge(d, f, by=c("Participant.Id", "fupday"), all = TRUE)
d <- merge(d, g, by=c("Participant.Id", "fupday"), all = TRUE)
d <- merge(d, h, by=c("Participant.Id", "fupday"), all = TRUE)

questLong <- merge(questLong, d, by=c("Participant.Id", "fupday"), all = TRUE)

# input studyfeed
pp <- p[,c("Participant.Id", "studyfeed")]

questLong <- merge(questLong, pp, by = "Participant.Id", all = TRUE)
questLong <- questLong[,c(1, 11, 2:10)]

# convert to numeric
questLong$fupday <- as.numeric(questLong$fupday)

# Remove surplus objects ----
rm(list=ls()[! ls() %in% c("p","re","questLong","t","o", "pn", "med", "h","i","sf")])

### End of file. ###
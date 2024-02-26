###############################################################################|
### PRECISe Figure 1 - Patient Flowchart
### Julia Bels, May 2023
### R version 4.3.1
###
### This script is meant for the synthesis of Figure 1. Patient Flowchart.
### The flowchart itself is made via https://lucid.app/lucidchart/f962cd53-9d57-425b-8966-3e441c44bfc6/edit?invitationId=inv_c6da9e67-ca14-4b85-bee9-6d17b29db199&page=0_0#
###
###############################################################################|


# 1. ENROLLMENT phase  ----

#import screening data
s <- read.csv("~/PRECISe/Screening lijsten/PRECISe_screeninglogs_combined_ALL_SITES_FINAL.csv",sep=";")

#import object
load("PRECISeobjects_V1.0.RData")

## 1.1 Input data into ENROLLMENT phase of flowchart  ----

#Show values and corresponding N for "ENROLLMENT" phase in flowchart
paste("Number of patients 'Assessed for eligibility': ", nrow(s))

table(s$ELIG_YN)        #input "no" into "Excluded" 
table(s$ELIG_REASON)    #input subtotals into text below "Excluded"

table(s$ENROL_YN)       #input into "No" into "Eligible but randomised"
table(s$ENROL_REASON)   #input subtotals into text below "Eligible but not randomised"

# 2. ALLOCATION phase  ----

## 2.1 Check who did not receive any study nutrition  ----

#make sum of calories
intake <- t %>%
  group_by(Participant.Id) %>% 
  summarise(num = n(),
            totalintake = sum(NUTR_VOL, na.rm = TRUE))

#check who has "user missing" for study nutrition (they are NOT in received_intervention)
received_intervention <-intersect(intake$Participant.Id,p$Participant.Id)

#check who has 0 for study nutrition (they ARE in received_0)
received_0 <- intake[intake$totalintake == "0",]
received_0 <- intersect(received_0$Participant.Id,p$Participant.Id)

#make new variable RECEIVED_TRT -> did not receive intervention OR did received intervention
p$RECEIVED_TRT <- ifelse(!(p$Participant.Id %in% received_intervention) | p$Participant.Id %in% received_0, "Did not receive intervention", "Did receive intervention")

#make new variable RECEIVED_TRT_OPT & check reason with data and comments in Castor

#   04-PRECISe-102 --> TERM_REASOPT = 3 - death
#   01-PRECISe-100 --> TERM_REASOPT = 3 - death
#   02-PRECISe-197 --> TERM_REASOPT = 7 - consent withdrawal
#   04-PRECISe-008 --> TERM_REASOPT = 7 - consent withdrawal
#   05-PRECISe-032 --> TERM_REASOPT = 7 - consent withdrawal
#   03-PRECISe-001 --> TERM_REASOPT = 7 - consent withdrawal
#   02-PRECISe-026 --> TERM_REASOPT = 4 - resumption full oral intake (quick extubation)
#   02-PRECISe-229 --> TERM_REASOPT = 4 - resumption full oral intake (quick extubation)
#   04-PRECISe-004 --> TERM_REASOPT = 5 - decision by PI not to start intervention
#   08-PRECISe-025 --> TERM_REASOPT = NA - started with PN & thereafter decision by PI to start non-study EN
#   010-PRECISe-010 --> TERM_REASOPT = NA - resumption full oral intake (quick extubation)

p$RECEIVED_TRT_OPT <- ifelse(p$RECEIVED_TRT %in% "Did not receive intervention" & p$TERM_REASOPT %in% "3", "Death", 
                      ifelse(p$RECEIVED_TRT %in% "Did not receive intervention" & p$TERM_REASOPT %in% "7", "Withdrawal of consent",
                      ifelse(p$RECEIVED_TRT %in% "Did not receive intervention" & p$TERM_REASOPT %in% "4", "Resumption of full oral intake",
                      ifelse(p$RECEIVED_TRT %in% "Did not receive intervention" & p$TERM_REASOPT %in% "5", "Medical decision", NA))))

p$RECEIVED_TRT_OPT <- ifelse(p$Participant.Id %in% "08-PRECISe-025", "Medical decision",
                      ifelse(p$Participant.Id %in% "010-PRECISe-010", "Resumption of full oral intake", p$RECEIVED_TRT_OPT))

## 2.2 Input data into ALLOCATION phase of flowchart  ----

#Show values and corresponding N for "ENROLLMENT" phase in flowchart

table(p$studyfeed)                        #input into "Allocated to Group 1/Group 2 (adjust after deblinding)"

table(p$RECEIVED_TRT, p$studyfeed)        #input into "Did not receive allocated intervention" per group
table(p$RECEIVED_TRT_OPT, p$studyfeed)    #input subtotals into applicable reasons for non-adherence

# 3. FOLLOW-UP phase  ----

## 3.1 Check who did not have any EQ-5D-5L during follow-up  ----
f <- p[,c("Participant.Id","EQ5D.HUS.imp0.30", "EQ5D.HUS.imp0.90","EQ5D.HUS.imp0.180")]

#Check for which patients no primary endpoint has been collected
f <- f[!with(f,is.na(EQ5D.HUS.imp0.30)& is.na(EQ5D.HUS.imp0.90)& is.na(EQ5D.HUS.imp0.180)),]

hasEQ5D <-intersect(f$Participant.Id,p$Participant.Id)

p$EQ5Dcollected <- ifelse(!(p$Participant.Id %in% hasEQ5D), "LTFU", "EQ5D collected")

table(p$EQ5Dcollected, useNA = "always")

#Make variable that identifies participants excluded from primary outcome analysis
p$EXCLUDED <- ifelse(p$EQ5Dcollected == "LTFU", 1, 0)

#make new variable to analyse reason for LTFU: consent withdrawal OR Survived but not contactable
p$EQ5Dcollected_reason <- ifelse(p$EQ5Dcollected == "LTFU" & p$TERM_REASOPT == "7" | 
                                 p$EQ5Dcollected == "LTFU" & p$TERM_REASOPT == "4", "Consent withdrawal",
                          ifelse(p$EQ5Dcollected == "LTFU" & p$TERM_REASOPT == "1" |
                                 p$EQ5Dcollected == "LTFU" & p$TERM_REASOPT == "5" |
                                 p$EQ5Dcollected == "LTFU" & p$TERM_REASOPT == "6" , "Survived but not contactable", NA))
p$EQ5Dcollected_reason <- ifelse(p$EQ5Dcollected == "LTFU" & p$TERM_OPT == "0", "Survived but not contactable", p$EQ5Dcollected_reason)

## 3.2 Input data into FOLLOW-UP phase of flowchart  ----
table(p$EQ5Dcollected, p$studyfeed)         #input 0s into "Lost to follow-up" 

table(p$EQ5Dcollected_reason, p$studyfeed)  #input subtotals into applicable reasons for LTFU (1= consent withdrawal; 2= unable to contact participant)

#Check ratio of patient vs proxy consent withdrawals
pp <- p[p$EQ5Dcollected_reason %in% "Consent withdrawal",]
table(pp$TERM_REASOPT, pp$studyfeed)        #input 4's as patient withdrawal, 7's as proxy withdrawal

# 4. ANALYSIS phase  ----

table(p$EQ5Dcollected, p$studyfeed)         #input 1s into "Analysed for primary outcome" 

# 5. Other summaries  ----

# Overall: number of days recruiting
s$Date <- as.Date(s$Date, format = "%d-%m-%Y")

min(s$Date, na.rm = TRUE)
max(s$Date, na.rm = TRUE)

difftime(max(s$Date, na.rm = TRUE), min(s$Date, na.rm = TRUE), units = "days") + 1 # Input response into "overall number of days recruiting"

# Per site: number of days recruiting 
test <- s %>%
  group_by(Site) %>%    
  slice_min(Date)
test <- test[!duplicated(test$Site),]

test2 <- s %>%
     group_by(Site) %>%
     slice_max(Date)
test2 <- test2[!duplicated(test2$Site),]

testt <- merge(test, test2, by=c("Site"), all = TRUE)
testt$difftime <- difftime(testt$Date.y, testt$Date.x, units = "days") + 1

recruitpersite <- testt[,c(1, 16)] 

# Per site: number of patients recruited

test <- s %>%
  group_by(Site) %>%
  count(ENROL_YN == "yes")

colnames(test)

test <- test[test$`ENROL_YN == "yes"` == TRUE,]
test <- test[,c(1, 3)]
recruitpersite <- merge(test, recruitpersite, by=c("Site"), all = TRUE)

recruitpersiteoverall = data.frame(Site = c("Overall"), 
                            n = c(935),
                            difftime = c(877))

recruitpersite <- rbind(recruitpersite, recruitpersiteoverall)


colnames(recruitpersite) <- c("Site", "Number of patients recruited", "Number of days actively recruiting")
recruitpersite$Site <- ifelse(recruitpersite$Site == 1, "Site 01 - MUMC (Maastricht, the Netherlands)",
                       ifelse(recruitpersite$Site == 2, "Site 02 - ZOL (Genk, Belgium)",
                       ifelse(recruitpersite$Site == 3, "Site 03 - Gelderse Vallei (Ede, the Netherlands)",
                       ifelse(recruitpersite$Site == 4, "Site 04 - MS Twente (Enschede, the Netherlands)",
                       ifelse(recruitpersite$Site == 5, "Site 05 - Zuyderland MC (Heerlen, the Netherlands)",
                       ifelse(recruitpersite$Site == 6, "Site 06 - CHU Liège (Liège, Belgium)",
                       ifelse(recruitpersite$Site == 7, "Site 07 - CHR Liège (Liège, Belgium)",
                       ifelse(recruitpersite$Site == 8, "Site 08 - UZ Brussel (Brussels, Belgium)",
                       ifelse(recruitpersite$Site == 9, "Site 09 - AZ Groeninge (Kortrijk, Belgium)",
                       ifelse(recruitpersite$Site == 10, "Site 10 - Catharina (Eindhoven, the Netherlands)", recruitpersite$Site))))))))))

write_xlsx(recruitpersite,"recruitpersite.xlsx")

# Remove surplus objects ----
rm(list=ls()[! ls() %in% c("p","questLong","t", "i")])

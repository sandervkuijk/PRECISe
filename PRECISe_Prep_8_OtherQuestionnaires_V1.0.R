###############################################################################|
### PRECISe Prep for outcomes of questionnaires
### Julia Bels, January 2024
### R version 4.3.1
###
### This script is meant for calculation of the SF-36, HADS, and IES-R 
### questionnaires (i.e. secondary outcomes).
###
###############################################################################|

# 1. HADS ----
## 1.1 Import of relevant datasets ----
h <- read.csv("~/PRECISe/PRECISe_FINAL_ANALYSES/data/raw/PRECISe_study_HADS_-_NL_export_20240115.csv", 
              sep=";", na.strings = list("", -99, -98, -97, -96, -95, "01-01-2999", "01-01-2996"))
h <- h[,c("Castor.Participant.ID","Survey.Package.Name","HADS_NL_Q1OPT","HADS_NL_Q2OPT",
          "HADS_NL_Q3OPT","HADS_NL_Q4OPT","HADS_NL_Q5OPT","HADS_NL_Q6OPT","HADS_NL_Q7OPT",
          "HADS_NL_Q8OPT","HADS_NL_Q9OPT","HADS_NL_Q10OPT","HADS_NL_Q11OPT","HADS_NL_Q12OPT",
          "HADS_NL_Q13OPT","HADS_NL_Q14OPT")]   

h2 <- read.csv("~/PRECISe/PRECISe_FINAL_ANALYSES/data/raw/PRECISe_study_HADS_-_FR_export_20240115.csv", 
               sep=";", na.strings = list("", -99, -98, -97, -96, -95, "01-01-2999", "01-01-2996"))
h2 <- h2[,c("Castor.Participant.ID","Survey.Package.Name", "HADS_FR_Q1OPT","HADS_FR_Q2OPT",
            "HADS_FR_Q3OPT","HADS_FR_Q4OPT","HADS_FR_Q5OPT","HADS_FR_Q6OPT","HADS_FR_Q7OPT",
            "HADS_FR_Q8OPT","HADS_FR_Q9OPT","HADS_FR_Q10OPT","HADS_FR_Q11OPT",
            "HADS_FR_Q12OPT","HADS_FR_Q13OPT","HADS_FR_Q14OPT")]
  
## 1.2 Rename items, to correspond to scoring system ----
colnames(h) <- c("Participant.Id","Survey.Package.Name","q1", "q2", "q3", "q4", "q5",
                 "q6", "q7", "q8", "q9", "q10", "q11", "q12", "q13", "q14")
colnames(h2) <- c("Participant.Id","Survey.Package.Name","q1", "q2", "q3", "q4", "q5",
                  "q6", "q7", "q8", "q9", "q10", "q11", "q12", "q13", "q14")

## 1.3 Combine datasets ----
h <- rbind(h, h2)

## 1.4 Determine follow-up date ----
h$fupday <- ifelse(h$Survey.Package.Name == "Follow-up 30 - Questionnaires - BE (FR)" | h$Survey.Package.Name == "Follow-up 30 - Vragenlijsten - BE (NL)" | h$Survey.Package.Name == "Follow-up 30 - Vragenlijsten - NL (NL)", 30,
            ifelse(h$Survey.Package.Name == "Follow-up 90 - Questionnaires - BE (FR)" | h$Survey.Package.Name == "Follow-up 90 - Vragenlijsten - BE (NL)" | h$Survey.Package.Name == "Follow-up 90 - Vragenlijsten - NL (NL)", 90,
            ifelse(h$Survey.Package.Name == "Follow-up 180 - Questionnaires - BE (FR)" | h$Survey.Package.Name == "Follow-up 180 - Vragenlijsten - BE (NL)" | h$Survey.Package.Name == "Follow-up 180 - Vragenlijsten - NL (NL)", 180, NA)))

## 1.5 Exclude records that only have NAs ----
vars <- h[,c("q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12",
              "q13","q14")]
h$missing <- rowSums(vars, na.rm = TRUE) * NA ^ (rowSums(!is.na(vars)) == 0) #only gives NA if ALL columns are NA, otherwise: partial score
h <- h[!with(h,is.na(missing)),] #only select records where h$missing is not NA

## 1.6 Recode answer options ----
# Uneven questions (1, 3, 5, 7, 9,11 and 13) relate to anxiety & are scored 3-2-1-0
# Even questions ( 2, 4, 6, 8, 10, 12 and 14) relate to depression & are scored 0-1-2-3

h$q1 <- ifelse(h$q1 %in% 0, 3,
               ifelse(h$q1 %in% 1, 2,
                      ifelse(h$q1 %in% 2, 1,
                             ifelse(h$q1 %in% 3, 0, NA))))
h$q3 <- ifelse(h$q3 %in% 0, 3,
               ifelse(h$q3 %in% 1, 2,
                      ifelse(h$q3 %in% 2, 1,
                             ifelse(h$q3 %in% 3, 0, NA))))
h$q5 <- ifelse(h$q5 %in% 0, 3,
               ifelse(h$q5 %in% 1, 2,
                      ifelse(h$q5 %in% 2, 1,
                             ifelse(h$q5 %in% 3, 0, NA))))
h$q7 <- ifelse(h$q7 %in% 0, 3,
               ifelse(h$q7 %in% 1, 2,
                      ifelse(h$q7 %in% 2, 1,
                             ifelse(h$q7 %in% 3, 0, NA))))
h$q9 <- ifelse(h$q9 %in% 0, 3,
               ifelse(h$q9 %in% 1, 2,
                      ifelse(h$q9 %in% 2, 1,
                             ifelse(h$q9 %in% 3, 0, NA))))
h$q11 <- ifelse(h$q11 %in% 0, 3,
               ifelse(h$q11 %in% 1, 2,
                      ifelse(h$q11 %in% 2, 1,
                             ifelse(h$q11 %in% 3, 0, NA))))
h$q13 <- ifelse(h$q13 %in% 0, 3,
               ifelse(h$q13 %in% 1, 2,
                      ifelse(h$q13 %in% 2, 1,
                             ifelse(h$q13 %in% 3, 0, NA))))

## 1.7 Calculate subscores & total score ----

### 1.7.1 Subscores   ----
vars <- h[,c("q1","q3","q5","q7","q9","q11","q13")]
h$HADS.anx <- rowSums(vars, na.rm = FALSE)

vars <- h[,c("q2","q4","q6","q8","q10","q12","q14")]
h$HADS.depr <- rowSums(vars, na.rm = FALSE)

### 1.7.2 Total score   ----
h$HADS.total <- h$HADS.anx + h$HADS.depr

## 1.8 Visualisation ----
plot <- ggplot(h, aes(x=as.factor(fupday), y=HADS.total)) + 
  geom_boxplot(aes(x=as.factor(fupday), y=HADS.total), fill = "orange" , alpha = 0.25, outlier.shape = NA) +
  xlab("Day since randomisation") +
  ylab("HADS score") + 
  scale_fill_brewer(palette="Set2") +
  stat_summary(fun.y=mean, geom="point", shape=5, size=4, color="black", fill="black") +
  theme_minimal()

## 1.9 Combine with main wide and long objects, "p" and "questLong" respectively ----

#Combine with questLong
h <- h[,c("Participant.Id","fupday","HADS.anx","HADS.depr", "HADS.total")]
questLong <- merge(questLong, h, by=c("Participant.Id", "fupday"), all = TRUE)

#Combine with p
hwide <- reshape(h, 
                 timevar = "fupday",
                 idvar = c("Participant.Id"),
                 direction = "wide")
hwide <- hwide[,c("Participant.Id",
                  "HADS.anx.30","HADS.depr.30","HADS.total.30",
                  "HADS.anx.90","HADS.depr.90","HADS.total.90",
                  "HADS.anx.180","HADS.depr.180","HADS.total.180")]
p <- merge(p, hwide, by=c("Participant.Id"), all = TRUE)

# 2. IES-R ----
## 2.1 Import of relevant datasets ----
#Read csv
i1 <- read.csv("~/PRECISe/PRECISe_FINAL_ANALYSES/data/raw/PRECISe_study_IES-R_-_NL_export_20240115.csv", 
              sep=";", na.strings = list("", -99, -98, -97, -96, -95, "01-01-2999", "01-01-2996"))
i1 <- i1[,c("Castor.Participant.ID", "Survey.Package.Name", "IESR_NL_Q1OPT","IESR_NL_Q2OPT","IESR_NL_Q3OPT",
            "IESR_NL_Q4OPT","IESR_NL_Q5OPT","IESR_NL_Q6OPT","IESR_NL_Q7OPT","IESR_NL_Q8OPT","IESR_NL_Q9OPT",
            "IESR_NL_Q10OPT","IESR_NL_Q11OPT","IESR_NL_Q12OPT","IESR_NL_Q13OPT","IESR_NL_Q14OPT","IESR_NL_Q15OPT",
            "IESR_NL_Q16OPT","IESR_NL_Q17OPT","IESR_NL_Q18OPT","IESR_NL_Q19OPT","IESR_NL_Q20OPT","IESR_NL_Q21OPT",
            "IESR_NL_Q22OPT" )]

i2 <- read.csv("~/PRECISe/PRECISe_FINAL_ANALYSES/data/raw/PRECISe_study_IES-R_-_FR_export_20240115.csv", 
               sep=";", na.strings = list("", -99, -98, -97, -96, -95, "01-01-2999", "01-01-2996"))
i2 <- i2[,c("Castor.Participant.ID", "Survey.Package.Name", "IESR_FR_Q1OPT","IESR_FR_Q2OPT","IESR_FR_Q3OPT",
            "IESR_FR_Q4OPT","IESR_FR_Q5OPT","IESR_FR_Q6OPT","IESR_FR_Q7OPT","IESR_FR_Q8OPT","IESR_FR_Q9OPT",
            "IESR_FR_Q10OPT","IESR_FR_Q11OPT","IESR_FR_Q12OPT","IESR_FR_Q13OPT","IESR_FR_Q14OPT","IESR_FR_Q15OPT",
            "IESR_FR_Q16OPT","IESR_FR_Q17OPT","IESR_FR_Q18OPT","IESR_FR_Q19OPT","IESR_FR_Q20OPT","IESR_FR_Q21OPT",
            "IESR_FR_Q22OPT" )]

## 2.2 Rename items, to combine ----
colnames(i1) <- c("Participant.Id", "Survey.Package.Name", "q1","q2","q3","q4","q5","q6","q7","q8",
                      "q9","q10","q11","q12","q13","q14","q15","q16","q17","q18","q19","q20","q21","q22")
colnames(i2) <- c("Participant.Id", "Survey.Package.Name", "q1","q2","q3","q4","q5","q6","q7","q8",
                  "q9","q10","q11","q12","q13","q14","q15","q16","q17","q18","q19","q20","q21","q22")

## 2.3 Combine datasets ----
ies <- rbind(i1, i2)

## 2.4 Determine follow-up moment ----
ies$fupday <- ifelse(ies$Survey.Package.Name == "Follow-up 30 - Vragenlijsten - NL (NL)" | ies$Survey.Package.Name == "Follow-up 30 - Vragenlijsten - BE (NL)" | ies$Survey.Package.Name == "Follow-up 30 - Questionnaires - BE (FR)", 30,
              ifelse(ies$Survey.Package.Name == "Follow-up 90 - Vragenlijsten - NL (NL)" | ies$Survey.Package.Name == "Follow-up 90 - Vragenlijsten - BE (NL)" | ies$Survey.Package.Name == "Follow-up 90 - Questionnaires - BE (FR)", 90,
              ifelse(ies$Survey.Package.Name == "Follow-up 180 - Vragenlijsten - NL (NL)" | ies$Survey.Package.Name == "Follow-up 180 - Vragenlijsten - BE (NL)" | ies$Survey.Package.Name == "Follow-up 180 - Questionnaires - BE (FR)", 180, NA)))

## 2.5 Exclude records that only have NAs ----
vars <- ies[, c("q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12",
              "q13","q14","q15","q16","q17","q18","q19","q20","q21","q22")]
ies$missing <- rowSums(vars, na.rm = TRUE) * NA ^ (rowSums(!is.na(vars)) == 0) #only gives NA if ALL columns are NA, otherwise: partial score
ies <- ies[!with(ies,is.na(missing)),] #only select records where ies$missing is not NA

## 2.6 Calculate score ----
vars <- ies[, c("q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12",
              "q13","q14","q15","q16","q17","q18","q19","q20","q21","q22")]
ies$IESR_score <- rowSums(vars, na.rm = FALSE) 

## 2.7 Visualisation ----
plot <- ggplot(ies, aes(x=as.factor(fupday), y=IESR_score)) + 
  geom_boxplot(aes(x=as.factor(fupday), y=IESR_score), fill = "orange" , alpha = 0.25, outlier.shape = NA) +
  scale_y_continuous(limits = c(0,60),
                     breaks = c(0,25,50))+
  xlab("Day since randomisation") +
  ylab("IES-R score") + 
  scale_fill_brewer(palette="Set2") +
  stat_summary(fun.y=mean, geom="point", shape=5, size=4, color="black", fill="black") +
  theme_minimal()

## 2.8 Combine with main wide and long objects, "p" and "questLong" respectively ----

#Combine with questLong
ies <- ies[,c("Participant.Id","fupday","IESR_score")]
questLong <- merge(questLong, ies, by=c("Participant.Id", "fupday"), all = TRUE)

#Combine with p
ieswide <- reshape(ies, 
                 timevar = "fupday",
                 idvar = c("Participant.Id"),
                 direction = "wide")
ieswide <- ieswide[,c("Participant.Id","IESR_score.30","IESR_score.90","IESR_score.180")]
p <- merge(p, ieswide, by=c("Participant.Id"), all = TRUE)

# 3. SF-36 ----
# Source used for scoring: https://www.rand.org/health-care/surveys_tools/mos/36-item-short-form/scoring.html
# Our answer options were coded differently (0 to 4 instead of 1 to 5)

## 3.1 Import of relevant datasets ----
# Dutch sites
sf <- read.csv("~/PRECISe/PRECISe_FINAL_ANALYSES/data/raw/PRECISe_study_SF36_-_NL_export_20240115.csv", 
               sep=";", na.strings = list("", -99, -98, -97, -96, -95, "01-01-2999", "01-01-2996"))
sf <- sf[,c(2, 9, 11:46)]

#French-speaking sites
sff <- read.csv("~/PRECISe/PRECISe_FINAL_ANALYSES/data/raw/PRECISe_study_SF36_-_FR_export_20240115.csv", 
                sep=";", na.strings = list("", -99, -98, -97, -96, -95, "01-01-2999", "01-01-2996"))
sff <- sff[,c(2, 9, 11:46)]

## 3.2 Rename items, to correspond to scoring system ----
colnames(sf) <- c("Participant.Id","Survey.Package.Name","q1","q2","q3","q4","q5","q6","q7","q8",
                  "q9","q10","q11","q12","q13","q14","q15","q16","q17","q18","q19","q20","q21","q22","q23",
                  "q24","q25","q26","q27","q28","q29","q30","q31","q32","q33","q34","q35","q36")
colnames(sff) <- c("Participant.Id","Survey.Package.Name","q1","q2","q3","q4","q5","q6","q7","q8",
                  "q9","q10","q11","q12","q13","q14","q15","q16","q17","q18","q19","q20","q21","q22","q23",
                  "q24","q25","q26","q27","q28","q29","q30","q31","q32","q33","q34","q35","q36")

## 3.3 Combine datasets ----
sf <- rbind(sf, sff)

## 3.4 Determine follow-up date ----
sf$fupday <- ifelse(sf$Survey.Package.Name == "Follow-up 30 - Vragenlijsten - BE (NL)" | sf$Survey.Package.Name == "Follow-up 30 - Vragenlijsten - NL (NL)" | sf$Survey.Package.Name == "Follow-up 30 - Questionnaires - BE (FR)", 30,
             ifelse(sf$Survey.Package.Name == "Follow-up 90 - Vragenlijsten - BE (NL)" | sf$Survey.Package.Name == "Follow-up 90 - Vragenlijsten - NL (NL)"| sf$Survey.Package.Name == "Follow-up 90 - Questionnaires - BE (FR)", 90,
             ifelse(sf$Survey.Package.Name == "Follow-up 180 - Vragenlijsten - BE (NL)" | sf$Survey.Package.Name == "Follow-up 180 - Vragenlijsten - NL (NL)" | sf$Survey.Package.Name == "Follow-up 180 - Questionnaires - BE (FR)", 180, NA)))

## 3.5 Exclude records that only have NAs ----
vars <- sf[,c("q1","q2","q3","q4","q5","q6","q7","q8","q9","q10","q11","q12",
              "q13","q14","q15","q16","q17","q18","q19","q20","q21","q22","q23",
             "q24","q25","q26","q27","q28","q29","q30","q31","q32","q33","q34",
             "q35","q36")]
sf$missing <- rowSums(vars, na.rm = TRUE) * NA ^ (rowSums(!is.na(vars)) == 0) #only gives NA if ALL columns are NA, otherwise: partial score
sf <- sf[!with(sf,is.na(missing)),] #only select records where sf$missing is not NA

## 3.6 Recode answer options ----

#  Items 1, 2, 20, 22, 34, 36 | 0 →	100, 1 →	75, 2 →	50, 3 →	25, 4 →	0
sf$q1 <- ifelse(sf$q1 %in% 0, 100,
                ifelse(sf$q1 %in% 1, 75,
                       ifelse(sf$q1 %in% 2, 50,
                              ifelse(sf$q1 %in% 3, 25,
                                     ifelse(sf$q1 %in% 4, 0, NA)))))
sf$q2 <- ifelse(sf$q2 %in% 0, 100,
                ifelse(sf$q2 %in% 1, 75,
                       ifelse(sf$q2 %in% 2, 50,
                              ifelse(sf$q2 %in% 3, 25,
                                     ifelse(sf$q2 %in% 4, 0, NA)))))
sf$q20 <- ifelse(sf$q20 %in% 0, 100,
                ifelse(sf$q20 %in% 1, 75,
                       ifelse(sf$q20 %in% 2, 50,
                              ifelse(sf$q20 %in% 3, 25,
                                     ifelse(sf$q20 %in% 4, 0, NA)))))
sf$q22 <- ifelse(sf$q22 %in% 0, 100,
                ifelse(sf$q22 %in% 1, 75,
                       ifelse(sf$q22 %in% 2, 50,
                              ifelse(sf$q22 %in% 3, 25,
                                     ifelse(sf$q22 %in% 4, 0, NA)))))
sf$q34 <- ifelse(sf$q34 %in% 0, 100,
                ifelse(sf$q34 %in% 1, 75,
                       ifelse(sf$q34 %in% 2, 50,
                              ifelse(sf$q34 %in% 3, 25,
                                     ifelse(sf$q34 %in% 4, 0, NA)))))
sf$q36 <- ifelse(sf$q36 %in% 0, 100,
                ifelse(sf$q36 %in% 1, 75,
                       ifelse(sf$q36 %in% 2, 50,
                              ifelse(sf$q36 %in% 3, 25,
                                     ifelse(sf$q36 %in% 4, 0, NA)))))

# Items 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 | 0 →	0, 1 →	50, 2 →	100
sf$q3 <- ifelse(sf$q3 %in% 0, 0,
                ifelse(sf$q3 %in% 1, 50,
                       ifelse(sf$q3 %in% 2, 100, NA)))
sf$q4 <- ifelse(sf$q4 %in% 0, 0,
                ifelse(sf$q4 %in% 1, 50,
                       ifelse(sf$q4 %in% 2, 100, NA)))
sf$q5 <- ifelse(sf$q5 %in% 0, 0,
                ifelse(sf$q5 %in% 1, 50,
                       ifelse(sf$q5 %in% 2, 100, NA)))
sf$q6 <- ifelse(sf$q6 %in% 0, 0,
                ifelse(sf$q6 %in% 1, 50,
                       ifelse(sf$q6 %in% 2, 100, NA)))
sf$q7 <- ifelse(sf$q7 %in% 0, 0,
                ifelse(sf$q7 %in% 1, 50,
                       ifelse(sf$q7 %in% 2, 100, NA)))
sf$q8 <- ifelse(sf$q8 %in% 0, 0,
                ifelse(sf$q8 %in% 1, 50,
                       ifelse(sf$q8 %in% 2, 100, NA)))
sf$q9 <- ifelse(sf$q9 %in% 0, 0,
                ifelse(sf$q9 %in% 1, 50,
                       ifelse(sf$q9 %in% 2, 100, NA)))
sf$q10 <- ifelse(sf$q10 %in% 0, 0,
                ifelse(sf$q10 %in% 1, 50,
                       ifelse(sf$q10 %in% 2, 100, NA)))
sf$q11 <- ifelse(sf$q11 %in% 0, 0,
                ifelse(sf$q11 %in% 1, 50,
                       ifelse(sf$q11 %in% 2, 100, NA)))
sf$q12 <- ifelse(sf$q12 %in% 0, 0,
                ifelse(sf$q12 %in% 1, 50,
                       ifelse(sf$q12 %in% 2, 100, NA)))

# Items 13, 14, 15, 16, 17, 18, 19 | 0 →	0, 1 →	100
sf$q13 <- ifelse(sf$q13 %in% 0, 0,
                 ifelse(sf$q13 %in% 1, 100, NA))
sf$q14 <- ifelse(sf$q14 %in% 0, 0,
                 ifelse(sf$q14 %in% 1, 100, NA))
sf$q15 <- ifelse(sf$q15 %in% 0, 0,
                 ifelse(sf$q15 %in% 1, 100, NA))
sf$q16 <- ifelse(sf$q16 %in% 0, 0,
                 ifelse(sf$q16 %in% 1, 100, NA))
sf$q17 <- ifelse(sf$q17 %in% 0, 0,
                 ifelse(sf$q17 %in% 1, 100, NA))
sf$q18 <- ifelse(sf$q18 %in% 0, 0,
                 ifelse(sf$q18 %in% 1, 100, NA))
sf$q19 <- ifelse(sf$q19 %in% 0, 0,
                 ifelse(sf$q19 %in% 1, 100, NA))

# Items 21, 23, 26, 27, 30 | 0 →	100, 1 →	80, 2 →	60, 3 →	40, 4 →	20, 5 →	0
sf$q21 <- ifelse(sf$q21 %in% 0, 100,
                 ifelse(sf$q21 %in% 1, 80, 
                        ifelse(sf$q21 %in% 2, 60,
                               ifelse(sf$q21 %in% 3, 40,
                                      ifelse(sf$q21 %in% 4, 20,
                                             ifelse(sf$q21 %in% 5, 0, NA))))))
sf$q23 <- ifelse(sf$q23 %in% 0, 100,
                 ifelse(sf$q23 %in% 1, 80, 
                        ifelse(sf$q23 %in% 2, 60,
                               ifelse(sf$q23 %in% 3, 40,
                                      ifelse(sf$q23 %in% 4, 20,
                                             ifelse(sf$q23 %in% 5, 0, NA))))))
sf$q26 <- ifelse(sf$q26 %in% 0, 100,
                 ifelse(sf$q26 %in% 1, 80, 
                        ifelse(sf$q26 %in% 2, 60,
                               ifelse(sf$q26 %in% 3, 40,
                                      ifelse(sf$q26 %in% 4, 20,
                                             ifelse(sf$q26 %in% 5, 0, NA))))))
sf$q27 <- ifelse(sf$q27 %in% 0, 100,
                 ifelse(sf$q27 %in% 1, 80, 
                        ifelse(sf$q27 %in% 2, 60,
                               ifelse(sf$q27 %in% 3, 40,
                                      ifelse(sf$q27 %in% 4, 20,
                                             ifelse(sf$q27 %in% 5, 0, NA))))))
sf$q30 <- ifelse(sf$q30 %in% 0, 100,
                 ifelse(sf$q30 %in% 1, 80, 
                        ifelse(sf$q30 %in% 2, 60,
                               ifelse(sf$q30 %in% 3, 40,
                                      ifelse(sf$q30 %in% 4, 20,
                                             ifelse(sf$q30 %in% 5, 0, NA))))))

# Items 24, 25, 28, 29, 31 | 0 →	0, 1 →	20, 2 →	40, 3 →	60, 4 →	80, 5 →	100
sf$q24 <- ifelse(sf$q24 %in% 0, 0,
                 ifelse(sf$q24 %in% 1, 20, 
                        ifelse(sf$q24 %in% 2, 40,
                               ifelse(sf$q24 %in% 3, 60,
                                      ifelse(sf$q24 %in% 4, 80,
                                             ifelse(sf$q24 %in% 5, 100, NA))))))
sf$q25 <- ifelse(sf$q25 %in% 0, 0,
                 ifelse(sf$q25 %in% 1, 20, 
                        ifelse(sf$q25 %in% 2, 40,
                               ifelse(sf$q25 %in% 3, 60,
                                      ifelse(sf$q25 %in% 4, 80,
                                             ifelse(sf$q25 %in% 5, 100, NA))))))
sf$q28 <- ifelse(sf$q28 %in% 0, 0,
                 ifelse(sf$q28 %in% 1, 20, 
                        ifelse(sf$q28 %in% 2, 40,
                               ifelse(sf$q28 %in% 3, 60,
                                      ifelse(sf$q28 %in% 4, 80,
                                             ifelse(sf$q28 %in% 5, 100, NA))))))
sf$q29 <- ifelse(sf$q29 %in% 0, 0,
                 ifelse(sf$q29 %in% 1, 20, 
                        ifelse(sf$q29 %in% 2, 40,
                               ifelse(sf$q29 %in% 3, 60,
                                      ifelse(sf$q29 %in% 4, 80,
                                             ifelse(sf$q29 %in% 5, 100, NA))))))
sf$q31 <- ifelse(sf$q31 %in% 0, 0,
                 ifelse(sf$q31 %in% 1, 20, 
                        ifelse(sf$q31 %in% 2, 40,
                               ifelse(sf$q31 %in% 3, 60,
                                      ifelse(sf$q31 %in% 4, 80,
                                             ifelse(sf$q31 %in% 5, 100, NA))))))

# Items 32, 33, 35 | 0 →	0, 1 →	25, 2 →	50, 3 →	75, 4 →	100
sf$q32 <- ifelse(sf$q32 %in% 0, 0,
                ifelse(sf$q32 %in% 1, 25,
                       ifelse(sf$q32 %in% 2, 50,
                              ifelse(sf$q32 %in% 3, 75,
                                     ifelse(sf$q32 %in% 4, 100, NA)))))
sf$q33 <- ifelse(sf$q33 %in% 0, 0,
                 ifelse(sf$q33 %in% 1, 25,
                        ifelse(sf$q33 %in% 2, 50,
                               ifelse(sf$q33 %in% 3, 75,
                                      ifelse(sf$q33 %in% 4, 100, NA)))))

sf$q35 <- ifelse(sf$q35 %in% 0, 0,
                 ifelse(sf$q35 %in% 1, 25,
                        ifelse(sf$q35 %in% 2, 50,
                               ifelse(sf$q35 %in% 3, 75,
                                      ifelse(sf$q35 %in% 4, 100, NA)))))

## 3.7 Calculate subscores & total score ----

### 3.7.1 Subscores   ----

# 1 Physical functioning	=	3 4 5 6 7 8 9 10 11 12
# 2 Role limitations due to physical health	=	13 14 15 16
# 3 Role limitations due to emotional problems = 17 18 19
# 4 Energy/fatigue = 23 27 29 31
# 5 Emotional well-being = 24 25 26 28 30
# 6 Social functioning = 20 32
# 7 Pain = 21 22
# 8 General health = 1 33 34 35 36

vars1 <- sf[, c("q3","q4","q5","q6","q7","q8","q9","q10","q11","q12")]
vars2 <- sf[, c("q13","q14","q15","q16")]
vars3 <- sf[, c("q17","q18","q19")]
vars4 <- sf[, c("q23","q27","q29","q31")]
vars5 <- sf[, c("q24","q25","q26","q28","q30")]
vars6 <- sf[, c("q20","q32")]
vars7 <- sf[, c("q21","q22")]
vars8 <- sf[, c("q1","q33","q34","q35","q36")]

#Physical functioning
sf$PF <- rowSums(vars1, na.rm = FALSE) 
sf$PF <- sf$PF / 10
#Role physical health
sf$RP <- rowSums(vars2, na.rm = FALSE) 
sf$RP <- sf$RP / 4
#Role emotional problems
sf$RE <- rowSums(vars3, na.rm = FALSE)
sf$RE <- sf$RE / 3
#Energy/fatigue (vitality)
sf$VT <- rowSums(vars4, na.rm = FALSE) 
sf$VT <- sf$VT / 4
#Emotional well-being (mental health)
sf$MH <- rowSums(vars5, na.rm = FALSE) 
sf$MH <- sf$MH / 5
#Social functioning
sf$SF <- rowSums(vars6, na.rm = FALSE) 
sf$SF <- sf$SF / 2
# Bodily Pain
sf$BP <- rowSums(vars7, na.rm = FALSE) 
sf$BP <- sf$BP / 2
#General health
sf$GH <- rowSums(vars8, na.rm = FALSE) 
sf$GH <- sf$GH / 5

### 3.7.2 Total score   ----
vars <- sf[,c("PF","RP","RE","VT","MH","SF","BP","GH")]
sf$SF36score <- rowSums(vars, na.rm = FALSE) 
sf$SF36score <- sf$SF36score / 8

## 3.8 Visualisation ----
plot <- ggplot(sf, aes(x=as.factor(fupday), y=SF36score)) + 
  geom_boxplot(aes(x=as.factor(fupday), y=SF36score), fill = "orange" , alpha = 0.25, outlier.shape = NA) +
  xlab("Day since randomisation") +
  ylab("SF-36 score") + 
  scale_fill_brewer(palette="Set2") +
  stat_summary(fun.y=mean, geom="point", shape=5, size=4, color="black", fill="black") +
  theme_minimal()

## 3.9 Combine with main wide and long objects, "p" and "questLong" respectively ----

#Combine with questLong
sff <- sf[,c("Participant.Id","fupday","SF36score")]
questLong <- merge(questLong, sff, by=c("Participant.Id", "fupday"), all = TRUE)

#Combine with p
sfwide <- reshape(sff, 
                   timevar = "fupday",
                   idvar = c("Participant.Id"),
                   direction = "wide")
sfwide <- sfwide[,c("Participant.Id","SF36score.30","SF36score.90","SF36score.180")]
p <- merge(p, sfwide, by=c("Participant.Id"), all = TRUE)


# Remove surplus objects ----
rm(list=ls()[! ls() %in% c("p","re","questLong","t","o","pn","med","i", "sf")])

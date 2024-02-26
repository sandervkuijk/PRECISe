###############################################################################|
### PRECISe Table 2 - Outcomes
### Julia Bels, January 2024
###
###############################################################################|

#Load data
load("PRECISeobjects_V1.0.RData")

#load packages
library(gtsummary)
library(finalfit)

# 1. Make table with absolute values and P-values ----

## 1.1 Prepare variables for import into table ----
p$DIS_HOSPOPT <- ifelse(p$DIS_HOSPOPT %in% 1, "Home",
                 ifelse(p$DIS_HOSPOPT %in% 2, "Rehabilitation facility",
                 ifelse(p$DIS_HOSPOPT %in% 3, "Nursing home",
                 ifelse(p$DIS_HOSPOPT %in% 4, "Other hospital",
                 ifelse(p$DIS_HOSPOPT %in% 5, "Other", p$DIS_HOSPOPT)))))
p$hospitalmortality <- as.numeric(ifelse(p$hospitalmortality == "Dead", 1,
                                  ifelse(p$hospitalmortality == "Alive", 0, p$hospitalmortality)))
p$TERM_WORKOPT  <- ifelse(p$TERM_WORKOPT %in% 0, "No (patient did work before ICU admission)",
                   ifelse(p$TERM_WORKOPT %in% 1, "No (patient did not work before ICU admission)",
                   ifelse(p$TERM_WORKOPT %in% 2, "Yes", p$TERM_WORKOPT)))

## 1.2 Make table with p-values ----

TABLE3 <- p %>% select(ICUmortality, hospitalmortality, mortality.D30, mortality.D60, mortality.D90, IMVdurationtotal, ICU_LoS,
             READ_YN, READ_count, prokinetic, prokineticdays, GI_symptoms, EI_OPT.ICU.acquired.infection, ICUtowork, TERM_WORKOPT,
             newAKI, RR_YN, RRTdurationtotal, hepaticdysfunction, SOFA.mean, SOFA.max, mobilizationDays ,hosp_LoS,DIS_HOSPOPT, TERM_REHABYN, TERM_REHAB, studyfeed) %>% 
  tbl_summary(
    by = studyfeed, #change to variable assigned to intervention
    label = list(ICUmortality ~" ICU mortality", hospitalmortality ~ "Hospital mortality",  mortality.D30 ~ "30-day mortality", mortality.D60 ~ "60-day mortality", 
                 mortality.D90 ~ "90-day mortality", IMVdurationtotal  ~ "Duration of invasive mechanical ventilation (days)",
                 ICU_LoS ~ "Length of ICU stay (days)", READ_YN ~ "Readmission to ICU", READ_count ~ "Number of readmissions", 
                 prokinetic ~ "Administration of prokinetic", prokineticdays ~ "Duration of prokinetic administration (days)", 
                 GI_symptoms ~ "Gastrointestinal intolerance or symptoms", EI_OPT.ICU.acquired.infection ~ "ICU-acquired infections",
                 newAKI ~ "Acute kidney injury", RR_YN ~ "Renal replacement therapy", RRTdurationtotal ~"Duration of renal replacement therapy (days)", 
                 hepaticdysfunction ~"Hepatic dysfunction",SOFA.mean ~ "Mean SOFA score during ICU admission", SOFA.max ~ "Maximum SOFA score during ICU admission", 
                 mobilizationDays ~ "Duration of mobilization (days)" , hosp_LoS ~ "Length of hospital stay (days)", DIS_HOSPOPT ~ "Destination of hospital discharge",
                 TERM_REHABYN ~ "Admission to rehabilitation facility", TERM_REHAB ~ "Length of stay at rehabilitation facility (days)",
                 TERM_WORKOPT ~ "Return to work", ICUtowork ~ "Number of days between ICU admission and return to work (days"),
    digits = c(IMVdurationtotal, ICU_LoS, READ_count, SOFA.mean, SOFA.max, prokineticdays, RRTdurationtotal, hosp_LoS, TERM_REHAB,mobilizationDays) ~ 0,
    sort= c(DIS_HOSPOPT, TERM_WORKOPT) ~ "frequency",
    missing_text = "(Substract from total N)",
    type = list(ICUmortality ~"dichotomous", hospitalmortality ~ "dichotomous",  mortality.D30 ~ "dichotomous", mortality.D60 ~ "dichotomous",
                mortality.D90 ~ "dichotomous", IMVdurationtotal ~ "continuous", ICU_LoS ~ "continuous", READ_YN ~ "dichotomous", 
                READ_count ~ "continuous", prokinetic ~ "dichotomous", prokineticdays ~ "continuous",GI_symptoms ~ "dichotomous", 
                EI_OPT.ICU.acquired.infection ~ "dichotomous", newAKI ~ "dichotomous", RR_YN ~ "dichotomous", 
                RRTdurationtotal ~"continuous", hepaticdysfunction ~"dichotomous", SOFA.mean~ "continuous", SOFA.max~ "continuous", 
                mobilizationDays ~"continuous", hosp_LoS ~ "continuous", 
                DIS_HOSPOPT ~ "categorical", TERM_REHABYN ~ "dichotomous", TERM_REHAB ~ "continuous",TERM_WORKOPT ~ "categorical",
                ICUtowork ~ "continuous"),
    statistic = list(
      c(IMVdurationtotal,ICU_LoS,READ_count,prokineticdays,RRTdurationtotal,mobilizationDays,hosp_LoS,TERM_REHAB) ~ c("{median} ({p25}, {p75})"),
      c(SOFA.mean, SOFA.max, ICUtowork) ~ "{mean} ({sd})"),
    value = list(hospitalmortality = "1", mortality.D30 = "1", mortality.D60 = "1", mortality.D90 = "1", READ_YN = "1",
                 prokinetic = "1", GI_symptoms = "1", EI_OPT.ICU.acquired.infection = "1", newAKI = "1", RR_YN = "1",
                 hepaticdysfunction = "1", TERM_REHABYN = "1"),
    include = everything(),)%>% bold_labels() %>% modify_header(label ~ "**Variable**") %>% modify_caption("**Table 2. Clinical outcomes**")

TABLE3 %>% as_flex_table() %>% flextable::save_as_docx(path = "OutcomesTable.docx")


# 2. Determine categorical and continuous outcomes (measured once) using (G)LME ----

## 2.1 Load package & convert relevant variables to factor ----
library(lme4)
library(performance)
library(lmerTest)

## 2.2 Perform analyses on continuous data & check assumptions ----

### 2.2.1 ICU length of stay (continuous) ----
model <- lmer(I(log(ICU_LoS)) ~ studyfeed + (1 | Institute), data = p)

summary(model)
res <- resid(model)
hist(res)
qqnorm(resid(model))
qqline(res) 

#Export coeff and CI boundaries
mm <- as.data.frame(tidy(model,conf.int=TRUE,effects="fixed"))
mm$Variable <- "ICU length of stay (log-transformed)"
mm = mm[-1,]
mm <- mm[,c("Variable", "estimate","conf.low", "conf.high","p.value")]

### 2.2.2 Hospital length of stay (continuous) ----
model <- lmer(I(log(hosp_LoS)) ~ studyfeed + (1 | Institute), data = p)

summary(model)
res <- resid(model)
hist(res)
qqnorm(resid(model))
qqline(res) 

#Export coeff and CI boundaries
coef <- as.data.frame(tidy(model,conf.int=TRUE,effects="fixed"))
coef$Variable <- "Hospital length of stay (log-transformed)"
coef = coef[-1,]
coef <- coef[,c("Variable", "estimate","conf.low", "conf.high","p.value")]

coef <- rbind(mm, coef)


### 2.2.3 Ventilator days (continuous) ----
model <- lmer(I(log(IMVdurationtotal)) ~ studyfeed + (1 | Institute), data = p)

summary(model)
res <- resid(model)
hist(res)
qqnorm(resid(model))
qqline(res) 

#Export coeff and CI boundaries
mm <- as.data.frame(tidy(model,conf.int=TRUE,effects="fixed"))
mm$Variable <- "Duration of invasive mechanical ventilation (log-transformed)"
mm = mm[-1,]
mm <- mm[,c("Variable", "estimate","conf.low", "conf.high","p.value")]

coef <- rbind(mm, coef)

### 2.2.4 Duration of renal replacement therapy (continuous) ----
model <- lmer(I(log(RRTdurationtotal)) ~ studyfeed + (1 | Institute), data = p) #can be simplified
model <- lm(I(log(RRTdurationtotal)) ~ studyfeed, data = p)

summary(model)
res <- resid(model)
hist(res)
qqnorm(resid(model))
qqline(res) 

#Export coeff and CI boundaries
mm <- as.data.frame(tidy(model,conf.int=TRUE,effects="fixed"))
mm$Variable <- "Duration of RRT (log-transformed) - assumptions possibly violated"
mm = mm[-1,]
mm <- mm[,c("Variable", "estimate","conf.low", "conf.high","p.value")]

coef <- rbind(coef, mm)

### 2.2.5 Length of stay at rehabilitation facility (continuous) ----
model <- lmer(TERM_REHAB ~ studyfeed + (1 | Institute), data = p)

summary(model)
res <- resid(model)
hist(res)
qqnorm(resid(model))
qqline(res) 

#Export coeff and CI boundaries
mm <- as.data.frame(tidy(model,conf.int=TRUE,effects="fixed"))
mm$Variable <- "Length of stay at rehabilitation facility"
mm = mm[-1,]
mm <- mm[,c("Variable", "estimate","conf.low", "conf.high","p.value")]

coef <- rbind(coef, mm)

### 2.2.6 Number of days of mobilization (continuous) | HOW TO TRANSOFRM ? ----
model <- lmer(mobilizationDays ~ studyfeed + (1 | Institute), data = p)

summary(model)
res <- resid(model)
hist(res)
qqnorm(resid(model))
qqline(res) 

#Export coeff and CI boundaries
mm <- as.data.frame(tidy(model,conf.int=TRUE,effects="fixed"))
mm$Variable <- "Number of days of mobilization"
mm = mm[-1,]
mm <- mm[,c("Variable", "estimate","conf.low", "conf.high","p.value")]

coef <- rbind(coef, mm)

### 2.2.7 Number of days on prokinetic (continuous) ----
model <- lmer(prokineticdays ~ studyfeed + (1 | Institute), data = p)

summary(model)
res <- resid(model)
hist(res)
qqnorm(resid(model))
qqline(res) 

#Export coeff and CI boundaries
mm <- as.data.frame(tidy(model,conf.int=TRUE,effects="fixed"))
mm$Variable <- "Number of days on prokinetic"
mm = mm[-1,]
mm <- mm[,c("Variable", "estimate","conf.low", "conf.high","p.value")]

coef <- rbind(coef, mm)

### 2.2.8 Number between ICU admission and return to work (continuous) ----
model <- lmer(ICUtowork ~ studyfeed + (1 | Institute), data = p) #can be simplified
model <- lm(ICUtowork ~ studyfeed, data = p)

summary(model)
res <- resid(model)
hist(res)
qqnorm(resid(model))
qqline(res) 

#Export coeff and CI boundaries
mm <- as.data.frame(tidy(model,conf.int=TRUE,effects="fixed"))
mm$Variable <- "Number of days between ICU admission and work"
mm = mm[-1,]
mm <- mm[,c("Variable", "estimate","conf.low", "conf.high","p.value")]

coef <- rbind(coef, mm)

### 2.2.9 Mean SOFA score during first two weeks (continuous) ----
model <- lmer(SOFA.mean ~ studyfeed + (1 | Institute), data = p)

summary(model)
res <- resid(model)
hist(res)
qqnorm(resid(model))
qqline(res) 


#Export coeff and CI boundaries
mm <- as.data.frame(tidy(model,conf.int=TRUE,effects="fixed"))
mm$Variable <- "Mean SOFA score during first 2 weeks of admission"
mm = mm[-1,]
mm <- mm[,c("Variable", "estimate","conf.low", "conf.high","p.value")]

coef <- rbind(coef, mm)

### 2.2.9 Max SOFA score during first two weeks (continuous) ----
model <- lmer(SOFA.max ~ studyfeed + (1 | Institute), data = p)

summary(model)
res <- resid(model)
hist(res)
qqnorm(resid(model))
qqline(res) 

#Export coeff and CI boundaries
mm <- as.data.frame(tidy(model,conf.int=TRUE,effects="fixed"))
mm$Variable <- "Maximum SOFA score during first 2 weeks of admission"
mm = mm[-1,]
mm <- mm[,c("Variable", "estimate","conf.low", "conf.high","p.value")]

coef <- rbind(coef, mm)


### 2.2.10 Number of ICU readmissions (continuous) | totally violated ----
model <- lmer(p$READ_count ~ studyfeed + (1 | Institute), data = p) #can be simplified
model <- lm(p$READ_count ~ studyfeed, data = p)

summary(model)
res <- resid(model)
hist(res)
qqnorm(resid(model))
qqline(res) 

check_model(model)

plot(fitted(model), res)
abline(0,0)




## 2.3 Perform analyses on dichotomous data ----
library(broom.mixed)

p$Institute <- as.factor(p$Institute)
p$hospitalmortality <- as.factor(p$hospitalmortality)
p$ICUmortality <- as.factor(p$ICUmortality)
p$mortality.D30 <- as.factor(p$mortality.D30)
p$mortality.D60 <- as.factor(p$mortality.D60)
p$mortality.D90 <- as.factor(p$mortality.D90)
p$READ_YN <- as.factor(p$READ_YN)
p$GI_symptoms <- as.factor(p$GI_symptoms)
p$EI_OPT.ICU.acquired.infection <- as.factor(p$EI_OPT.ICU.acquired.infection)
p$prokinetic <- as.factor(p$prokinetic)
p$newAKI <- as.factor(p$newAKI)
p$RR_YN <- as.factor(p$RR_YN)
p$hepaticdysfunction <- as.factor(p$hepaticdysfunction)
p$TERM_REHABYN <- as.factor(p$TERM_REHABYN)


### 2.3.1 ICU mortality (dichotomous) ----
model <- glmer(ICUmortality ~ studyfeed + (1 | Institute), data = p, family = "binomial") #can be simplified
model <- glm(ICUmortality ~ studyfeed, data = p, family = "binomial") 

#Export coeff and CI boundaries
mm <- as.data.frame(tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed"))
mm = mm[-1,]
mm$Variable <- "ICU mortality (Odds Ratio)"
mm <- mm[,c(8,2,6:7,5)]

coef <- rbind(coef, mm)

### 2.3.2 Hospital mortality (dichotomous) ----
model <- glmer(hospitalmortality ~ studyfeed + (1 | Institute), data = p, family = "binomial")

#Export coeff and CI boundaries
mm <- as.data.frame(tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed"))
mm = mm[-1,]
mm$Variable <- "Hospital mortality (Odds Ratio)"
mm <- mm[,c(9, 3,7:8, 6)]

coef <- rbind(coef, mm)

### 2.3.3 30-day mortality (dichotomous) ----
model <- glmer(mortality.D30 ~ studyfeed + (1 | Institute), data = p, family = "binomial")
summary(model)

#Export coeff and CI boundaries
mm <- as.data.frame(tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed"))
mm = mm[-1,]
mm$Variable <- "30-day mortality (Odds Ratio)"
mm <- mm[,c(9, 3,7:8, 6)]

coef <- rbind(coef, mm)

### 2.3.4 60-day mortality (dichotomous) ----
model <- glmer(mortality.D60 ~ studyfeed + (1 | Institute), data = p, family = "binomial")
summary(model)

#Export coeff and CI boundaries
mm <- as.data.frame(tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed"))
mm = mm[-1,]
mm$Variable <- "60-day mortality (Odds Ratio)"
mm <- mm[,c(9, 3,7:8, 6)]

coef <- rbind(coef, mm)

### 2.3.5 90-day mortality (dichotomous) ----
model <- glmer(mortality.D90 ~ studyfeed + (1 | Institute), data = p, family = "binomial")
summary(model)

#Export coeff and CI boundaries
mm <- as.data.frame(tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed"))
mm = mm[-1,]
mm$Variable <- "90-day mortality (Odds Ratio)"
mm <- mm[,c(9, 3,7:8, 6)]

coef <- rbind(coef, mm)

### 2.3.6 Incidence of readmissions (dichotomous) ----
model <- glmer(READ_YN ~ studyfeed + (1 | Institute), data = p, family = "binomial")

#Export coeff and CI boundaries
mm <- as.data.frame(tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed"))
mm = mm[-1,]
mm$Variable <- "Incidence of readmissions (Odds Ratio)"
mm <- mm[,c(9, 3,7:8, 6)]

coef <- rbind(coef, mm)

### 2.3.7 Incidence of GI symptoms (dichotomous) ----
model <- glmer(GI_symptoms ~ studyfeed + (1 | Institute), data = p, family = "binomial")

#Export coeff and CI boundaries
mm <- as.data.frame(tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed"))
mm = mm[-1,]
mm$Variable <- "Incidence of GI intolerance (Odds Ratio)"
mm <- mm[,c(9, 3,7:8, 6)]

coef <- rbind(coef, mm)

### 2.3.8 Incidence of ICU-acquired infections (dichotomous) ----
model <- glmer(EI_OPT.ICU.acquired.infection ~ studyfeed + (1 | Institute), data = p, family = "binomial")
summary(model)

#Export coeff and CI boundaries
mm <- as.data.frame(tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed"))
mm = mm[-1,]
mm$Variable <- "Incidence of ICU-acquired infections (Odds Ratio)"
mm <- mm[,c(9, 3,7:8, 6)]

coef <- rbind(coef, mm)

### 2.3.9 Administration of prokinetic (dichotomous) ----
model <- glmer(prokinetic ~ studyfeed + (1 | Institute), data = p, family = "binomial")
summary(model)

#Export coeff and CI boundaries
mm <- as.data.frame(tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed"))
mm = mm[-1,]
mm$Variable <- "Administration of prokinetic (Odds Ratio)"
mm <- mm[,c(9, 3,7:8, 6)]

coef <- rbind(coef, mm)

### 2.3.10 New AKI (dichotomous) ----
model <- glmer(newAKI ~ studyfeed + (1 | Institute), data = p, family = "binomial")

#Export coeff and CI boundaries
mm <- as.data.frame(tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed"))
mm = mm[-1,]
mm$Variable <- "Incidence of new AKI (Odds Ratio)"
mm <- mm[,c(9, 3,7:8, 6)]

coef <- rbind(coef, mm)

### 2.3.11 Incidence of renal replacement therapy (dichotomous) ----
model <- glmer(RR_YN ~ studyfeed + (1 | Institute), data = p, family = "binomial")

#Export coeff and CI boundaries
mm <- as.data.frame(tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed"))
mm = mm[-1,]
mm$Variable <- "Incidence of renal replacement therapy (Odds Ratio)"
mm <- mm[,c(9, 3,7:8, 6)]

coef <- rbind(coef, mm)

### 2.3.12 Incidence of hepatic dysfunction (dichotomous) ----
model <- glmer(hepaticdysfunction ~ studyfeed + (1 | Institute), data = p, family = "binomial")

#Export coeff and CI boundaries
mm <- as.data.frame(tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed"))
mm = mm[-1,]
mm$Variable <- "Incidence of hepatic dysfunction (Odds Ratio)"
mm <- mm[,c(9, 3,7:8, 6)]

coef <- rbind(coef, mm)

### 2.3.13 Admission to rehabilitation facility (dichotomous) ----
model <- glmer(TERM_REHABYN ~ studyfeed + (1 | Institute), data = p, family = "binomial")

#Export coeff and CI boundaries
mm <- as.data.frame(tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed"))
mm = mm[-1,]
mm$Variable <- "Admission to rehabilitation facility (Odds Ratio)"
mm <- mm[,c(9, 3,7:8, 6)]

coef <- rbind(coef, mm)

### 2.3.14 Return to work (dichotomous) ----
model <- glmer(TERM_REHABYN ~ studyfeed + (1 | Institute), data = p, family = "binomial")

#Export coeff and CI boundaries
mm <- as.data.frame(tidy(model,conf.int=TRUE,exponentiate=TRUE,effects="fixed"))
mm = mm[-1,]
mm$Variable <- "Admission to rehabilitation facility (Odds Ratio)"
mm <- mm[,c(9, 3,7:8, 6)]

coef <- rbind(coef, mm)

#Export 
write_xlsx(coef, "treatmenteffects_second_tert_outcomes_V1.0.xlsx")

## End of file. ##
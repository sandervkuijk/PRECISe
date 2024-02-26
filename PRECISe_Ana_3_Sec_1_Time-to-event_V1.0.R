###############################################################################|
### PRECISe Analysis of the secondary outcomes of time-to-event data 
### Sander van Kuijk, January 2024
###
### This script is to model the primary outcome over the course of follow-up,
### and compare the course between the two treatment allocations.
###
### This script is run after data prepping.
###
###############################################################################|

options(scipen = 999)

### Install packages if not installed previously ----

list.of.packages <- c("survival", "rms", "coxme")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

### Load packages ----

lapply(list.of.packages, library, character.only = TRUE)
rm(list = ls())

### Helper functions ----

# Extract results form coxme object, including p-value in 3 decimals
extract_coxme_table <- function (x){
  beta <- x$coefficients
  nvar <- length(beta)
  nfrail <- nrow(x$var) - nvar
  se <- sqrt(diag(x$var)[nfrail + 1:nvar])
  z <- round(beta/se, 2)
  p <- signif(1 - pchisq((beta/se)^2, 1), 3)
  table = data.frame(cbind(beta, se, z, p))
  return(table)
}

### Read in environment ----

setwd("L:/SPEC/ICU/RESEARCH/PRECISe/Analyses")
load("PRECISeobjects_V1.0.RData")

rm(list=setdiff(ls(), c("p", "extract_coxme_table")))

### Analyses ----
p$Institute <- factor(p$Institute)
p$TTD_alive[is.na(p$TTD_alive)] <- 184

## Data exploration
table(p$studyfeed)
table(p$KM_mortality)
p$KM_period
p$TTD_alive

# Make survival objects for figures and analyses
p$KM_period_m <- round(p$KM_period/30.25, 2)
p$TTD_alive_m <- round(p$TTD_alive/30.25, 2)
S_mort_d <- Surv(p$KM_period, event = p$KM_mortality)
S_mort_m <- Surv(p$KM_period_m, event = p$KM_mortality)
S_TTD_d <- Surv(p$TTD_alive, event = rep(1, nrow(p)))
S_TTD_m <- Surv(p$TTD_alive_m, event = rep(1, nrow(p)))

# Kaplan-Meier estimates, descriptive
res.km <- survfit(S_mort_d ~ p$studyfeed, conf.type = "log-log")
summary(res.km, times = c(30, 90, 180))

# Kaplan-Meier figures, including at-risk table
setwd("L:/SPEC/ICU/RESEARCH/PRECISe/Results/Figures")

pdf("KM_curves_mort.pdf", width = 8, height = 6)
par(mfrow = c(1,1))
par(mar = c(10, 5, 2, 4))
survplot(npsurv(S_mort_m ~ p$studyfeed, conf.int = FALSE), type = "kaplan-meier",
         time.inc = 1, n.risk = TRUE, lty = c(1, 2),
         xlim = c(0, 6), xlab = "Follow-up (months)", cex.n.risk = 1,
         levels.only = TRUE, label.curves = FALSE, adj.n.risk = 1,
         y.n.risk = -0.50, ylab = "Survival probability")
legend(0, 0, legend = c("Group 1", "Group 2"), lty = 1:2, xjust = 0, yjust = 0,
       cex = 0.8)
mtext("Number at risk", side = 1, line = 4, adj = 0)
box()
dev.off()

pdf("KM_curves_ttd.pdf", width = 8, height = 6)
par(mar = c(10, 5, 2, 4))
survplot(npsurv(S_TTD_m ~ p$studyfeed, conf.int = FALSE), type = "kaplan-meier",
         time.inc = 1, n.risk = TRUE, lty = c(1, 2),
         xlim = c(0, 6), xlab = "Follow-up (days)", cex.n.risk = 1,
         levels.only = TRUE, label.curves = FALSE, adj.n.risk = 1,
         y.n.risk = -0.50, ylab = "Probability of not being discharged")
legend(0, 0, legend = c("Group 1", "Group 2"), lty = 1:2, xjust = 0, yjust = 0,
       cex = 0.8)
mtext("Number at risk", side = 1, line = 4, adj = 0)
box()
dev.off()

## (Mixed-effects) Frailty model for mortality
fit_1 <- coxme(S_mort_d ~ studyfeed + (1|Institute), data = p)

# Examine results
fit_1
extract_coxme_table(fit_1)

paste("HR: ", round(exp(fit_1$coefficients), 2), ", 95% CI: ",
      round(exp(confint(fit_1))[1], 2), " - ",
      round(exp(confint(fit_1))[2], 2), sep = "")

# Examine Schoenfeld residuals for proportional hazards assumption
# Rerun frailty model as test on fit_1 lead to message:
# R Session Aborted
fit_2 <- coxph(S_mort_d ~ studyfeed + frailty(Institute), data = p)
fit_2 # Check results, OK!
cox.zph(fit_2) # Check assumption: no evidence of violation of assumption

# Sensitivity analysis
fit_s <- coxme(S_mort_d ~ studyfeed + DEM_SEX + APACHEscore +
                 ADM_SYSTEMDIAGNOSIS + ADM_TYPEOPT + NRSscore +
                 (1|Institute), data = p)

# Examine results
fit_s
extract_coxme_table(fit_s)[1, ]

paste("HR: ", round(exp(fit_s$coefficients)[1], 2), ", 95% CI: ",
      round(exp(confint(fit_s))[1, 1], 2), " - ",
      round(exp(confint(fit_s))[1, 2], 2), sep = "")

## (Mixed-effects) Frailty model for time-to-discharge
fit_3 <- coxme(S_TTD_d ~ studyfeed + (1|Institute), data = p)

# Examine Schoenfeld residuals for proportional hazards assumption
fit_4 <- coxph(S_TTD_d ~ studyfeed + frailty(Institute), data = p)
fit_4 # Check results, OK!
cox.zph(fit_4) # Check assumption: no evidence of violation of assumption

# Examine results
fit_3
extract_coxme_table(fit_3)[1, ]

paste("HR: ", round(exp(fit_3$coefficients)[1], 2), ", 95% CI: ",
      round(exp(confint(fit_3))[1, 1], 2), " - ",
      round(exp(confint(fit_3))[1, 2], 2), sep = "")

### End of file.
###############################################################################|
### PRECISe - Other functional outcomes: MRC-SUM, HGS, 6MWT, Rockwood
### Julia Bels, January 2024
### R version 4.3.1
###
### This script is meant for calculation of the repeated measures: MRC-SUM, 
### HGS, 6MWT and Rockwood clinical frailty scale.
###
###############################################################################|

# 1. Define outcomes for D30, D90 and D180 ----

## 1.1 Total MRC-SUM score ----

# Calculate total scores: if one or more subscores = NA, returns NA (as it should) 
p$MRCSUMscoreD30 <- rowSums(p[,c(308:319)])  
p$MRCSUMscoreD90 <- rowSums(p[,c(390:401)]) 
p$MRCSUMscoreD180 <- rowSums(p[,c(472:483)])

## 1.2 Hand grip strength per follow-up moment ----

#Calculate highest grip strength per follow-up moment
p$HGS.max.30 <- pmax(p$HG_MEAS_D30_Measurement.1_Left.hand..kg.,p$HG_MEAS_D30_Measurement.1_Right.hand..kg.,
                    p$HG_MEAS_D30_Measurement.2_Left.hand..kg.,p$HG_MEAS_D30_Measurement.2_Right.hand..kg.,
                    p$HG_MEAS_D30_Measurement.3_Left.hand..kg.,p$HG_MEAS_D30_Measurement.3_Right.hand..kg.,
                    na.rm = TRUE) #NAs are correct

p$HGS.max.90 <- pmax(p$HG_MEAS_D90_Measurement.1_Left.hand..kg.,p$HG_MEAS_D90_Measurement.1_Right.hand..kg.,
                    p$HG_MEAS_D90_Measurement.2_Left.hand..kg.,p$HG_MEAS_D90_Measurement.2_Right.hand..kg.,
                    p$HG_MEAS_D90_Measurement.3_Left.hand..kg.,p$HG_MEAS_D90_Measurement.3_Right.hand..kg.,
                    na.rm = TRUE) #NAs are correct

p$HGS.max.180 <- pmax(p$HG_MEAS_D180_Measurement.1_Left.hand..kg.,p$HG_MEAS_D180_Measurement.1_Right.hand..kg.,
                    p$HG_MEAS_D180_Measurement.2_Left.hand..kg.,p$HG_MEAS_D180_Measurement.2_Right.hand..kg.,
                    p$HG_MEAS_D180_Measurement.3_Left.hand..kg.,p$HG_MEAS_D180_Measurement.3_Right.hand..kg.,
                    na.rm = TRUE) #NAs are correct

#Standardize for age and sex
#(Hand-Grip Strength: Normative Reference Values and Equations for Individuals 18 to 85 Years of Age Residing in the United States,  DOI: 10.2519/jospt.2018.7851)
p$HGS_predictedv <- ifelse(p$DEM_SEX == 1, 
                               -29.959 -3.095E-05 * (p$DEM_AGE^3) + 38.719 * (p$DEM_HEIGHT/100) + 0.113 * (p$DEM_WEIGHT), 
                               -22.717 -1.920E-05 * (p$DEM_AGE^3) + 30.360 * (p$DEM_HEIGHT/100) + 0.048 * (p$DEM_WEIGHT))

p$HGS.pred.30 <- (p$HGS.max.30/p$HGS_predictedv) * 100
p$HGS.pred.90 <- (p$HGS.max.90/p$HGS_predictedv) * 100
p$HGS.pred.180 <- (p$HGS.max.180/p$HGS_predictedv) * 100

## 1.3 6MWT ----

#Standardize for age and sex
#(Reference equations for the six-minute walk in healthy adults, DOI: 10.1164/ajrccm.158.5.9710086)
p$SMWT.pred <- ifelse(p$DEM_SEX == 1, 
                      (7.57 * p$DEM_HEIGHT)-(5.02 * p$DEM_AGE)-(1.76 * p$DEM_WEIGHT)-309,
                      (2.11 * p$DEM_HEIGHT)-(2.29 * p$DEM_WEIGHT)-(5.78* p$DEM_AGE)+667)

p$SMWT.pred.30 <- (p$SMWT_DIST_D30/p$SMWT.pred) * 100
p$SMWT.pred.90 <- (p$SMWT_DIST_D90/p$SMWT.pred) * 100
p$SMWT.pred.180 <- (p$SMWT_DIST_D180/p$SMWT.pred) * 100

### End of file. ###

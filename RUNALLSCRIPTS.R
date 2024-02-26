###############################################################################|
###  
### RUN ALL PRECISe scripts
###
###############################################################################|

setwd("C:/Users/julia/Documents/PRECISe/PRECISe_FINAL_ANALYSES/scripts")

source("PRECISe_Prep_1_studyexport_V0.4.R")
source("PRECISe_Prep_2_EQ-5D-5L_V0.4.R")
source("PRECISe_Prep_3_EQ-5D-5Ldomains_V0.1.R")
source("PRECISe_Prep_4_Treatmentreports_V0.3.R")
source("PRECISe_Prep_5_OtherEN_V0.2.R")
source("PRECISe_Prep_6_PN_V0.2.R")
source("PRECISe_Prep_7_Quantificationofintake_V0.2.R")
source("PRECISe_Prep_8_ConcomitantMedication_V0.2.R")
source("PRECISe_Prep_9_OtherQuestionnaires_V0.3.R")
source("PRECISe_Prep_10_OtherFunctionalOutcomes_V0.1.R")
source("PRECISe_Prep_11_ClinicalOutcomes_V0.2.R")
source("PRECISe_Prep_12_SF36_PCS_MCS_V0.1.R")

source("PRECISe_Output_1_Figure1_PatientFlowchart_V0.2.R")
source("PRECISe_Output_2_Table1_BaselineCharacteristics_V0.1.R")
source("PRECISe_Output_3_Figure2_IntakeStudyNutrition_V0.3.R")
source("PRECISe_Output_4_Table2_Outcomes_V0.2.R")


setwd("C:/Users/julia/Documents/PRECISe/PRECISe_FINAL_ANALYSES/output")

rm(list=ls()[! ls() %in% c("p","questLong","t", "i")])

save(p, questLong, i, t, file = "PRECISeobjects_V0.4.RData")

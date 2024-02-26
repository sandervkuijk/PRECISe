###############################################################################|
### PRECISe Prep for PCS and MCS of SF-36
### Julia Bels, February 2024
### R version 4.3.2
###
### This script is meant for calculation of the physical component score (PCS) 
### and mental component score (MCS) of SF-36 questionnaire.
###
### References used: 
### AaronsonNK,MullerM,P.D.A.C,Essink-BotML,FekkesM, SandermanR,etal.Translation,validation,andnormingof the DutchlanguageversionoftheSF-36healthsurveyincommunity andchronicdiseasepopulations. JClinEpidemiol 1998;51(11): 1055e68.
### --> Means and SD from Dutch general population.
###
### https://www.rand.org/pubs/reprints/RP1309.html 
### --> Scoring coefficients, both from uncorrelated and correlated model. 
###
###############################################################################|

# 1. Calculate Z-scores ----
# By subtracting the Dutch general mean from each scale score, 
# and dividing the difference by the corresponding Dutch general standard deviation.

sf$PF_Z <- (sf$PF-83.0)/22.8
sf$RP_Z <- (sf$RP-76.4)/36.3
sf$BP_Z <- (sf$BP-74.9)/23.4
sf$GH_Z <- (sf$GH-70.7)/20.7
sf$VT_Z <- (sf$VT-68.6)/19.3
sf$SF_Z <- (sf$SF-84.0)/22.4
sf$RE_Z <- (sf$RE-82.3)/32.9
sf$MH_Z <- (sf$MH-76.8)/17.4

# 2. Compute aggregate scores for PCS and MCS using physical and mental factor score coefficients ----

## pcsag=(PF_Z * .42402)+(RP_Z * .35119)+(BP_Z * .31754)+
##       (SF_Z * -.00753)+(MH_Z * -.22069)+(RE_Z * -.19206)+
##       (VT_Z * .02877)+(GH_Z * .24954);

## mcsag=(PF_Z * -.22999)+(RP_Z * -.12329)+(BP_Z * -.09731)+
##       (SF_Z * .26876)+(MH_Z * .48581)+(RE_Z * .43407)+
##       (VT_Z * .23534)+(GH_Z * -.01571);

#Orthogonal coefficients
sf$pcsag1 <- ((sf$PF_Z * .42402)+(sf$RP_Z * .35119)+(sf$BP_Z * .31754) + 
             (sf$SF_Z * -.00753)+(sf$MH_Z * -.22069)+(sf$RE_Z * -.19206) +
             (sf$VT_Z * .02877)+(sf$GH_Z * .24954))

sf$mcsag1 <- ((sf$PF_Z * -.22999)+(sf$RP_Z * -.12329)+(sf$BP_Z * -.09731)+
             (sf$SF_Z * .26876)+(sf$MH_Z * .48581)+(sf$RE_Z * .43407)+
             (sf$VT_Z * .23534)+(sf$GH_Z * -.01571))


#Oblique coefficients 
sf$pcsag2 <- ((sf$PF_Z * .20)+(sf$RP_Z * .31)+(sf$BP_Z * .23) + 
                (sf$SF_Z * .11)+(sf$MH_Z * -.03)+(sf$RE_Z * .03) +
                (sf$VT_Z * .13)+(sf$GH_Z * .20))

sf$mcsag2 <- ((sf$PF_Z * -.02)+(sf$RP_Z * .03)+(sf$BP_Z * .04)+
                (sf$SF_Z * .14)+(sf$MH_Z * .35)+(sf$RE_Z * .20)+
                (sf$VT_Z * .29)+(sf$GH_Z * .10))

# 3. Compute standardized scores ----
# Multiply each aggregate component scale score by 10 and add the resulting product
# to 50.

sf$PCS1 = (sf$pcsag1*10) + 50
sf$MCS1 = (sf$mcsag1*10) + 50

sf$PCS2 = (sf$pcsag2*10) + 50
sf$MCS2 = (sf$mcsag2*10) + 50

# 4. Check correlations ----
library("ggpubr")

# Should correlate highly --> all have high R
ggscatter(sf, x = "PCS1", y = "PF", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")
ggscatter(sf, x = "PCS2", y = "PF", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")

ggscatter(sf, x = "PCS1", y = "RP", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")
ggscatter(sf, x = "PCS2", y = "RP", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")

ggscatter(sf, x = "PCS1", y = "BP", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")
ggscatter(sf, x = "PCS2", y = "BP", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")


ggscatter(sf, x = "MCS1", y = "MH", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")
ggscatter(sf, x = "MCS2", y = "MH", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")

ggscatter(sf, x = "MCS1", y = "RE", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")
ggscatter(sf, x = "MCS2", y = "RE", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")

ggscatter(sf, x = "MCS1", y = "SF", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")
ggscatter(sf, x = "MCS2", y = "SF", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")

# Should correlate moderately --> all have R around 0.5
ggscatter(sf, x = "PCS1", y = "GH", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")
ggscatter(sf, x = "MCS1", y = "GH", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")
ggscatter(sf, x = "PCS1", y = "GH", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")
ggscatter(sf, x = "MCS1", y = "GH", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")

# Should not correlate --> low R = 0.17 (uncorrelated model)
ggscatter(sf, x = "MCS1", y = "PCS1", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")

# Correlation is allowed (correlated model) --> R=0.83
ggscatter(sf, x = "MCS2", y = "PCS2", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson")

# Conclusion: scoring check complete, no errors detected during scoring.

# 5. Combine with main study objects ----
sff <- sf[,c("Participant.Id","fupday","PCS1","MCS1","PCS2","MCS2")]
questLong <- merge(questLong, sff, by=c("Participant.Id", "fupday"), all = TRUE)

## End of file. ##
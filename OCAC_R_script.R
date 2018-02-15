
setwd("U:/OCAC")
library("xlsx")
library(polycor)
#-------loading data files--------------

OCACraw <- read.xlsx("OCACresults140213_forimport.xlsx",1)
OCACcleaned <- read.xlsx("OCAC exploratory analysis.xlsx",1)
OCACcOrAbove <- read.xlsx("OCACcOrAboves.xlsx",1)
OCACcOrAboveCorrMatrix <- read.xlsx("correlation matrix for import in R.xlsx",1)
SocioEconomic <- read.xlsx("ocac and socioeconomic indicators.xlsx", 1)
factorScoresB <- read.xlsx("factor scores B and other data.xlsx", 1)
factorScoresWeigthedAVG <- read.xlsx("fator scores weighted avg and other data.xlsx",1)
factorScoresRegMethod <- read.xlsx("fator scores regression method and other data.xlsx",1)

newDataPolychor <- read.xlsx("new data for polychor correlations.xlsx", 1)
factorScoresWeigthedAVG_new <- read.xlsx("fator scores weighted avg and other data_new.xlsx",1)
OCACcOrAbove_new <- read.xlsx("OCACcOrAboves_new.xlsx",1)



#OCACordered <- lapply(OCACcleaned[3:92], function(x) ordered(x, labels("A","B","C","D","E")) )
OCACordered <-OCACcleaned

OCACcNumberical <- OCACcOrAbove[, c(2:91)]
OCACcForCFA <- OCACcNumberical
OCACcForCFA <- OCACcForCFA[ -c(9,14,22,24,34,41,42,43,45,51,55,69,74,90) ]

OCACcForCFA_new <- OCACcOrAbove_new[ -c(1,2,8,10,15,16,19,24,25,26,30,33,36,38,40,41,44,45,46,48,53,54,55,56,58,59,63,73,75,76,78,79,91,92,95)]


OCACcNumberical2$Beneficiary.communication.during.emergencies <- OCACcNumberical2$Investment.in.innovative.action <- OCACcNumberical2$Investing.in.skills <- OCACcNumberical2$Succession.management <- NULL

asOrdered <- function(x) as.ordered(x)
factorsOrdered <- function(d) modifyList(d, lapply(d, asOrdered))
OCACordered[,c(3:92)] <- factorsOrdered(OCACcleaned[,c(3:92)])

OCA
asOrdered <- function(x) as.ordered(x)
factorsOrdered <- function(d) modifyList(d, lapply(d, asOrdered))
OCACcAboveOrdered <- factorsOrdered(OCACcNumberical)

asOrdered <- function(x) as.ordered(x)
factorsOrdered <- function(d) modifyList(d, lapply(d, asOrdered))
newDataPolychorOrdered <- factorsOrdered(newDataPolychor)


### regressions ######

reg1 <- lm(SumCorAbove~.-SumCorAbove - Country.name, data=OCACcOrAbove)
summary(reg1)
htmlreg(list(reg1), file = "DevelopmentStages.doc", 
        inline.css = FALSE, octype = TRUE, html.tag = TRUE, 
        head.tag = TRUE, body.tag = TRUE, custom.model.names = c("Sum C or Above"))
unlink("texreg.doc")

reg2 <- lm(How.many.C.or.above.~Urban.population....of.total., data=SocioEconomic)
summary(reg2)

regFactors <- lm(Percentage.of.attributes.that.are.C.or.higher~
                  FinancialResourceMobilisation+                
                  RCRCidentity+                                 
                  Autonomy+                                     
                  ExternalCommunications+                       
                  SecurityAndSafety+                            
                  HumanResourcesManagement+                     
                  RiskManagement+                               
                  PMER+                                         
                  VolunteeringCultureAndManagement+             
                  FinancialManagement+                          
                  GovernanceCapacity+                           
                  Logistics, data=factorScoresWeigthedAVG)          

regFactors2 <- lm(Percentage.of.attributes.that.are.C.or.higher~
                  FnncRM+                                       
                  RCRCdn+                                       
                  Autnmy+                                       
                  ExtrnC+                                       
                  ScrtAS+                                       
                  HmnRsM+                                       
                  RskMng+                                       
                  PMER+                                         
                  VlnCAM+                                       
                  FnnclM+                                       
                  GvrnnC+                                       
                  Lgstcs, data=factorScoresRegMethod)      

regFactors3 <- lm(Percentage.of.attributes.that.are.C.or.higher~
                   FinancialResourceMobilisation+                
                   RCRCidentity+                                 
                   Autonomy+                                     
                   ExternalCommunications+                       
                   SecurityAndSafety+                            
                   HumanResourcesManagement+                     
                   RiskManagement+                               
                   PMER+                                         
                   VolunteeringCultureAndManagement+             
                   FinancialManagement+                          
                   GovernanceCapacity+                           
                   Logistics+
                   HDI, data=factorScoresWeigthedAVG)         
summary(regFactors3)

regFactors4 <- lm(Percentage.of.attributes.that.are.C.or.higher~
                    FinancialResourceMobilisation                
                    , data=factorScoresWeigthedAVG)         
summary(regFactors4)

regFactors5 <- lm(Percentage.of.attributes.that.are.C.or.higher~            
                    RCRCidentity+                                 
                    Autonomy+                                     
                    ExternalCommunications+                       
                    SecurityAndSafety+                            
                    HumanResourcesManagement+                     
                    RiskManagement+                               
                    PMER+                                         
                    VolunteeringCultureAndManagement+             
                    FinancialManagement+                          
                    GovernanceCapacity+                           
                    Logistics, data=factorScoresWeigthedAVG)         
summary(regFactors5)

regFactors6 <- lm(Percentage.of.attributes.that.are.C.or.higher~
                    FinancialResourceMobilisation+                
                    RCRCidentity+                                 
                    Autonomy+                                     
                    ExternalCommunications+                       
                    SecurityAndSafety+                            
                    HumanResourcesManagement+                     
                    RiskManagement+                               
                    PMER+                                         
                    VolunteeringCultureAndManagement+             
                    FinancialManagement+                          
                    GovernanceCapacity+                           
                    Logistics+
                    log(cgdp)
                    , data=factorScoresWeigthedAVG)         
summary(regFactors6)

### cummulative explained variance - in order of explained variance - new data

regFactors7a <- lm(percentage.of.strength.attributes~
                 RCRCidentity+
                 PMER+
                 GovernanceCapacity+
                 HumanResourcesManagement+
                 VolunteeringCultureAndManagement+
                 FinancialManagement+
                 SecurityAndSafety+
                 RiskManagement+
                 FinancialResourceMobilisation+
                 ExternalCommunications+
                 Logistics+
                 Autonomy
                 ,data=factorScoresWeigthedAVG_new)  
                 
                   
regFactors7b <- lm(percentage.of.strength.attributes~
                     RCRCidentity+
                     PMER+
                     GovernanceCapacity+
                     HumanResourcesManagement+
                     VolunteeringCultureAndManagement+
                     FinancialManagement+
                     SecurityAndSafety+
                     RiskManagement+
                     FinancialResourceMobilisation+
                     ExternalCommunications+
                     Logistics
                   ,data=factorScoresWeigthedAVG_new)  
                   
                      

regFactors7c <- lm(percentage.of.strength.attributes~
                     RCRCidentity+
                     PMER+
                     GovernanceCapacity+
                     HumanResourcesManagement+
                     VolunteeringCultureAndManagement+
                     FinancialManagement+
                     SecurityAndSafety+
                     RiskManagement+
                     FinancialResourceMobilisation+
                     ExternalCommunications
                   ,data=factorScoresWeigthedAVG_new)  


regFactors7d <- lm(percentage.of.strength.attributes~
                     RCRCidentity+
                     PMER+
                     GovernanceCapacity+
                     HumanResourcesManagement+
                     VolunteeringCultureAndManagement+
                     FinancialManagement+
                     SecurityAndSafety+
                     RiskManagement+
                     FinancialResourceMobilisation
                   ,data=factorScoresWeigthedAVG_new)  

regFactors7e <- lm(percentage.of.strength.attributes~
                     RCRCidentity+
                     PMER+
                     GovernanceCapacity+
                     HumanResourcesManagement+
                     VolunteeringCultureAndManagement+
                     FinancialManagement+
                     SecurityAndSafety+
                     RiskManagement
                   ,data=factorScoresWeigthedAVG_new)  

regFactors7f <- lm(percentage.of.strength.attributes~
                     RCRCidentity+
                     PMER+
                     GovernanceCapacity+
                     HumanResourcesManagement+
                     VolunteeringCultureAndManagement+
                     FinancialManagement+
                     SecurityAndSafety
                   ,data=factorScoresWeigthedAVG_new)  


regFactors7g <- lm(percentage.of.strength.attributes~
                     RCRCidentity+
                     PMER+
                     GovernanceCapacity+
                     HumanResourcesManagement+
                     VolunteeringCultureAndManagement+
                     FinancialManagement
                   ,data=factorScoresWeigthedAVG_new)  


regFactors7h <- lm(percentage.of.strength.attributes~
                     RCRCidentity+
                     PMER+
                     GovernanceCapacity+
                     HumanResourcesManagement+
                     VolunteeringCultureAndManagement
                   ,data=factorScoresWeigthedAVG_new)  



regFactors7i <- lm(percentage.of.strength.attributes~
                     RCRCidentity+
                     PMER+
                     GovernanceCapacity+
                     HumanResourcesManagement
                   ,data=factorScoresWeigthedAVG_new)  

regFactors7j <- lm(percentage.of.strength.attributes~
                     RCRCidentity+
                     PMER+
                     GovernanceCapacity
                   ,data=factorScoresWeigthedAVG_new)  


regFactors7k <- lm(percentage.of.strength.attributes~
                     RCRCidentity+
                     PMER
                   ,data=factorScoresWeigthedAVG_new)  

regFactors7l <- lm(percentage.of.strength.attributes~
                     RCRCidentity
                   ,data=factorScoresWeigthedAVG_new)  


regFactors8a <- lm(percentage.of.strength.attributes~
                    Autonomy
                   ,data=factorScoresWeigthedAVG_new)  


regFactors8b <- lm(percentage.of.strength.attributes~
                     Logistics
                   ,data=factorScoresWeigthedAVG_new)  



regFactors8c <- lm(percentage.of.strength.attributes~
                     ExternalCommunications
                   ,data=factorScoresWeigthedAVG_new)  


regFactors8d <- lm(percentage.of.strength.attributes~
                     SecurityAndSafety
                   ,data=factorScoresWeigthedAVG_new)  

regFactors8e <- lm(percentage.of.strength.attributes~
                     FinancialResourceMobilisation
                   ,data=factorScoresWeigthedAVG_new)  

regFactors8f <- lm(percentage.of.strength.attributes~
                    FinancialManagement
                   ,data=factorScoresWeigthedAVG_new)  


regFactors8g <- lm(percentage.of.strength.attributes~
                     RiskManagement
                   ,data=factorScoresWeigthedAVG_new)  


regFactors8h <- lm(percentage.of.strength.attributes~
                     VolunteeringCultureAndManagement
                   ,data=factorScoresWeigthedAVG_new)  



regFactors8i <- lm(percentage.of.strength.attributes~
                     HumanResourcesManagement
                   ,data=factorScoresWeigthedAVG_new)  

regFactors8j <- lm(percentage.of.strength.attributes~
                     GovernanceCapacity
                   ,data=factorScoresWeigthedAVG_new)  


regFactors8k <- lm(percentage.of.strength.attributes~
                     PMER
                   ,data=factorScoresWeigthedAVG_new)  

regFactors8l <- lm(percentage.of.strength.attributes~
                     RCRCidentity
                   ,data=factorScoresWeigthedAVG_new)  



library(texreg)

htmlreg(list(regFactors7a,regFactors7b,regFactors7c,regFactors7d,regFactors7e,regFactors7f,regFactors7g,regFactors7h,regFactors7i,regFactors7j,regFactors7k, regFactors7l, regFactors8a,regFactors8b,regFactors8c,regFactors8d,regFactors8e,regFactors8f,regFactors8g,regFactors8h,regFactors8i,regFactors8j,regFactors8k, regFactors8l ), file = "factorReg_new.doc", 
        inline.css = FALSE, octype = TRUE, html.tag = TRUE, 
        head.tag = TRUE, body.tag = TRUE)



regFactorsFRM0 <- lm(Percentage.of.attributes.that.are.C.or.higher~
                       RCRCidentity                       ,data=factorScoresWeigthedAVG)  

regFactorsFRM1 <- lm(Percentage.of.attributes.that.are.C.or.higher~
                     RCRCidentity+
                     FinancialResourceMobilisation
                   ,data=factorScoresWeigthedAVG)  

regFactorsFRM2 <- lm(Percentage.of.attributes.that.are.C.or.higher~
                       RCRCidentity+
                       FinancialResourceMobilisation+
                       PMER
                     ,data=factorScoresWeigthedAVG)  

regFactorsFRM3 <- lm(Percentage.of.attributes.that.are.C.or.higher~
                       RCRCidentity+
                       FinancialResourceMobilisation+
                       PMER+
                       GovernanceCapacity
                     ,data=factorScoresWeigthedAVG) 



regFactorsFRM4 <- lm(Percentage.of.attributes.that.are.C.or.higher~
                       RCRCidentity+
                       FinancialResourceMobilisation+
                       PMER+
                       GovernanceCapacity+
                       HumanResourcesManagement
                     ,data=factorScoresWeigthedAVG) 



regFactorsFRM5 <- lm(Percentage.of.attributes.that.are.C.or.higher~
                       RCRCidentity+
                       FinancialResourceMobilisation+
                       PMER+
                       GovernanceCapacity+
                       HumanResourcesManagement+
                       VolunteeringCultureAndManagement
                     ,data=factorScoresWeigthedAVG) 



regFactorsFRM6 <- lm(Percentage.of.attributes.that.are.C.or.higher~
                       RCRCidentity+
                       FinancialResourceMobilisation+
                       PMER+
                       GovernanceCapacity+
                       HumanResourcesManagement+
                       VolunteeringCultureAndManagement+
                       RiskManagement
                     ,data=factorScoresWeigthedAVG) 


regFactorsFRM7 <- lm(Percentage.of.attributes.that.are.C.or.higher~
                       RCRCidentity+
                       FinancialResourceMobilisation+
                       PMER+
                       GovernanceCapacity+
                       HumanResourcesManagement+
                       VolunteeringCultureAndManagement+
                       RiskManagement+
                       FinancialManagement
                     ,data=factorScoresWeigthedAVG) 


regFactorsFRM8 <- lm(Percentage.of.attributes.that.are.C.or.higher~
                       RCRCidentity+
                       FinancialResourceMobilisation+
                       PMER+
                       GovernanceCapacity+
                       HumanResourcesManagement+
                       VolunteeringCultureAndManagement+
                       RiskManagement+
                       FinancialManagement+
                       SecurityAndSafety
                     ,data=factorScoresWeigthedAVG) 



regFactorsFRM9 <- lm(Percentage.of.attributes.that.are.C.or.higher~
                       RCRCidentity+
                       FinancialResourceMobilisation+
                       PMER+
                       GovernanceCapacity+
                       HumanResourcesManagement+
                       VolunteeringCultureAndManagement+
                       RiskManagement+
                       FinancialManagement+
                       SecurityAndSafety+
                       ExternalCommunications
                     ,data=factorScoresWeigthedAVG) 


regFactorsFRM10 <- lm(Percentage.of.attributes.that.are.C.or.higher~
                       RCRCidentity+
                       FinancialResourceMobilisation+
                       PMER+
                       GovernanceCapacity+
                       HumanResourcesManagement+
                       VolunteeringCultureAndManagement+
                       RiskManagement+
                       FinancialManagement+
                       SecurityAndSafety+
                       ExternalCommunications+
                       Logistics
                     ,data=factorScoresWeigthedAVG) 


regFactorsFRM11 <- lm(Percentage.of.attributes.that.are.C.or.higher~
                       RCRCidentity+
                       FinancialResourceMobilisation+
                       PMER+
                       GovernanceCapacity+
                       HumanResourcesManagement+
                       VolunteeringCultureAndManagement+
                       RiskManagement+
                       FinancialManagement+
                       SecurityAndSafety+
                       ExternalCommunications+
                       Logistics+
                       Autonomy
                       ,data=factorScoresWeigthedAVG) 

htmlreg(list(regFactorsFRM0, regFactorsFRM1, regFactorsFRM2, regFactorsFRM3, regFactorsFRM4, regFactorsFRM5, regFactorsFRM6, regFactorsFRM7, regFactorsFRM8,regFactorsFRM9, regFactorsFRM10, regFactorsFRM11  ), file = "factorRegFRM.doc", 
        inline.css = FALSE, octype = TRUE, html.tag = TRUE, 
        head.tag = TRUE, body.tag = TRUE)



#### correlations and partial correlations of factors####
correlationmatrix <- cor(factorScoresWeigthedAVG[,c(3:14,20)]) 
partialCorrelationmatrix <- pcor(factorScoresWeigthedAVG[,c(3:14,20)]) 
partialCorrelationmatrix2 <- pcor(factorScoresWeigthedAVG[,c(4:14,20)]) 
#### poly corr  matrix######
polycormatrix <- hetcor(OCACordered[,c(3:92)], use="pairwise.complete.obs")
polycormatrix2 <- hetcor(OCACordered[,c(3:89)], use="pairwise.complete.obs")
polycormatrix3 <- hetcor(na.omit(OCACordered[,c(3:67,69:89)]), ML=TRUE)

binarycorrmatrix <- hetcor(OCACcAboveOrdered, use="complete.obs")
# sorted corr matrix
mat.sort(polycormatrix2, pc)

binarycovmatrix <- cov(OCACcNumberical, use="pairwise.complete.obs")
binarycovmatrixforCFA <- cov(OCACcForCFA, use="pairwise.complete.obs")

partialCorrelationsVariables <- pcor(D1)

variablenames <- names(OCACcNumberical)


#new data  polychor matrix

polycormatrix_new<- hetcor(newDataPolychorOrdered, use="pairwise.complete.obs")




####impute missing data######

# generate 5 multiple complete datasets 
library(mice)
out <- mice(OCACcForCFA, m=5) 
D1 <- complete(out, 1) 
D2 <- complete(out, 2) 
D3 <- complete(out, 3) 
D4 <- complete(out, 4) 
D5 <- complete(out, 5) 

library(mice)
out <- mice(OCACcForCFA_new, m=5) 
D1_n <- complete(out, 1) 
D2_n <- complete(out, 2) 
D3_n <- complete(out, 3) 
D4_n <- complete(out, 4) 
D5_n <- complete(out, 5) 


#### pca (A,B,C, etc)#######
library(psych)
options(width=10000)

### all factors no rotate
pcAllnorotate <- principal(polycormatrix2$correlations, nfactors=87, rotate="none", n.obs=49)
plot(pcAllnorotate$values, type="b")
print.psych(pcAllnorotate, cut=0.0, sort=FALSE)

### number of factors to eliminate 
fa.parallel(polycormatrix2$correlations, n.obs=49, fa="pc")


### all factors varimax 
pcAll <- principal(polycormatrix2$correlations, nfactors=87, n.obs=49)
plot(pcAll$values, type="b")
print.psych(pcAll, cut=0.0, sort=TRUE)

### all factors oblimin 
pcAllOblimin <- principal(polycormatrix2$correlations, rotate="oblimin", nfactors=87, n.obs=49)
print.psych(pcAllOblimin, cut=0.0, sort=TRUE)
## 20 components

pc20 <- principal(polycormatrix2$correlations, nfactors=20, n.obs=49, rotate="oblimin")
print.psych(pc20, cut=0.0, sort=TRUE)
#sortedPC <- fa.sort(pc)
#sink("sortedMatrix.txt")
#mat.sort(polycormatrix2$correlations, paste(attributes(sortedPC$communality)))


## 15 components
pc15Ob <- principal(polycormatrix2$correlations, nfactors=15, n.obs=49, rotate="oblimin")
print.psych(pc15Ob, cut=0.0, sort=TRUE)
sortedPC15Ob <- fa.sort(pc15Ob)

mat.sort(polycormatrix2$correlations,sortedPC15Ob$order)


## 10 components
pc10Ob <- principal(polycormatrix2$correlations, nfactors=10, n.obs=49, rotate="oblimin")
print.psych(pc10Ob, cut=0.0, sort=TRUE)
sortedPC10Ob <- fa.sort(pc10Ob)

mat.sort(polycormatrix2$correlations,sortedPC10Ob$order)
                 
# 5 components
pc5Ob <- principal(polycormatrix2$correlations, nfactors=5, n.obs=49, rotate="oblimin")
print.psych(pc5Ob, cut=0.0, sort=TRUE)
sortedPC5Ob <- fa.sort(pc5Ob)

# 4 components
pc4Ob <- principal(polycormatrix2$correlations, nfactors=4, n.obs=49, rotate="oblimin")
print.psych(pc4Ob, cut=0.0, sort=TRUE)
sortedPC4Ob <- fa.sort(pc4Ob)            
                 
#### misc

pc15roateVarimax <- principal(polycormatrix$correlations, nfactors=15, rotate="varimax")
print.psych(pc15roateVarimax, cut=0.0, sort=TRUE)

pc15 <- principal(polycormatrix$correlations, nfactors=15, rotate="none")
pc15roate <- principal(polycormatrix$correlations, nfactors=15, rotate="oblimin")
print.psych(pc15roate, cut=0.3, sort=TRUE)
             
             
nScree(polycormatrix2$correlations)

FA15Oblim <- fa.poly(polycormatrix$correlations, nfactors=15, rotate="oblimin")


fa.parallel.poly(OCACordered[,c(3:89)])
fa.parallel.poly(polycormatrix2$correlations)




#####pca (a,b, c...) new data

library(psych)
options(width=10000)

### all factors no rotate
pcAllnorotate <- principal(polycormatrix_new$correlations, nfactors=87, rotate="none", n.obs=51)
plot(pcAllnorotate$values, type="b")
print.psych(pcAllnorotate, cut=0.0, sort=FALSE)

### number of factors to eliminate 
fa.parallel(polycormatrix_new$correlations, n.obs=51, fa="pc")


## 10 components
pc10Ob_new <- principal(polycormatrix_new$correlations, nfactors=10, n.obs=51, rotate="oblimin")
print.psych(pc10Ob_new, cut=0.0, sort=TRUE)

## 15 components
pc15Ob_new <- principal(polycormatrix_new$correlations, nfactors=15, n.obs=51, rotate="oblimin")
print.psych(pc15Ob_new, cut=0.0, sort=TRUE)


## 8 components
pc8Ob_new <- principal(polycormatrix_new$correlations, nfactors=8, n.obs=51, rotate="oblimin")
print.psych(pc8Ob_new, cut=0.0, sort=TRUE)






###### factor analysis
fa.parallel(polycormatrix2$correlations, n.obs=49, fa="fa")






#### pca (C+)####
library(psych)
options(width=10000)

### all factors no rotate
pcCAllNorotate <- prcomp(OCACcNumberical2, na.action="na.omit", pd=TRUE)
plot(pcCAllNorotate$values, type="b")
print.psych(pcCAllNorotate, cut=0.0, sort=FALSE)

### number of factors to eliminate 
fa.parallel(pcCAllNorotate, n.obs=49, fa="pc")


### all factors varimax 
pcAll <- principal(polycormatrix2$correlations, nfactors=87, n.obs=49)
plot(pcAll$values, type="b")
print.psych(pcAll, cut=0.0, sort=TRUE)

### all factors oblimin 
pcAllOblimin <- principal(polycormatrix2$correlations, rotate="oblimin", nfactors=87, n.obs=49)
print.psych(pcAllOblimin, cut=0.0, sort=TRUE)
## 20 components

pc20 <- principal(polycormatrix2$correlations, nfactors=20, n.obs=49, rotate="oblimin")
print.psych(pc20, cut=0.0, sort=TRUE)
#sortedPC <- fa.sort(pc)
#sink("sortedMatrix.txt")
#mat.sort(polycormatrix2$correlations, paste(attributes(sortedPC$communality)))


## 15 components
pc15Ob <- principal(polycormatrix2$correlations, nfactors=15, n.obs=49, rotate="oblimin")
print.psych(pc15Ob, cut=0.0, sort=TRUE)
sortedPC15Ob <- fa.sort(pc15Ob)

mat.sort(polycormatrix2$correlations,sortedPC15Ob$order)


## 10 components
pc10Ob <- principal(polycormatrix2$correlations, nfactors=10, n.obs=49, rotate="oblimin")
print.psych(pc10Ob, cut=0.0, sort=TRUE)
sortedPC10Ob <- fa.sort(pc10Ob)

mat.sort(polycormatrix2$correlations,sortedPC10Ob$order)

# 5 components
pc5Ob <- principal(polycormatrix2$correlations, nfactors=5, n.obs=49, rotate="oblimin")
print.psych(pc5Ob, cut=0.0, sort=TRUE)
sortedPC5Ob <- fa.sort(pc5Ob)

# 4 components
pc4Ob <- principal(polycormatrix2$correlations, nfactors=4, n.obs=49, rotate="oblimin")
print.psych(pc4Ob, cut=0.0, sort=TRUE)
sortedPC4Ob <- fa.sort(pc4Ob)            
library(sem)


#######CFA####################

cfaModel1 <- specifyModel()
AdminLegal -> Contract.Management , 1
AdminLegal -> Filing.and.archives , AdminLegal2
AdminLegal -> Legal.compliance , AdminLegal3
Autonomy -> Autonomy...choice.of.leadership.at.branch.level , 1
Autonomy -> Autonomy...choice.of.leadership.at.the.Headquarters.level , Autonomy2
Autonomy -> Autonomy...Geographical.coverage , Autonomy3
Autonomy -> Autonomy...programmes.and.interventions , Autonomy4
Basicoperations -> Geographical.coverage , 1
Basicoperations -> Policies , Basicoperations2
Externalcommunication -> Beneficiary.communication.during.emergencies , 1
Externalcommunication -> Branding , Externalcommunication2
Externalcommunication -> Communications.during.emergencies , Externalcommunication3
Externalcommunication -> Donor.mobilisation , Externalcommunication4
Externalcommunication -> External.communication , Externalcommunication5
Externalcommunication -> External.environment , Externalcommunication6
Externalcommunication -> Participation.in.Movement.events , Externalcommunication7
Externalcommunication -> Public.image , Externalcommunication8
Externalcommunication -> Reporting , Externalcommunication9
Finance -> Budgeting , 1
Finance -> Consolidation , Finance2
Finance -> Documented.processes.and.procedures , Finance3
Finance -> Expenditure.Authorisation , Finance4
Finance -> External.Audit , Finance5
Finance -> Finance.staff , Finance6
Finance -> Financial.Information.Systems , Finance7
Finance -> Financial.Reporting , Finance8
Finance -> Independent.advice.and.oversight , Finance9
Finance -> Internal.Control.Audit , Finance10
Finance -> Treasury.Management , Finance11
FinancialResourceMobilisation -> Business.continuity , 1
FinancialResourceMobilisation -> Restricted.income.diversification.and.stability , FinancialResourceMobilisation2
FinancialResourceMobilisation -> RM.capacities , FinancialResourceMobilisation3
FinancialResourceMobilisation -> Unrestricted.fund.mobilisation , FinancialResourceMobilisation4
FinancialResourceMobilisation -> Unrestricted.funding.over.core.budget , FinancialResourceMobilisation5
FinancialResourceMobilisation -> Unrestricted.income.diversification.and.stability , FinancialResourceMobilisation6
FinancialResourceMobilisation -> Unrestricted.reserves , FinancialResourceMobilisation7
FinancialResourceMobilisation -> Unrestricted.working.capital , FinancialResourceMobilisation8
HumanResources -> Job.satisfaction , 1
HumanResources -> Recruitment.performance.and.surge.capacity , HumanResources2
HumanResources -> Recruitment.procedure , HumanResources3
HumanResources -> Staff...volunteer.and.member.composition , HumanResources4
HumanResources -> Staff.compensation , HumanResources5
HumanResources -> Staff.development , HumanResources6
HumanResources -> Staffing.structure , HumanResources7
Infrastructure -> Buildings , 1
Infrastructure -> Fleet , Infrastructure2
Infrastructure -> Internal.and.external.communications , Infrastructure3
Infrastructure -> Internal.management.and.service.delivery , Infrastructure4
Internalstakeholderengamentdecisionmaking -> Constituency.empowerment , 1
Internalstakeholderengamentdecisionmaking -> Delegation.of.management.responsibilities , Internalstakeholderengamentdecisionmaking2
Internalstakeholderengamentdecisionmaking -> Internal.Communication , Internalstakeholderengamentdecisionmaking3
Internalstakeholderengamentdecisionmaking -> Member.involvement.in.planning.and.decision.making , Internalstakeholderengamentdecisionmaking4
Internalstakeholderengamentdecisionmaking -> Volunteer.involvement.in.planning.and.decision.making , Internalstakeholderengamentdecisionmaking5
LeadershipGovernance -> Governance , 1
LeadershipGovernance -> Governance.â...Senior.Management.complementarity , LeadershipGovernance2
Logisticsprocurement -> Fleet.management , 1
Logisticsprocurement -> Procurement.and.mobilisation.of.goods.and.services , Logisticsprocurement2
Logisticsprocurement -> Warehousing , Logisticsprocurement3
Programdevelopment -> Evaluations , 1
Programdevelopment -> Monitoring.against.objectives.and.budgets , Programdevelopment2
Programdevelopment -> Needs.assessment , Programdevelopment3
Programdevelopment -> Operational.planning , Programdevelopment4
Programdevelopment -> Overheads , Programdevelopment5
Programdevelopment -> Planning..Monitoring..Evaluation.and.Reporting..PMER..capacities , Programdevelopment6
Programdevelopment -> Programme.descriptions , Programdevelopment7
Programdevelopment -> Stakeholders.satisfaction , Programdevelopment8
Programdevelopment -> Sustainability.of.services , Programdevelopment9
RCRCidentity -> Emblem.Law , 1
RCRCidentity -> Fundamental.Principles , RCRCidentity2
RCRCidentity -> Red.Cross.Red.Crescent.Law , RCRCidentity3
RCRCidentity -> Statutes , RCRCidentity4
RCRCidentity -> Strategic.Plan , RCRCidentity5
Relationshipwithgovernment -> Auxiliary.role , 1
Relationshipwithgovernment -> Humanitarian.Diplomacy , Relationshipwithgovernment2
RiskManagement -> Insurance , 1
RiskManagement -> Integrity.framework , RiskManagement2
RiskManagement -> Reputational.risk.management , RiskManagement3
SecurityandSafety -> Security..Safety.Management , 1
SecurityandSafety -> Security.Safety..culture , SecurityandSafety2
SecurityandSafety -> Security.Safety.Training , SecurityandSafety3
SecurityandSafety -> Violence...abuse.of.power.prevention , SecurityandSafety4
SecurityandSafety -> Working.conditions , SecurityandSafety5
Volunteering -> Volunteer.recognition , 1
Volunteering -> Volunteer.records.database , Volunteering2
Volunteering -> Volunteer.recruitment.and.retention , Volunteering3
Volunteering -> Volunteer.specific.programmes , Volunteering4
Volunteering -> Volunteering.policy , Volunteering5
Red.Cross.Red.Crescent.Law <-> Red.Cross.Red.Crescent.Law , error1
Statutes <-> Statutes , error2
Emblem.Law <-> Emblem.Law , error3
Fundamental.Principles <-> Fundamental.Principles , error4
Strategic.Plan <-> Strategic.Plan , error5
Autonomy...choice.of.leadership.at.the.Headquarters.level <-> Autonomy...choice.of.leadership.at.the.Headquarters.level , error6
Autonomy...choice.of.leadership.at.branch.level <-> Autonomy...choice.of.leadership.at.branch.level , error7
Autonomy...programmes.and.interventions <-> Autonomy...programmes.and.interventions , error8
Autonomy...Geographical.coverage <-> Autonomy...Geographical.coverage , error9
Policies <-> Policies , error10
Geographical.coverage <-> Geographical.coverage , error11
Staff...volunteer.and.member.composition <-> Staff...volunteer.and.member.composition , error12
Working.conditions <-> Working.conditions , error13
Staffing.structure <-> Staffing.structure , error14
Recruitment.procedure <-> Recruitment.procedure , error15
Recruitment.performance.and.surge.capacity <-> Recruitment.performance.and.surge.capacity , error16
Staff.development <-> Staff.development , error17
Job.satisfaction <-> Job.satisfaction , error18
Staff.compensation <-> Staff.compensation , error19
Volunteer.recruitment.and.retention <-> Volunteer.recruitment.and.retention , error20
Volunteering.policy <-> Volunteering.policy , error21
Volunteer.specific.programmes <-> Volunteer.specific.programmes , error22
Volunteer.recognition <-> Volunteer.recognition , error23
Volunteer.records.database <-> Volunteer.records.database , error24
Security..Safety.Management <-> Security..Safety.Management , error25
Security.Safety.Training <-> Security.Safety.Training , error26
Security.Safety..culture <-> Security.Safety..culture , error27
Violence...abuse.of.power.prevention <-> Violence...abuse.of.power.prevention , error28
Buildings <-> Buildings , error29
Fleet <-> Fleet , error30
Internal.management.and.service.delivery <-> Internal.management.and.service.delivery , error31
Internal.and.external.communications <-> Internal.and.external.communications , error32
Procurement.and.mobilisation.of.goods.and.services <-> Procurement.and.mobilisation.of.goods.and.services , error33
Warehousing <-> Warehousing , error34
Fleet.management <-> Fleet.management , error35
Independent.advice.and.oversight <-> Independent.advice.and.oversight , error36
Financial.Reporting <-> Financial.Reporting , error37
Finance.staff <-> Finance.staff , error38
Budgeting <-> Budgeting , error39
Expenditure.Authorisation <-> Expenditure.Authorisation , error40
Financial.Information.Systems <-> Financial.Information.Systems , error41
Documented.processes.and.procedures <-> Documented.processes.and.procedures , error42
Treasury.Management <-> Treasury.Management , error43
Consolidation <-> Consolidation , error44
External.Audit <-> External.Audit , error45
Internal.Control.Audit <-> Internal.Control.Audit , error46
Filing.and.archives <-> Filing.and.archives , error47
Legal.compliance <-> Legal.compliance , error48
Contract.Management <-> Contract.Management , error49
Core.accountability.principles <-> Core.accountability.principles , error50
Internal.accountability.principles <-> Internal.accountability.principles , error51
Stakeholder.specific.standards <-> Stakeholder.specific.standards , error52
Planning.and.development <-> Planning.and.development , error53
Constituency.empowerment <-> Constituency.empowerment , error54
Internal.Communication <-> Internal.Communication , error55
Volunteer.involvement.in.planning.and.decision.making <-> Volunteer.involvement.in.planning.and.decision.making , error56
Member.involvement.in.planning.and.decision.making <-> Member.involvement.in.planning.and.decision.making , error57
Delegation.of.management.responsibilities <-> Delegation.of.management.responsibilities , error58
Branding <-> Branding , error59
Public.image <-> Public.image , error60
Reporting <-> Reporting , error61
Communications.during.emergencies <-> Communications.during.emergencies , error62
Beneficiary.communication.during.emergencies <-> Beneficiary.communication.during.emergencies , error63
External.communication <-> External.communication , error64
Donor.mobilisation <-> Donor.mobilisation , error65
Participation.in.Movement.events <-> Participation.in.Movement.events , error66
External.environment <-> External.environment , error67
Unrestricted.income.diversification.and.stability <-> Unrestricted.income.diversification.and.stability , error68
Unrestricted.funding.over.core.budget <-> Unrestricted.funding.over.core.budget , error69
Restricted.income.diversification.and.stability <-> Restricted.income.diversification.and.stability , error70
Unrestricted.reserves <-> Unrestricted.reserves , error71
Business.continuity <-> Business.continuity , error72
Unrestricted.working.capital <-> Unrestricted.working.capital , error73
RM.capacities <-> RM.capacities , error74
Unrestricted.fund.mobilisation <-> Unrestricted.fund.mobilisation , error75
Operational.planning <-> Operational.planning , error76
Sustainability.of.services <-> Sustainability.of.services , error77
Programme.descriptions <-> Programme.descriptions , error78
Overheads <-> Overheads , error79
Needs.assessment <-> Needs.assessment , error80
Stakeholders.satisfaction <-> Stakeholders.satisfaction , error81
Monitoring.against.objectives.and.budgets <-> Monitoring.against.objectives.and.budgets , error82
Evaluations <-> Evaluations , error83
Planning..Monitoring..Evaluation.and.Reporting..PMER..capacities <-> Planning..Monitoring..Evaluation.and.Reporting..PMER..capacities , error84
Investment.in.innovative.action <-> Investment.in.innovative.action , error85
Investing.in.skills <-> Investing.in.skills , error86
Succession.management <-> Succession.management , error87
Integrity.framework <-> Integrity.framework , error88
Reputational.risk.management <-> Reputational.risk.management , error89
Insurance <-> Insurance , error90
Auxiliary.role <-> Auxiliary.role , error91
Humanitarian.Diplomacy <-> Humanitarian.Diplomacy , error92
Governance <-> Governance , error93
Governance.â...Senior.Management.complementarity <-> Governance.â...Senior.Management.complementarity , error94
AdminLegal <-> RCRCidentity , Factorcov20
AdminLegal <-> Autonomy , Factorcov21
AdminLegal <-> Basicoperations , Factorcov22
AdminLegal <-> HumanResources , Factorcov23
AdminLegal <-> Volunteering , Factorcov24
AdminLegal <-> SecurityandSafety , Factorcov25
AdminLegal <-> Infrastructure , Factorcov26
AdminLegal <-> Logisticsprocurement , Factorcov27
AdminLegal <-> Finance , Factorcov28
AdminLegal <-> AdminLegal ,NA, 1
AdminLegal <-> Internalstakeholderengamentdecisionmaking , Factorcov31
AdminLegal <-> Externalcommunication , Factorcov32
AdminLegal <-> FinancialResourceMobilisation , Factorcov33
AdminLegal <-> Programdevelopment , Factorcov34
AdminLegal <-> Foresightplanningforthefuture , Factorcov35
AdminLegal <-> RiskManagement , Factorcov36
AdminLegal <-> Relationshipwithgovernment , Factorcov37
AdminLegal <-> LeadershipGovernance , Factorcov38
Autonomy <-> RCRCidentity , Factorcov39
Autonomy <-> Autonomy , NA, 1
Autonomy <-> Basicoperations , Factorcov41
Autonomy <-> HumanResources , Factorcov42
Autonomy <-> Volunteering , Factorcov43
Autonomy <-> SecurityandSafety , Factorcov44
Autonomy <-> Infrastructure , Factorcov45
Autonomy <-> Logisticsprocurement , Factorcov46
Autonomy <-> Finance , Factorcov47
Autonomy <-> AdminLegal , Factorcov48
Autonomy <-> Internalstakeholderengamentdecisionmaking , Factorcov50
Autonomy <-> Externalcommunication , Factorcov51
Autonomy <-> FinancialResourceMobilisation , Factorcov52
Autonomy <-> Programdevelopment , Factorcov53
Autonomy <-> Foresightplanningforthefuture , Factorcov54
Autonomy <-> RiskManagement , Factorcov55
Autonomy <-> Relationshipwithgovernment , Factorcov56
Autonomy <-> LeadershipGovernance , Factorcov57
Basicoperations <-> RCRCidentity , Factorcov58
Basicoperations <-> Autonomy , Factorcov59
Basicoperations <-> Basicoperations ,NA, 1
Basicoperations <-> HumanResources , Factorcov61
Basicoperations <-> Volunteering , Factorcov62
Basicoperations <-> SecurityandSafety , Factorcov63
Basicoperations <-> Infrastructure , Factorcov64
Basicoperations <-> Logisticsprocurement , Factorcov65
Basicoperations <-> Finance , Factorcov66
Basicoperations <-> AdminLegal , Factorcov67
Basicoperations <-> Internalstakeholderengamentdecisionmaking , Factorcov69
Basicoperations <-> Externalcommunication , Factorcov70
Basicoperations <-> FinancialResourceMobilisation , Factorcov71
Basicoperations <-> Programdevelopment , Factorcov72
Basicoperations <-> Foresightplanningforthefuture , Factorcov73
Basicoperations <-> RiskManagement , Factorcov74
Basicoperations <-> Relationshipwithgovernment , Factorcov75
Basicoperations <-> LeadershipGovernance , Factorcov76
Externalcommunication <-> RCRCidentity , Factorcov77
Externalcommunication <-> Autonomy , Factorcov78
Externalcommunication <-> Basicoperations , Factorcov79
Externalcommunication <-> HumanResources , Factorcov80
Externalcommunication <-> Volunteering , Factorcov81
Externalcommunication <-> SecurityandSafety , Factorcov82
Externalcommunication <-> Infrastructure , Factorcov83
Externalcommunication <-> Logisticsprocurement , Factorcov84
Externalcommunication <-> Finance , Factorcov85
Externalcommunication <-> AdminLegal , Factorcov86
Externalcommunication <-> Internalstakeholderengamentdecisionmaking , Factorcov88
Externalcommunication <-> Externalcommunication , NA, 1
Externalcommunication <-> FinancialResourceMobilisation , Factorcov90
Externalcommunication <-> Programdevelopment , Factorcov91
Externalcommunication <-> Foresightplanningforthefuture , Factorcov92
Externalcommunication <-> RiskManagement , Factorcov93
Externalcommunication <-> Relationshipwithgovernment , Factorcov94
Externalcommunication <-> LeadershipGovernance , Factorcov95
Finance <-> RCRCidentity , Factorcov96
Finance <-> Autonomy , Factorcov97
Finance <-> Basicoperations , Factorcov98
Finance <-> HumanResources , Factorcov99
Finance <-> Volunteering , Factorcov100
Finance <-> SecurityandSafety , Factorcov101
Finance <-> Infrastructure , Factorcov102
Finance <-> Logisticsprocurement , Factorcov103
Finance <-> Finance , NA, 1
Finance <-> AdminLegal , Factorcov105
Finance <-> Internalstakeholderengamentdecisionmaking , Factorcov107
Finance <-> Externalcommunication , Factorcov108
Finance <-> FinancialResourceMobilisation , Factorcov109
Finance <-> Programdevelopment , Factorcov110
Finance <-> Foresightplanningforthefuture , Factorcov111
Finance <-> RiskManagement , Factorcov112
Finance <-> Relationshipwithgovernment , Factorcov113
Finance <-> LeadershipGovernance , Factorcov114
FinancialResourceMobilisation <-> RCRCidentity , Factorcov115
FinancialResourceMobilisation <-> Autonomy , Factorcov116
FinancialResourceMobilisation <-> Basicoperations , Factorcov117
FinancialResourceMobilisation <-> HumanResources , Factorcov118
FinancialResourceMobilisation <-> Volunteering , Factorcov119
FinancialResourceMobilisation <-> SecurityandSafety , Factorcov120
FinancialResourceMobilisation <-> Infrastructure , Factorcov121
FinancialResourceMobilisation <-> Logisticsprocurement , Factorcov122
FinancialResourceMobilisation <-> Finance , Factorcov123
FinancialResourceMobilisation <-> AdminLegal , Factorcov124
FinancialResourceMobilisation <-> Internalstakeholderengamentdecisionmaking , Factorcov126
FinancialResourceMobilisation <-> Externalcommunication , Factorcov127
FinancialResourceMobilisation <-> FinancialResourceMobilisation ,NA, 1
FinancialResourceMobilisation <-> Programdevelopment , Factorcov129
FinancialResourceMobilisation <-> Foresightplanningforthefuture , Factorcov130
FinancialResourceMobilisation <-> RiskManagement , Factorcov131
FinancialResourceMobilisation <-> Relationshipwithgovernment , Factorcov132
FinancialResourceMobilisation <-> LeadershipGovernance , Factorcov133
HumanResources <-> RCRCidentity , Factorcov153
HumanResources <-> Autonomy , Factorcov154
HumanResources <-> Basicoperations , Factorcov155
HumanResources <-> HumanResources , NA, 1
HumanResources <-> Volunteering , Factorcov157
HumanResources <-> SecurityandSafety , Factorcov158
HumanResources <-> Infrastructure , Factorcov159
HumanResources <-> Logisticsprocurement , Factorcov160
HumanResources <-> Finance , Factorcov161
HumanResources <-> AdminLegal , Factorcov162
HumanResources <-> Internalstakeholderengamentdecisionmaking , Factorcov164
HumanResources <-> Externalcommunication , Factorcov165
HumanResources <-> FinancialResourceMobilisation , Factorcov166
HumanResources <-> Programdevelopment , Factorcov167
HumanResources <-> RiskManagement , Factorcov169
HumanResources <-> Relationshipwithgovernment , Factorcov170
HumanResources <-> LeadershipGovernance , Factorcov171
Infrastructure <-> RCRCidentity , Factorcov172
Infrastructure <-> Autonomy , Factorcov173
Infrastructure <-> Basicoperations , Factorcov174
Infrastructure <-> HumanResources , Factorcov175
Infrastructure <-> Volunteering , Factorcov176
Infrastructure <-> SecurityandSafety , Factorcov177
Infrastructure <-> Infrastructure , NA, 1
Infrastructure <-> Logisticsprocurement , Factorcov179
Infrastructure <-> Finance , Factorcov180
Infrastructure <-> AdminLegal , Factorcov181
Infrastructure <-> Internalstakeholderengamentdecisionmaking , Factorcov183
Infrastructure <-> Externalcommunication , Factorcov184
Infrastructure <-> FinancialResourceMobilisation , Factorcov185
Infrastructure <-> Programdevelopment , Factorcov186
Infrastructure <-> RiskManagement , Factorcov188
Infrastructure <-> Relationshipwithgovernment , Factorcov189
Infrastructure <-> LeadershipGovernance , Factorcov190
Internalstakeholderengamentdecisionmaking <-> RCRCidentity , Factorcov191
Internalstakeholderengamentdecisionmaking <-> Autonomy , Factorcov192
Internalstakeholderengamentdecisionmaking <-> Basicoperations , Factorcov193
Internalstakeholderengamentdecisionmaking <-> HumanResources , Factorcov194
Internalstakeholderengamentdecisionmaking <-> Volunteering , Factorcov195
Internalstakeholderengamentdecisionmaking <-> SecurityandSafety , Factorcov196
Internalstakeholderengamentdecisionmaking <-> Infrastructure , Factorcov197
Internalstakeholderengamentdecisionmaking <-> Logisticsprocurement , Factorcov198
Internalstakeholderengamentdecisionmaking <-> Finance , Factorcov199
Internalstakeholderengamentdecisionmaking <-> AdminLegal , Factorcov200
Internalstakeholderengamentdecisionmaking <-> Internalstakeholderengamentdecisionmaking , NA, 1
Internalstakeholderengamentdecisionmaking <-> Externalcommunication , Factorcov203
Internalstakeholderengamentdecisionmaking <-> FinancialResourceMobilisation , Factorcov204
Internalstakeholderengamentdecisionmaking <-> Programdevelopment , Factorcov205
Internalstakeholderengamentdecisionmaking <-> Foresightplanningforthefuture , Factorcov206
Internalstakeholderengamentdecisionmaking <-> RiskManagement , Factorcov207
Internalstakeholderengamentdecisionmaking <-> Relationshipwithgovernment , Factorcov208
Internalstakeholderengamentdecisionmaking <-> LeadershipGovernance , Factorcov209
LeadershipGovernance <-> RCRCidentity , Factorcov210
LeadershipGovernance <-> Autonomy , Factorcov211
LeadershipGovernance <-> Basicoperations , Factorcov212
LeadershipGovernance <-> HumanResources , Factorcov213
LeadershipGovernance <-> Volunteering , Factorcov214
LeadershipGovernance <-> SecurityandSafety , Factorcov215
LeadershipGovernance <-> Infrastructure , Factorcov216
LeadershipGovernance <-> Logisticsprocurement , Factorcov217
LeadershipGovernance <-> Finance , Factorcov218
LeadershipGovernance <-> AdminLegal , Factorcov219
LeadershipGovernance <-> Internalstakeholderengamentdecisionmaking , Factorcov221
LeadershipGovernance <-> Externalcommunication , Factorcov222
LeadershipGovernance <-> FinancialResourceMobilisation , Factorcov223
LeadershipGovernance <-> Programdevelopment , Factorcov224
LeadershipGovernance <-> RiskManagement , Factorcov226
LeadershipGovernance <-> Relationshipwithgovernment , Factorcov227
LeadershipGovernance <-> LeadershipGovernance , NA, 1
Logisticsprocurement <-> RCRCidentity , Factorcov229
Logisticsprocurement <-> Autonomy , Factorcov230
Logisticsprocurement <-> Basicoperations , Factorcov231
Logisticsprocurement <-> HumanResources , Factorcov232
Logisticsprocurement <-> Volunteering , Factorcov233
Logisticsprocurement <-> SecurityandSafety , Factorcov234
Logisticsprocurement <-> Infrastructure , Factorcov235
Logisticsprocurement <-> Logisticsprocurement , NA, 1
Logisticsprocurement <-> Finance , Factorcov237
Logisticsprocurement <-> AdminLegal , Factorcov238
Logisticsprocurement <-> Internalstakeholderengamentdecisionmaking , Factorcov240
Logisticsprocurement <-> Externalcommunication , Factorcov241
Logisticsprocurement <-> FinancialResourceMobilisation , Factorcov242
Logisticsprocurement <-> Programdevelopment , Factorcov243
Logisticsprocurement <-> Foresightplanningforthefuture , Factorcov244
Logisticsprocurement <-> Relationshipwithgovernment , Factorcov246
Logisticsprocurement <-> LeadershipGovernance , Factorcov247
Programdevelopment <-> RCRCidentity , Factorcov248
Programdevelopment <-> Autonomy , Factorcov249
Programdevelopment <-> Basicoperations , Factorcov250
Programdevelopment <-> HumanResources , Factorcov251
Programdevelopment <-> Volunteering , Factorcov252
Programdevelopment <-> SecurityandSafety , Factorcov253
Programdevelopment <-> Infrastructure , Factorcov254
Programdevelopment <-> Logisticsprocurement , Factorcov255
Programdevelopment <-> Finance , Factorcov256
Programdevelopment <-> AdminLegal , Factorcov257
Programdevelopment <-> Internalstakeholderengamentdecisionmaking , Factorcov259
Programdevelopment <-> Externalcommunication , Factorcov260
Programdevelopment <-> FinancialResourceMobilisation , Factorcov261
Programdevelopment <-> Programdevelopment , NA, 1
Programdevelopment <-> RiskManagement , Factorcov264
Programdevelopment <-> Relationshipwithgovernment , Factorcov265
Programdevelopment <-> LeadershipGovernance , Factorcov266
RCRCidentity <-> RCRCidentity , NA, 1
RCRCidentity <-> Autonomy , Factorcov268
RCRCidentity <-> Basicoperations , Factorcov269
RCRCidentity <-> HumanResources , Factorcov270
RCRCidentity <-> Volunteering , Factorcov271
RCRCidentity <-> SecurityandSafety , Factorcov272
RCRCidentity <-> Infrastructure , Factorcov273
RCRCidentity <-> Logisticsprocurement , Factorcov274
RCRCidentity <-> Finance , Factorcov275
RCRCidentity <-> AdminLegal , Factorcov276
RCRCidentity <-> Internalstakeholderengamentdecisionmaking , Factorcov278
RCRCidentity <-> Externalcommunication , Factorcov279
RCRCidentity <-> FinancialResourceMobilisation , Factorcov280
RCRCidentity <-> Programdevelopment , Factorcov281
RCRCidentity <-> RiskManagement , Factorcov283
RCRCidentity <-> Relationshipwithgovernment , Factorcov284
RCRCidentity <-> LeadershipGovernance , Factorcov285
Relationshipwithgovernment <-> RCRCidentity , Factorcov286
Relationshipwithgovernment <-> Autonomy , Factorcov287
Relationshipwithgovernment <-> Basicoperations , Factorcov288
Relationshipwithgovernment <-> HumanResources , Factorcov289
Relationshipwithgovernment <-> Volunteering , Factorcov290
Relationshipwithgovernment <-> SecurityandSafety , Factorcov291
Relationshipwithgovernment <-> Infrastructure , Factorcov292
Relationshipwithgovernment <-> Logisticsprocurement , Factorcov293
Relationshipwithgovernment <-> Finance , Factorcov294
Relationshipwithgovernment <-> AdminLegal , Factorcov295
Relationshipwithgovernment <-> Internalstakeholderengamentdecisionmaking , Factorcov297
Relationshipwithgovernment <-> Externalcommunication , Factorcov298
Relationshipwithgovernment <-> FinancialResourceMobilisation , Factorcov299
Relationshipwithgovernment <-> Programdevelopment , Factorcov300
Relationshipwithgovernment <-> RiskManagement , Factorcov302
Relationshipwithgovernment <-> Relationshipwithgovernment , NA, 1
Relationshipwithgovernment <-> LeadershipGovernance , Factorcov304
RiskManagement <-> RCRCidentity , Factorcov305
RiskManagement <-> Autonomy , Factorcov306
RiskManagement <-> Basicoperations , Factorcov307
RiskManagement <-> HumanResources , Factorcov308
RiskManagement <-> Volunteering , Factorcov309
RiskManagement <-> SecurityandSafety , Factorcov310
RiskManagement <-> Infrastructure , Factorcov311
RiskManagement <-> Logisticsprocurement , Factorcov312
RiskManagement <-> Finance , Factorcov313
RiskManagement <-> AdminLegal , Factorcov314
RiskManagement <-> Internalstakeholderengamentdecisionmaking , Factorcov316
RiskManagement <-> Externalcommunication , Factorcov317
RiskManagement <-> FinancialResourceMobilisation , Factorcov318
RiskManagement <-> Programdevelopment , Factorcov319
RiskManagement <-> Foresightplanningforthefuture , Factorcov320
RiskManagement <-> RiskManagement , NA, 1
RiskManagement <-> Relationshipwithgovernment , Factorcov322
RiskManagement <-> LeadershipGovernance , Factorcov323
SecurityandSafety <-> RCRCidentity , Factorcov324
SecurityandSafety <-> Autonomy , Factorcov325
SecurityandSafety <-> Basicoperations , Factorcov326
SecurityandSafety <-> HumanResources , Factorcov327
SecurityandSafety <-> Volunteering , Factorcov328
SecurityandSafety <-> SecurityandSafety ,NA, 1
SecurityandSafety <-> Infrastructure , Factorcov330
SecurityandSafety <-> Logisticsprocurement , Factorcov331
SecurityandSafety <-> Finance , Factorcov332
SecurityandSafety <-> AdminLegal , Factorcov333
SecurityandSafety <-> Accountability , Factorcov334
SecurityandSafety <-> Internalstakeholderengamentdecisionmaking , Factorcov335
SecurityandSafety <-> Externalcommunication , Factorcov336
SecurityandSafety <-> FinancialResourceMobilisation , Factorcov337
SecurityandSafety <-> Programdevelopment , Factorcov338
SecurityandSafety <-> Foresightplanningforthefuture , Factorcov339
SecurityandSafety <-> RiskManagement , Factorcov340
SecurityandSafety <-> Relationshipwithgovernment , Factorcov341
SecurityandSafety <-> LeadershipGovernance , Factorcov342
Volunteering <-> RCRCidentity , Factorcov343
Volunteering <-> Autonomy , Factorcov344
Volunteering <-> Basicoperations , Factorcov345
Volunteering <-> HumanResources , Factorcov346
Volunteering <-> Volunteering , NA, 1
Volunteering <-> SecurityandSafety , Factorcov348
Volunteering <-> Infrastructure , Factorcov349
Volunteering <-> Logisticsprocurement , Factorcov350
Volunteering <-> Finance , Factorcov351
Volunteering <-> AdminLegal , Factorcov352
Volunteering <-> Accountability , Factorcov353
Volunteering <-> Internalstakeholderengamentdecisionmaking , Factorcov354
Volunteering <-> Externalcommunication , Factorcov355
Volunteering <-> FinancialResourceMobilisation , Factorcov356
Volunteering <-> Programdevelopment , Factorcov357
Volunteering <-> Foresightplanningforthefuture , Factorcov358
Volunteering <-> RiskManagement , Factorcov359
Volunteering <-> Relationshipwithgovernment , Factorcov360
Volunteering <-> LeadershipGovernance , Factorcov361

cfa1 <- sem(cfaModel1, binarycovmatrix, nrow(na.omit(OCACcNumberical)))

#######aleternativ cfa using lavaan#####

library(lavaan)


cfaModel2 <- ' AdminLegal=~Contract.Management+Filing.and.archives+Legal.compliance
                Autonomy=~Autonomy...choice.of.leadership.at.the.Headquarters.level+Autonomy...Geographical.coverage+Autonomy...programmes.and.interventions
                Basicoperations=~Geographical.coverage+Policies
                Externalcommunication=~Branding+Communications.during.emergencies+Donor.mobilisation+External.communication+External.environment+Participation.in.Movement.events+Public.image+Reporting
                Finance=~Budgeting+Consolidation+Documented.processes.and.procedures+Expenditure.Authorisation+External.Audit+Finance.staff+Financial.Information.Systems+Financial.Reporting+Independent.advice.and.oversight+Internal.Control.Audit+Treasury.Management
                FinancialResourceMobilisation=~Business.continuity+Restricted.income.diversification.and.stability+RM.capacities+Unrestricted.fund.mobilisation+Unrestricted.funding.over.core.budget+Unrestricted.income.diversification.and.stability+Unrestricted.reserves+Unrestricted.working.capital
                HumanResources=~Job.satisfaction+Recruitment.performance.and.surge.capacity+Recruitment.procedure+Staff...volunteer.and.member.composition+Staff.compensation+Staff.development+Staffing.structure
                Infrastructure=~Buildings+Fleet+Internal.and.external.communications+Internal.management.and.service.delivery
                Internalstakeholderengamentdecisionmaking=~Constituency.empowerment+Delegation.of.management.responsibilities+Internal.Communication+Member.involvement.in.planning.and.decision.making+Volunteer.involvement.in.planning.and.decision.making
                LeadershipGovernance=~Governance+Governance.â...Senior.Management.complementarity
                Logisticsprocurement=~Fleet.management+Procurement.and.mobilisation.of.goods.and.services+Warehousing
                Programdevelopment=~Evaluations+Monitoring.against.objectives.and.budgets+Needs.assessment+Operational.planning+Overheads+Planning..Monitoring..Evaluation.and.Reporting..PMER..capacities+Programme.descriptions+Stakeholders.satisfaction+Sustainability.of.services
                RCRCidentity=~Emblem.Law+Fundamental.Principles+Red.Cross.Red.Crescent.Law+Statutes+Strategic.Plan
                Relationshipwithgovernment=~Auxiliary.role+Humanitarian.Diplomacy
                RiskManagement=~Insurance+Integrity.framework+Reputational.risk.management
                SecurityandSafety=~Security..Safety.Management+Security.Safety..culture+Security.Safety.Training+Violence...abuse.of.power.prevention+Working.conditions
                Volunteering=~Volunteer.recognition+Volunteer.records.database+Volunteer.recruitment.and.retention+Volunteer.specific.programmes+Volunteering.policy '


fit6 <- cfa(cfaModel2, sample.cov = binarycovmatrix, sample.nobs = 49)
fit6b <- cfa(cfaModel2, sample.cov = binarycovmatrix, sample.nobs = 49)


fit5 <- sem(cfaModel2, sample.cov = binarycovmatrix, sample.nobs = 49)


cfaModel3 <-  ' FinancialResourceMobilisation=~Unrestricted.income.diversification.and.stability+Unrestricted.funding.over.core.budget+Restricted.income.diversification.and.stability+Unrestricted.reserves+Business.continuity+Unrestricted.working.capital+Overheads+RM.capacities+Unrestricted.fund.mobilisation+Insurance+Fleet+Staffing.structure
                RCRCidentity=~Red.Cross.Red.Crescent.Law+Statutes+Emblem.Law+Fundamental.Principles+Strategic.Plan+Internal.Communication+Policies+Geographical.coverage
                Autonomy=~Autonomy...choice.of.leadership.at.the.Headquarters.level+Autonomy...choice.of.leadership.at.branch.level+Autonomy...programmes.and.interventions+Autonomy...Geographical.coverage
                ExternalCommunications=~Branding+Public.image+Communications.during.emergencies+Internal.and.external.communications+Auxiliary.role+Donor.mobilisation+Beneficiary.communication.during.emergencies+External.environment
                SecurityAndSafety=~Security..Safety.Management+Security.Safety.Training+Security.Safety..culture
                HumanResourcesManagement=~Recruitment.procedure+Recruitment.performance.and.surge.capacity+Staff.development+Job.satisfaction+Staff.compensation+Delegation.of.management.responsibilities
                RiskManagement=~Integrity.framework+Reputational.risk.management+Violence...abuse.of.power.prevention+Independent.advice.and.oversight+External.Audit+Internal.Control.Audit
                PMER=~Needs.assessment+Operational.planning+Sustainability.of.services+Monitoring.against.objectives.and.budgets+Stakeholders.satisfaction+Reporting+Evaluations+Planning..Monitoring..Evaluation.and.Reporting..PMER..capacities
                VolunteeringCultureAndManagement=~Volunteer.recruitment.and.retention+Volunteer.specific.programmes+Volunteering.policy+Volunteer.recognition+Volunteer.records.database+Volunteer.involvement.in.planning.and.decision.making
                FinancialManagement=~Financial.Reporting+Finance.staff+Budgeting+Expenditure.Authorisation+Financial.Information.Systems+Documented.processes.and.procedures+Treasury.Management+Consolidation
                GovernanceCapacity=~Governance+Member.involvement.in.planning.and.decision.making+Constituency.empowerment+Governance.â...Senior.Management.complementarity
                Logistics=~Procurement.and.mobilisation.of.goods.and.services+Warehousing+Fleet.management '

fit7 <- cfa(cfaModel3, sample.cov = binarycovmatrixforCFA, sample.nobs = 49)
fit7b <- cfa(cfaModel3, sample.cov = binarycovmatrixforCFA, sample.nobs = 49, std.lv=TRUE)
fit8 <- cfa(cfaModel3, D1)
fit8b <- cfa(cfaModel3, D1, std.lv=TRUE)

f(p1 <- predict(fit7, newdata=D1)
p2 <- predict(fit7, newdata=D2)
p3 <- predict(fit7, newdata=D3)
p4 <- predict(fit7, newdata=D4)
p5 <- predict(fit7, newdata=D5)
FactorScores <- (p1 + p2 + p3 + p4 + p5)/5 

p1b <- predict(fit7b, newdata=D1)
p2b <- predict(fit7b, newdata=D2)
p3b <- predict(fit7b, newdata=D3)
p4b <- predict(fit7b, newdata=D4)
p5b <- predict(fit7b, newdata=D5)
FactorScoresb <- (p1b + p2b + p3b + p4b + p5b)/5 



####cfa 2nd round ####

cfaModel4 <-  '  FinancialResourceMobilisation=~Unrestricted.income.diversification.and.stability+Unrestricted.funding.over.core.budget+Restricted.income.diversification.and.stability+Unrestricted.reserves+Business.continuity+Unrestricted.working.capital+RM.capacities+Unrestricted.fund.mobilisation+Staffing.structure
RCRCidentity=~Statutes+Emblem.Law+Fundamental.Principles+Internal.Communication+Policies+Geographical.coverage
Autonomy=~Autonomy...choice.of.leadership.at.the.Headquarters.level+Autonomy...programmes.and.interventions+Autonomy...Geographical.coverage
ExternalCommunications=~Public.image+Communications.during.emergencies+Auxiliary.role+Beneficiary.communication.during.emergencies+External.environment
SecurityAndSafety=~Security..Safety.Management+Security.Safety.Training+Security.Safety..culture
HumanResourcesManagement=~Recruitment.procedure+Recruitment.performance.and.surge.capacity+Staff.development+Job.satisfaction+Staff.compensation+Delegation.of.management.responsibilities
RiskManagement=~Integrity.framework+Reputational.risk.management+Violence...abuse.of.power.prevention
PMER=~Needs.assessment+Operational.planning+Sustainability.of.services+Monitoring.against.objectives.and.budgets+Stakeholders.satisfaction+Reporting+Evaluations
VolunteeringCultureAndManagement=~Volunteering.policy+Volunteer.recognition+Volunteer.records.database+Volunteer.involvement.in.planning.and.decision.making
FinancialManagement=~Financial.Reporting+Finance.staff+Budgeting+Expenditure.Authorisation+Financial.Information.Systems+Documented.processes.and.procedures+Treasury.Management+Consolidation+Independent.advice.and.oversight+External.Audit+Internal.Control.Audit
GovernanceCapacity=~Governance+Member.involvement.in.planning.and.decision.making+Constituency.empowerment+Governance.â...Senior.Management.complementarity
Logistics=~Warehousing+Fleet.management '


fitModel4 <- cfa(cfaModel4, D1, std.lv=TRUE)


cfaModel4b <-  '  FinancialResourceMobilisation=~Unrestricted.income.diversification.and.stability+Unrestricted.funding.over.core.budget+Restricted.income.diversification.and.stability+Unrestricted.reserves+Business.continuity+Unrestricted.working.capital+RM.capacities+Unrestricted.fund.mobilisation+Staffing.structure
RCRCidentity=~Statutes+Emblem.Law+Fundamental.Principles+Internal.Communication+Policies
Autonomy=~Autonomy...choice.of.leadership.at.the.Headquarters.level+Autonomy...programmes.and.interventions+Autonomy...Geographical.coverage
ExternalCommunications=~Public.image+Communications.during.emergencies+Auxiliary.role+Beneficiary.communication.during.emergencies+External.environment
SecurityAndSafety=~Security..Safety.Management+Security.Safety.Training+Security.Safety..culture
HumanResourcesManagement=~Recruitment.procedure+Recruitment.performance.and.surge.capacity+Staff.development+Job.satisfaction+Staff.compensation+Delegation.of.management.responsibilities
RiskManagement=~Integrity.framework+Reputational.risk.management+Violence...abuse.of.power.prevention
PMER=~Needs.assessment+Operational.planning+Sustainability.of.services+Monitoring.against.objectives.and.budgets+Reporting+Evaluations
VolunteeringCultureAndManagement=~Volunteering.policy+Volunteer.recognition+Volunteer.records.database+Volunteer.involvement.in.planning.and.decision.making
FinancialManagement=~Financial.Reporting+Finance.staff+Budgeting+Expenditure.Authorisation+Financial.Information.Systems+Documented.processes.and.procedures+Treasury.Management+Consolidation+Independent.advice.and.oversight+External.Audit+Internal.Control.Audit
GovernanceCapacity=~Governance+Member.involvement.in.planning.and.decision.making+Constituency.empowerment+Governance.â...Senior.Management.complementarity
Logistics=~Warehousing+Fleet.management '

fitModel4b <- cfa(cfaModel4b, D1, std.lv=TRUE)
summary(fitModel4b)

p1 <- predict(fitModel4b, newdata=D1)
p2 <- predict(fitModel4b, newdata=D2)
p3 <- predict(fitModel4b, newdata=D3)
p4 <- predict(fitModel4b, newdata=D4)
p5 <- predict(fitModel4b, newdata=D5)
FactorScores4b <- (p1 + p2 + p3 + p4 + p5)/5 



cfaModel4c <-  '  FinancialResourceMobilisation=~Unrestricted.income.diversification.and.stability+Unrestricted.funding.over.core.budget+Restricted.income.diversification.and.stability+Unrestricted.reserves+Business.continuity+Unrestricted.working.capital+RM.capacities+Unrestricted.fund.mobilisation+Staffing.structure
RCRCidentity=~Statutes+Emblem.Law+Fundamental.Principles+Internal.Communication+Policies
Autonomy=~Autonomy...choice.of.leadership.at.the.Headquarters.level+Autonomy...programmes.and.interventions+Autonomy...Geographical.coverage
ExternalCommunications=~Public.image+Communications.during.emergencies+Auxiliary.role+Beneficiary.communication.during.emergencies
SecurityAndSafety=~Security..Safety.Management+Security.Safety.Training+Security.Safety..culture
HumanResourcesManagement=~Recruitment.procedure+Recruitment.performance.and.surge.capacity+Staff.development+Job.satisfaction+Staff.compensation
RiskManagement=~Integrity.framework+Reputational.risk.management+Violence...abuse.of.power.prevention
PMER=~Needs.assessment+Operational.planning+Sustainability.of.services+Monitoring.against.objectives.and.budgets+Reporting+Evaluations
VolunteeringCultureAndManagement=~Volunteering.policy+Volunteer.recognition+Volunteer.records.database+Volunteer.involvement.in.planning.and.decision.making
FinancialManagement=~Financial.Reporting+Finance.staff+Budgeting+Expenditure.Authorisation+Financial.Information.Systems+Documented.processes.and.procedures+Treasury.Management+Consolidation+Independent.advice.and.oversight+External.Audit+Internal.Control.Audit
GovernanceCapacity=~Governance+Member.involvement.in.planning.and.decision.making+Constituency.empowerment+Governance.â...Senior.Management.complementarity+Delegation.of.management.responsibilities
Logistics=~Warehousing+Fleet.management '
fitModel4c <- cfa(cfaModel4c, D1, std.lv=TRUE)
summary(fitModel4c)

#####cfa new data####

cfaModel4c_new <-  '  FinancialResourceMobilisation=~Unrestricted.income.diversification.and.stability+Unrestricted.funding.over.core.budget+Restricted.income.diversification.and.stability+Unrestricted.reserves+Business.continuity+Unrestricted.working.capital+RM.capacities+Unrestricted.fund.mobilisation+Staffing.structure
RCRCidentity=~Statutes+Emblem.Law+Fundamental.Principles+Internal.Communication+Policies
Autonomy=~Autonomy...choice.of.leadership.at.the.Headquarters.level+Autonomy...programmes.and.interventions+Autonomy...Geographical.coverage
ExternalCommunications=~Public.image+Communications.during.emergencies+Auxiliary.role+Beneficiary.communication.during.emergencies
SecurityAndSafety=~Security..Safety.Management+Security.Safety.Training+Security.Safety..culture
HumanResourcesManagement=~Recruitment.procedure+Recruitment.performance.and.surge.capacity+Staff.development+Job.satisfaction+Staff.compensation
RiskManagement=~Integrity.framework+Reputational.risk.management+Violence...abuse.of.power.prevention
PMER=~Needs.assessment+Operational.planning+Sustainability.of.services+Monitoring.against.objectives.and.budgets+Reporting+Evaluations
VolunteeringCultureAndManagement=~Volunteering.policy+Volunteer.recognition+Volunteer.records.database+Volunteer.involvement.in.planning.and.decision.making
FinancialManagement=~Financial.Reporting+Finance.staff+Budgeting+Expenditure.Authorisation+Financial.Information.Systems+Documented.processes.and.procedures+Treasury.Management+Consolidation+Independent.advice.and.oversight+External.Audit+Internal.Control.Audit
GovernanceCapacity=~Governance+Member.involvement.in.planning.and.decision.making+Constituency.empowerment+Governance.â...Senior.Management.complementarity+Delegation.of.management.responsibilities
Logistics=~Warehousing+Fleet.management '
fitModel4c_new <- cfa(cfaModel4c_new, D1_n, std.lv=TRUE)
summary(fitModel4c_new)

p1_n <- predict(fitModel4c_new, newdata=D1_n)
p2_n <- predict(fitModel4c_new, newdata=D2_n)
p3_n <- predict(fitModel4c_new, newdata=D3_n)
p4_n <- predict(fitModel4c_new, newdata=D4_n)
p5_n <- predict(fitModel4c_new, newdata=D5_n)
FactorScores_newdata <- (p1_n + p2_n + p3_n + p4_n + p5_n)/5 
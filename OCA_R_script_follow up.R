setwd("D:/Work/IFRC/OCAC")
library("xlsx")
library("corrplot")

OCACfollowUp <- read.xlsx("data for follow up analysis.xlsx",1)
OCACfollowUPGroups <-read.xlsx("data for follow up analysis - group scores.xlsx",1)
OCACfollowUpODA <-read.xlsx("ODA.xlsx",1)
OCACfollowUpIncome <- read.xlsx("rc_income_variables.xlsx",1)

mergeData <- merge(OCACfollowUp, OCACfollowUPGroups, by.x="Alpha2Countrycode", by.y="ISO.code", all.x=TRUE)
mergeData2 <- merge(mergeData, OCACfollowUpODA, by.x="Alpha2Countrycode", by.y="ISO.code", all.x=TRUE)
AllDataOCACfollowUp <- merge(mergeData2, OCACfollowUpIncome, by.x="Alpha2Countrycode", by.y="Alpha2Countrycode", all.x=TRUE)

  
reg1 <- lm(Percent.Of.Attributes.Cleared~Giving.Index+Income.In.Intl.Dollars+KPI.noPaidStaffPerCap+KPI.noPeopleDonatingBloodPerCap+KPI.noPeopleVolunteeringPerCap+Totalnumberofpeoplereportedaffected2002to2011+log(Cgdp)+Population.Log, data=OCACfollowUp)
summary(reg1)



OCACfollowUpVarOfIn <- AllDataOCACfollowUp[, c(226:237,175,238:239,47, 93, 103,102,116,158,180,184,84, 225,92,95, 240,242)]
OCACfollowUpRHSV <- AllDataOCACfollowUp[, c(175,238:239,47,102,116,158,180,184,84, 225,92,95, 240,242)]
OCACfollowUpLHSV <- AllDataOCACfollowUp[, c(226:237)]

corrplot(cor(na.omit(OCACfollowUpVarOfIn)), method = "number")

reg2 <- lm(AllDataOCACfollowUp[,c(226:237)]~., data=AllDataOCACfollowUp)



regG1<-lm(GS.Autonomy~., data=OCACfollowUpVarOfIn[,c(1,15:22)])                                                
regG2<-lm(GS.FinancialManagment~., data=OCACfollowUpVarOfIn[,c(2,15:22)])                                        
regG3<-lm(GS.GovernanceCapacity~., data=OCACfollowUpVarOfIn[,c(3,15:22)])                                        
regG4<-lm(GS.Externalcommunication~., data=OCACfollowUpVarOfIn[,c(4,15:22)])                                    
regG5<-lm(GS.Logistics~., data=OCACfollowUpVarOfIn[,c(5,15:22)])                                            
regG6<-lm(GS.PMER~., data=OCACfollowUpVarOfIn[,c(6,15:22)])                                      
regG7<-lm(GS.RCRCidentity~., data=OCACfollowUpVarOfIn[,c(7,15:22)])                                           
regG8<-lm(GS.VolunteeringCultureAndManagement~., data=OCACfollowUpVarOfIn[,c(8,15:22)])                           
regG9<-lm(GS.HumanResources~., data=OCACfollowUpVarOfIn[,c(9,15:22)])                            
regG10<-lm(GS.Financial.Resource.Mobilisation~., data=OCACfollowUpVarOfIn[,c(10,15:22)])                          
regG11<-lm(GS.SecurityandSafety~., data=OCACfollowUpVarOfIn[,c(11,15:22)])                                          
regG12<-lm(GS.RiskManagement~., data=OCACfollowUpVarOfIn[,c(12,15:22)])                                           

summary(regG1)
summary(regG2)
summary(regG3)
summary(regG4)
summary(regG5)
summary(regG6)
summary(regG7)
summary(regG8)
summary(regG9)
summary(regG10)
summary(regG11)
summary(regG12)


regG20<-lm(Percent.Of.Attributes.Cleared~., data=OCACfollowUpVarOfIn[,c(1:13)])  
summary(regG20)

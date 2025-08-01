### This script is to do a fast Anova Over For M13s comparison Data

library(readxl)
library(car)
library(MKinfer)
library(ggplot2)
library(lmPerm)
library(plyr)
library(lme4)
library(emmeans)

Conditions<-c("INS1 1032","INS1 1032 + 1031","INS1 1032 + 1600")

CellsInfo<-list()
for (i in 1:length(Conditions)) {
  CellsInfo[[i]]<- read_excel("D:/SubmissionM13 Paper/Revision Cell reports/M131vsM132/Exocytosis exp/CellsInfoOrganizeAll.xlsx",
                          sheet=Conditions[i])
  CellsInfo[[i]]$Condition<-Conditions[i]
  
}
names(CellsInfo)<-Conditions
Stats<-as.data.frame(CellsInfo[[1]])
for (i in 2:length(CellsInfo)) {
  Stats<-rbind(Stats,as.data.frame(CellsInfo[[i]]))
}
CellsInfo<-Stats
rm(Stats)
CellsInfo$Date<-as.factor(CellsInfo$Date)
CellsInfo$Condition<-as.factor(CellsInfo$Condition)
CellsInfo$Condition<-revalue(CellsInfo$Condition,c("INS1 1032"="INS1_NT","INS1 1032 + 1031"="INS1_M13-1","INS1 1032 + 1600"="INS1_M13-2"))


# For raw TotalExocytosis
shapiro.test(CellsInfo$TotalExo)
leveneTest(CellsInfo$TotalExo~ CellsInfo$Condition)

modelblock<-lmer(TotalExo~ Condition +(1|Date), data=CellsInfo)
summary(modelblock)

plot(fitted(modelblock), residuals(modelblock))
abline(h = 0, col = "red")

qqnorm(residuals(modelblock))
qqline(residuals(modelblock), col = "red")
shapiro.test(residuals(modelblock))

leveneTest(residuals(modelblock) ~ CellsInfo$Date)

qqnorm(unlist(ranef(modelblock)$Date))
qqline(unlist(ranef(modelblock)$Date), col = "red")

anova(modelblock)

posthoc <- emmeans(modelblock, pairwise ~ Condition)
summary(posthoc)


plotbox<-ggplot(CellsInfo,aes(y=TotalExo, group=Condition))+geom_boxplot(aes(fill=Condition))+
  ylab("Exocytosis/um^2")
plotbox

# For granules Density
modelblockgr<-lmer(grDensity~ Condition +(1|Date), data=CellsInfo)
summary(modelblock)

plot(fitted(modelblockgr), residuals(modelblockgr))
abline(h = 0, col = "red")

qqnorm(residuals(modelblockgr))
qqline(residuals(modelblockgr), col = "red")
shapiro.test(residuals(modelblockgr))

leveneTest(residuals(modelblockgr) ~ CellsInfo$Date)

qqnorm(unlist(ranef(modelblockgr)$Date))
qqline(unlist(ranef(modelblockgr)$Date), col = "red")

anova(modelblockgr)

posthoc <- emmeans(modelblockgr, pairwise ~ Condition)
summary(posthoc)


plotbox<-ggplot(CellsInfo,aes(y=grDensity, group=Condition))+geom_boxplot(aes(fill=Condition))+
  ylab("granules/um^2")+theme_classic()
plotbox
## priming Exocytosis/granules
CellsInfo$Priming<-CellsInfo$TotalExo/CellsInfo$grDensity
modelblockPriming<-lmer(Priming~ Condition +(1|Date), data=CellsInfo)
summary(modelblock)

plot(fitted(modelblockPriming), residuals(modelblockPriming))
abline(h = 0, col = "red")

qqnorm(residuals(modelblockPriming))
qqline(residuals(modelblockPriming), col = "red")
shapiro.test(residuals(modelblockPriming))

leveneTest(residuals(modelblockPriming) ~ CellsInfo$Date)

qqnorm(unlist(ranef(modelblockPriming)$Date))
qqline(unlist(ranef(modelblockPriming)$Date), col = "red")

anova(modelblockPriming)

posthoc <- emmeans(modelblockPriming, pairwise ~ Condition)
summary(posthoc)


plotbox<-ggplot(CellsInfo,aes(y=Priming, group=Condition))+geom_boxplot(aes(fill=Condition))+
  ylab("Exocytosis/granules")
plotbox
## Conclusion Total Exocytosis == Priming
## M13-2>M13-1>NT, But NT seems not to be significant from M13-1


#

CummulativeExo<-list()
for (i in 1:length(Conditions)) {
    CummulativeExo[[i]]<-read_excel("E:/SubmissionM13 Paper/Revision Cell reports/M131vsM132/Exocytosis exp/cumulativeExoAll.xlsx",sheet = Conditions[i])  
}
names(CummulativeExo)<-Conditions

NTmean<-apply(as.matrix(CummulativeExo[[1]]),1,mean)
NTSEM<-apply(as.matrix(CummulativeExo[[1]]),1,sd)/sqrt(dim(as.matrix(CummulativeExo[[1]]))[2])

M131mean<-apply(as.matrix(CummulativeExo[[2]]),1,mean)
M131SEM<-apply(as.matrix(CummulativeExo[[2]]),1,sd)/sqrt(dim(as.matrix(CummulativeExo[[2]]))[2])

M132mean<-apply(as.matrix(CummulativeExo[[3]]),1,mean)
M132SEM<-apply(as.matrix(CummulativeExo[[3]]),1,sd)/sqrt(dim(as.matrix(CummulativeExo[[3]]))[2])


x<-seq(0.1,50,0.1)
cumExoplot<- ggplot()+geom_ribbon(aes(x=x, ymin=NTmean-NTSEM,ymax=NTmean+NTSEM),color="#999999",fill="#999999")+
  geom_line(aes(x=x, y=NTmean),color="black", size=1)+
  geom_ribbon(aes(x=x, ymin=M131mean-M131SEM,ymax=M131mean+M131SEM),color="#ff9999",fill="#ff9999")+
  geom_line(aes(x=x, y=M131mean),color="red", size=1)+
  geom_ribbon(aes(x=x, ymin=M132mean-M132SEM,ymax=M132mean+M132SEM),color="#99cc33",fill="#99cc33")+
  geom_line(aes(x=x, y=M132mean),color="green", size=1)+
  ylab("Exocytosis/um^2")+
  xlab("time(s)")+geom_vline(xintercept = 10)+theme_classic()
  
cumExoplot





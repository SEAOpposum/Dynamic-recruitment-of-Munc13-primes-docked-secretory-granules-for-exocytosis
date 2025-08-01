## To analize DF and DF/S
### This script is to do a fast Anova Over For M13s comparison Data

library(readxl)
library(car)
library(MKinfer)
library(ggplot2)
library(lmPerm)
library(plyr)
library(lme4)
library(emmeans)
library(stringr)
library(dplyr)
library(rcompanion)
library(stats)

ABCorrection<-TRUE  #TRUE or FALSE
Conditions<-c("INS1 1032","INS1 1032 + 1031","INS1 1032 + 1600")
States<-c("E","F")

EventsSheet="SelectedEvents_DataAll.xlsx" # "SelectedEvents_DataAll.xlsx" 

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
#CellsInfo$Date<-as.factor(CellsInfo$Date)
CellsInfo$Condition<-as.factor(CellsInfo$Condition)
CellsInfo$Condition<-revalue(CellsInfo$Condition,c("INS1 1032"="INS1_NT","INS1 1032 + 1031"="INS1_M13-1","INS1 1032 + 1600"="INS1_M13-2"))
CellsInfo$ABOvCorrection<-rep(NA,dim(CellsInfo)[1],1)

### To get the AB correction
## the following script is to get the models for the linear relation of AB in batch mode.
## check that the directories (Disk letter is correct in all of the three codes)

if(ABCorrection==TRUE){
  modelsDirectories<- data.frame(Dates=c("250112","250114","250116"),Dir=
                                   c("D:/SubmissionM13 Paper/Revision Cell reports/M131vsM132/Exocytosis exp/Expression 250112",
                                     "D:/SubmissionM13 Paper/Revision Cell reports/M131vsM132/Exocytosis exp/Expression 250114",
                                     "D:/SubmissionM13 Paper/Revision Cell reports/M131vsM132/Exocytosis exp/Expression 250116"))
  for (i in 1:dim(modelsDirectories)[1]) {
    
    
    setwd(modelsDirectories[i,2])
    
    env<-new.env()
    
    source("Excelviewer4AllMunc13AB.R",local = env)
    
    m131<-env$m131mod
    m132<-env$m132mod
    nt<-env$NativeResults
    LPgreen<-env$greenlaser
    indx<-which(CellsInfo$Date==modelsDirectories[i,1])
    for (ii in 1:length(indx)) {
      if (CellsInfo$Condition[indx[ii]]=="INS1_M13-1"){
        
        x<-(CellsInfo$MeanCell[indx[ii]]-CellsInfo$MeanBg[indx[ii]])/LPgreen
        y<-(x*m131$coefficients[2])+m131$coefficients[1]
        OVplusSD<-(y/(nt[1,1]+nt[1,2]))#the prediction minus the mean Red+1SD
        CellsInfo$ABOvCorrection[indx[ii]]<-OVplusSD
      
        }else if(CellsInfo$Condition[indx[ii]]=="INS1_M13-2"){
          x<-(CellsInfo$MeanCell[indx[ii]]-CellsInfo$MeanBg[indx[ii]])/LPgreen
          y<-(x*m132$coefficients[2])+m132$coefficients[1]
        OVplusSD<-(y/(nt[1,1]+nt[1,2]))#the prediction minus the mean Red+1SD 
        CellsInfo$ABOvCorrection[indx[ii]]<-OVplusSD
      }else{
        CellsInfo$ABOvCorrection[indx[ii]]<-0
      }
    }
  }
  
  ## if lower than 0 == 1
  #CellsInfo$ABOvCorrection[CellsInfo$ABOvCorrection<1]<-1
  
}else{
  CellsInfo$ABOvCorrection<-rep(0,dim(CellsInfo)[1],1)
}

CellsInfo$Date<-as.factor(CellsInfo$Date)

### to open the events data
struktur<-data.frame(variable=c("ID_Cell_num","time","x","y","left c","left a","left c-a","left bg","left DF/S",
                                "right c","right a","right c-a","right bg","right DF/S",
                                "left DF align","left S align","right DF align","right s align"),
                     indexes=c("1","2","3","4","11:510","1011:1510","2011:2510","3011:3510","4011:4510",
                               "5011:5510","6011:6510","7011:7510","8011:8510","9011:9510",
                               "10011:10111","10211:10311","11011:10111","11211:11311"))

EventsData<-list()
for (i in 1:length(Conditions)) {
  EventsData[[i]]<- read_excel(paste("D:/SubmissionM13 Paper/Revision Cell reports/M131vsM132/Exocytosis exp/",EventsSheet,sep=""), 
                               sheet = Conditions[i], col_names = FALSE)
  
  
}
names(EventsData)<-Conditions
## the general function was up now to the specific treat
##

EventsFailureConditions<-list()
cnt<-1
for (ii in 1:length(Conditions)) {
  EventIndex<-which(as.numeric(EventsData[[ii]][2,])<500)
  FailureIndex<-which(as.numeric(EventsData[[ii]][2,])==500)
  Events<-EventsData[[ii]][,EventIndex]
  Failures<-EventsData[[ii]][,FailureIndex]
  
  Failures[1:2,]<-Events[1:2,] # now they have the same exotime and ID
  for (i in 1:dim(Failures)[2]) {
    Failures[10011:10111,i]<-Failures[(2011+as.numeric(Failures[2,1])-50):(2011+as.numeric(Failures[2,1])+50),i] #DF left
    Failures[10211:10311,i]<-as.character(as.numeric(unlist(Failures[(1011+as.numeric(Failures[2,1])-50):(1011+as.numeric(Failures[2,1])+50),i]))-
                                            as.numeric(unlist(Failures[(3011+as.numeric(Failures[2,1])-50):(3011+as.numeric(Failures[2,1])+50),i]))) #S left
    
    Failures[11011:11111,i]<-Failures[(7011+as.numeric(Failures[2,1])-50):(7011+as.numeric(Failures[2,1])+50),i] #DF right
    Failures[11211:11311,i]<-as.character(as.numeric(unlist(Failures[(6011+as.numeric(Failures[2,1])-50):(6011+as.numeric(Failures[2,1])+50),i]))-
                                            as.numeric(unlist(Failures[(8011+as.numeric(Failures[2,1])-50):(8011+as.numeric(Failures[2,1])+50),i])))#S rigt
    
  }
  EventsFailureConditions[[cnt]]<-Events
  cnt<-cnt+1
  EventsFailureConditions[[cnt]]<-Failures
  cnt<-cnt+1
}

names(EventsFailureConditions)<-unique(interaction(CellsInfo$Condition,States))

#convert the tables to copy number (Run the conversion to molecules copy number factor)

CellsInfo$ROI<-substring(CellsInfo$ROI,1,4)
ConversionData$Dates<-as.factor(ConversionData$Dates)
CellsInfo$ConversionFactDF<-rep(NaN,dim(CellsInfo)[1],1)
CellsInfo$ConversionFactS<-rep(NaN,dim(CellsInfo)[1],1)
for (i in 1:nlevels(CellsInfo$Date)) {
  indx<-which(CellsInfo$Date==ConversionData$Dates[i])
  CellsInfo$ConversionFactDF[indx]<-ConversionData$EGFPDF2[i]
  CellsInfo$ConversionFactS<-(CellsInfo$ConversionFactDF[i]*((pi*(0.55/2)^2))/(pi*(1/2)^2))#Molecules/µm2= S/(Constant*( Area.ΔF1/1µm2))
  # the area is circular therefore you have to make it circular
  
}

## having the constats so


for (i in 1:dim(CellsInfo)[1]) {
  indx<-which(CellsInfo$Condition[i]==str_match(names(EventsFailureConditions), "^[^.]+"))
  for (ii in 1:length(indx)) {
    indx2<-which(str_match(EventsFailureConditions[[indx[ii]]][1,], "_(.*?)_")[,2]==CellsInfo$ROI[i])
    if(!length(indx2)==0){
      for (iii in 1:length(indx2)) {
        if (CellsInfo$ABOvCorrection[i]!=0){
          EventsFailureConditions[[indx[ii]]][10011:10111,indx2[iii]]<-as.character((as.numeric(unlist(EventsFailureConditions[[indx[ii]]][10011:10111,indx2[iii]]))/CellsInfo$ConversionFactDF[i])+(
            (as.numeric(unlist(EventsFailureConditions[[indx[ii]]][10011:10111,indx2[iii]]))/CellsInfo$ConversionFactDF[i])/CellsInfo$ABOvCorrection[i]))#add the dark molecules correction to DF
          EventsFailureConditions[[indx[ii]]][10211:13111,indx2[iii]]<-as.character((as.numeric(unlist(EventsFailureConditions[[indx[ii]]][10211:13111,indx2[iii]]))/CellsInfo$ConversionFactS[i])+(
            (as.numeric(unlist(EventsFailureConditions[[indx[ii]]][10211:10311,indx2[iii]]))/CellsInfo$ConversionFactDF[i])/CellsInfo$ABOvCorrection[i]))#add the dark molecules correction to S 
          
          EventsFailureConditions[[indx[ii]]][12011:12021,indx2[iii]]<-as.character((as.numeric(unlist(EventsFailureConditions[[indx[ii]]][2101:2111,indx2[iii]]))/CellsInfo$ConversionFactDF[i])+(
            (as.numeric(unlist(EventsFailureConditions[[indx[ii]]][2101:2111,indx2[iii]]))/CellsInfo$ConversionFactDF[i])/CellsInfo$ABOvCorrection[i]))#add the dark molecules correction to DF
          #the last one is to add the Copy number before the Membrane depolarization
          
        }else{
          EventsFailureConditions[[indx[ii]]][10011:10111,indx2[iii]]<-as.character((as.numeric(unlist(EventsFailureConditions[[indx[ii]]][10011:10111,indx2[iii]]))/CellsInfo$ConversionFactDF[i]))
          EventsFailureConditions[[indx[ii]]][10211:13111,indx2[iii]]<-as.character((as.numeric(unlist(EventsFailureConditions[[indx[ii]]][10211:13111,indx2[iii]]))/CellsInfo$ConversionFactS[i]))
          EventsFailureConditions[[indx[ii]]][12011:12021,indx2[iii]]<-as.character(as.numeric(unlist(EventsFailureConditions[[indx[ii]]][2101:2111,indx2[iii]]))/CellsInfo$ConversionFactDF[i])
          #the last one is to add the Copy number before the Membrane depolarization
        }
        
      }
    }
  }
}

struktur<-rbind(struktur,data.frame(variable="CNPbK",indexes=12011:12021))

for (i in 1:length(EventsFailureConditions)) {
  ROI<-str_match(EventsFailureConditions[[i]][1,], "_(.*?)_")[,2]
  Dates<-rep(NaN,length(ROI),1)
  Cndtn<-rep(names(EventsFailureConditions)[i],length(ROI),1)
  for (ii in 1:length(ROI)) {
    Dates[ii]<-as.character(CellsInfo$Date[CellsInfo$ROI==ROI[ii]])
  }
  ROI<-unlist(EventsFailureConditions[[i]][1,])
  tid<-unlist(EventsFailureConditions[[i]][2,])
  CopyNumbers<-EventsFailureConditions[[i]][10052:10061,]
  CPNbK<-EventsFailureConditions[[i]][12011:12020,]
  storlek<-dim(CopyNumbers)
  CopyNumbers<-apply(matrix(as.numeric(unlist(CopyNumbers)),nrow = storlek[1],ncol = storlek[2]),2,mean,na.rm=TRUE)
  CPNbK<-apply(matrix(as.numeric(unlist(CPNbK)),nrow = storlek[1],ncol = storlek[2]),2,mean,na.rm=TRUE)
  SDens<-EventsFailureConditions[[i]][10252:10261,]
  SDens<-apply(matrix(as.numeric(unlist(SDens)),nrow = storlek[1],ncol = storlek[2]),2,mean,na.rm=TRUE)
  
  if (i==1){
    measuredCopynumbers<-data.frame(Cndtn,ROI,Dates,CopyNumbers,SDens,tid,CPNbK)
  }else{
    measuredCopynumbers<-rbind(measuredCopynumbers,data.frame(Cndtn,ROI,Dates,CopyNumbers,SDens,tid,CPNbK))
  }
}

measuredCopynumbers$Cndtn<-as.factor(measuredCopynumbers$Cndtn)

#DF copy number
sumCN<-measuredCopynumbers %>%
  group_by(Cndtn) %>%
  summarise(
    Mean = mean(CopyNumbers, na.rm = TRUE),
    SD = sd(CopyNumbers, na.rm = TRUE),
    Median= median(CopyNumbers, na.rm = TRUE),
    count=length(CopyNumbers),
    SEM=sd(CopyNumbers, na.rm = TRUE)/sqrt(length(CopyNumbers))
  )
# Sdensity
measuredCopynumbers %>%
  group_by(Cndtn) %>%
  summarise(
    Mean = mean(SDens, na.rm = TRUE),
    SD = sd(SDens, na.rm = TRUE),
    Median= median(SDens, na.rm = TRUE)
  )

ggplot(measuredCopynumbers,aes(x=Cndtn,y=CopyNumbers,group=Cndtn))+geom_boxplot() # the data looks good for now

p<-ggplot(data=sumCN, aes(x=Cndtn, y=Mean)) +
  geom_bar(stat="identity",position = position_dodge())+
  geom_errorbar(aes(ymin=Mean-SEM, ymax=Mean+SEM), width=.2,size=0.5,
                position=position_dodge(.9))+theme_classic() 
  
p


# Statistics about copy n

measuredCopynumbers$Protein<-str_split_fixed(measuredCopynumbers$Cndtn, "\\.", 2)[,1]
measuredCopynumbers$status<-str_split_fixed(measuredCopynumbers$Cndtn, "\\.", 2)[,2]
measuredCopynumbers$Protein<-as.factor(measuredCopynumbers$Protein)
measuredCopynumbers$status<-as.factor(measuredCopynumbers$status)
measuredCopynumbers$DFovS<-measuredCopynumbers$CopyNumbers/measuredCopynumbers$SDens
shapiro.test(measuredCopynumbers$CopyNumbers)
leveneTest(CopyNumbers~Protein*status,data=measuredCopynumbers)

anova.2way.unbalanced(measuredCopynumbers$CopyNumbers,measuredCopynumbers$Protein,measuredCopynumbers$status)
pairwisePermutationTest(CopyNumbers~Protein,data=measuredCopynumbers)

#Munc13-1 evfail
m131evfa<-measuredCopynumbers[measuredCopynumbers$Protein=="INS1_M13-1",]
perm.t.test(CopyNumbers~status,data=,m131evfa,paired=TRUE)
#Munc13-2 evfail
m132evfa<-measuredCopynumbers[measuredCopynumbers$Protein=="INS1_M13-2",]
perm.t.test(CopyNumbers~status,data=,m132evfa,paired=TRUE)


## To get the line plots

for (i in 1:length(EventsFailureConditions)) {
  CopyNumbers<-EventsFailureConditions[[i]][10011:10111,]
  storlek<-dim(CopyNumbers)
  CopyNumbersSEM<-apply(matrix(as.numeric(unlist(CopyNumbers)),nrow = storlek[1],ncol = storlek[2]),1,sd,na.rm=TRUE)/sqrt(storlek[2])
  CopyNumbers<-apply(matrix(as.numeric(unlist(CopyNumbers)),nrow = storlek[1],ncol = storlek[2]),1,mean,na.rm=TRUE)
  
  
  SDens<-EventsFailureConditions[[i]][10211:10311,]
  SDensSEM<-apply(matrix(as.numeric(unlist(SDens)),nrow = storlek[1],ncol = storlek[2]),1,sd,na.rm=TRUE)/sqrt(storlek[2])
  SDens<-apply(matrix(as.numeric(unlist(SDens)),nrow = storlek[1],ncol = storlek[2]),1,mean,na.rm=TRUE)
  
  GrDF<-EventsFailureConditions[[i]][11011:11111,]
  GrDFSEM<-apply(matrix(as.numeric(unlist(GrDF)),nrow = storlek[1],ncol = storlek[2]),1,sd,na.rm=TRUE)/sqrt(storlek[2])
  GrDF<-apply(matrix(as.numeric(unlist(GrDF)),nrow = storlek[1],ncol = storlek[2]),1,mean,na.rm=TRUE)
  
  if(i==1){
    CN<-CopyNumbers
    CNSEM<-CopyNumbersSEM
    CNS<-SDens
    CNSSEM<-SDensSEM
    grDF<-GrDF
    grDFSEM<-GrDFSEM
  }else{
    CN<-cbind(CN,CopyNumbers)
    CNS<-cbind(CNS,SDens)
    grDF<-cbind(grDF,GrDF)
    
    CNSEM<-cbind(CNSEM,CopyNumbersSEM)
    CNSSEM<-cbind(CNSSEM,SDensSEM)
    grDFSEM<-cbind(grDFSEM,GrDFSEM)
  }
}
CN<-as.data.frame(CN)
CNS<-as.data.frame(CNS)
grDF<-as.data.frame(grDF)
names(CN)<-names(EventsFailureConditions)
names(CNS)<-names(EventsFailureConditions)
names(grDF)<-names(EventsFailureConditions)

CNSEM<-as.data.frame(CNSEM)
CNSSEM<-as.data.frame(CNSSEM)
grDFSEM<-as.data.frame(grDFSEM)
names(CNSEM)<-names(EventsFailureConditions)
names(CNSSEM)<-names(EventsFailureConditions)
names(grDFSEM)<-names(EventsFailureConditions)

#Plots 
x<-seq(-5,5,0.1)
m131plot<-ggplot()+geom_ribbon(data=CN,aes(x=x,ymin=`INS1_M13-1.E`-CNSEM$`INS1_M13-1.E`,ymax=`INS1_M13-1.E`+CNSEM$`INS1_M13-1.E`),
                               color="#ff9999",fill="#ff9999")+
  geom_line(data=CN,aes(x=x,y=CN$`INS1_M13-1.E`),color="red",size=1)+
  geom_ribbon(data=CN,aes(x=x,ymin=`INS1_M13-1.F`-CNSEM$`INS1_M13-1.F`,ymax=`INS1_M13-1.F`+CNSEM$`INS1_M13-1.F`),
              color="#cccccc",fill="#cccccc")+
  geom_line(data=CN,aes(x=x,y=`INS1_M13-1.F`),color="#999999",size=1)+
  ylab("Copy number")+xlab("Time(s)")+
  ggtitle("Events vs Failures \n M13-1")+theme_classic()
  
m131plot

m132plot<-ggplot()+geom_ribbon(data=CN,aes(x=x,ymin=`INS1_M13-2.E`-CNSEM$`INS1_M13-2.E`,ymax=`INS1_M13-2.E`+CNSEM$`INS1_M13-2.E`),
                               color="#99cc33",fill="#99cc33")+
  geom_line(data=CN,aes(x=x,y=`INS1_M13-2.E`),color="green",size=1)+
  geom_ribbon(data=CN,aes(x=x,ymin=`INS1_M13-2.F`-CNSEM$`INS1_M13-2.F`,ymax=`INS1_M13-2.F`+CNSEM$`INS1_M13-2.F`),
              color="#cccccc",fill="#cccccc")+
  geom_line(data=CN,aes(x=x,y=`INS1_M13-2.F`),color="#999999",size=1)+
  ylab("Copy number")+xlab("Time(s)")+
  ggtitle("Events vs Failures \n M13-2")+theme_classic()

m132plot

## granules

Grm131plot<-ggplot()+geom_ribbon(data=grDF,aes(x=x,ymin=`INS1_M13-1.E`-grDFSEM$`INS1_M13-1.E`,ymax=`INS1_M13-1.E`+grDFSEM$`INS1_M13-1.E`),
                                 color="#ff9999",fill="#ff9999")+
  geom_line(data=grDF,aes(x=x,y=`INS1_M13-1.E`),color="red",size=1)+
  geom_ribbon(data=grDF,aes(x=x,ymin=`INS1_M13-1.F`-grDFSEM$`INS1_M13-1.F`,ymax=`INS1_M13-1.F`+grDFSEM$`INS1_M13-1.F`),
              color="#cccccc",fill="#cccccc")+
  geom_line(data=grDF,aes(x=x,y=`INS1_M13-1.F`),color="#999999",size=1)+
  ylab("Granules DF")+xlab("Time(s)")+
  ggtitle("Events vs Failures \n M13-1")+theme_classic()

Grm131plot




Grm132plot<-ggplot()+geom_ribbon(data=grDF,aes(x=x,ymin=`INS1_M13-2.E`-grDFSEM$`INS1_M13-2.E`,ymax=`INS1_M13-2.E`+grDFSEM$`INS1_M13-2.E`),
                               color="#99cc33",fill="#99cc33")+
  geom_line(data=grDF,aes(x=x,y=`INS1_M13-2.E`),color="green",size=1)+
  geom_ribbon(data=grDF,aes(x=x,ymin=`INS1_M13-2.F`-grDFSEM$`INS1_M13-2.F`,ymax=`INS1_M13-2.F`+grDFSEM$`INS1_M13-2.F`),
              color="#cccccc",fill="#cccccc")+
  geom_line(data=grDF,aes(x=x,y=`INS1_M13-2.F`),color="#999999",size=1)+
  ylab("Granules DF")+xlab("Time(s)")+
  ggtitle("Events vs Failures \n M13-2")+theme_classic()

Grm132plot


## The code is until here but I wonder the copy number vs time relation

measuredCopynumbers$tid<-as.numeric(measuredCopynumbers$tid)

pr<-ggplot(data=measuredCopynumbers,aes(x=tid,y=CPNbK, color=Cndtn))+geom_jitter() #not so clean
nbin=9
binnigCurvesCN<-matrix(NaN,nrow=nbin,ncol=nlevels(measuredCopynumbers$Cndtn))
binnigCurvestid<-matrix(NaN,nrow=nbin,ncol=nlevels(measuredCopynumbers$Cndtn))

binnigCurvesCNSEM<-matrix(NaN,nrow=nbin,ncol=nlevels(measuredCopynumbers$Cndtn))
binnigCurvestidSEM<-matrix(NaN,nrow=nbin,ncol=nlevels(measuredCopynumbers$Cndtn))
for (i in 1:nlevels(measuredCopynumbers$Cndtn)) {
  binnig<-measuredCopynumbers[measuredCopynumbers$Cndtn==levels(measuredCopynumbers$Cndtn)[i],]
  binnig<-binnig[order(binnig$tid),]
  binnig <- binnig %>% mutate(QuantileBins = ntile(tid, nbin))
  for (ii in 1:nbin) {
    binnigCurvesCN[ii,i]<-mean(binnig$CPNbK[binnig$QuantileBins==ii])
    binnigCurvestid[ii,i]<-mean(binnig$tid[binnig$QuantileBins==ii])
    
    binnigCurvesCNSEM[ii,i]<-sd(binnig$CPNbK[binnig$QuantileBins==ii])/sqrt(length(binnig$CPNbK[binnig$QuantileBins==ii]))
    binnigCurvestidSEM[ii,i]<-sd(binnig$tid[binnig$QuantileBins==ii])/sqrt(length(binnig$CPNbK[binnig$QuantileBins==ii]))
  }
}
binnigCurvesCN<-as.data.frame(binnigCurvesCN)
binnigCurvesCNSEM<-as.data.frame(binnigCurvesCNSEM)
binnigCurvestid<-as.data.frame(binnigCurvestid)
binnigCurvestidSEM<-as.data.frame(binnigCurvestidSEM)

names(binnigCurvesCN)<-levels(measuredCopynumbers$Cndtn)
names(binnigCurvesCNSEM)<-levels(measuredCopynumbers$Cndtn)
names(binnigCurvestid)<-levels(measuredCopynumbers$Cndtn)
names(binnigCurvestidSEM)<-levels(measuredCopynumbers$Cndtn)

m131pr<-ggplot()+geom_point(aes(y=binnigCurvesCN$`INS1_M13-1.E`,x=binnigCurvestid$`INS1_M13-1.E`*0.1),color="#cc0000",size=4)+
  geom_point(aes(y=binnigCurvesCN$`INS1_M13-1.F`,x=binnigCurvestid$`INS1_M13-1.F`*0.1),color="#999999",size=4)+xlab("Time(s)")+ylab("CPN")+
  geom_errorbar(aes(y=binnigCurvesCN$`INS1_M13-1.E`,x=binnigCurvestid$`INS1_M13-1.E`*0.1,
                    ymin=binnigCurvesCN$`INS1_M13-1.E`-binnigCurvesCNSEM$`INS1_M13-1.E`,
                    ymax=binnigCurvesCN$`INS1_M13-1.E`+binnigCurvesCNSEM$`INS1_M13-1.E`))+
  geom_errorbar(aes(y=binnigCurvesCN$`INS1_M13-1.F`,x=binnigCurvestid$`INS1_M13-1.F`*0.1,
                    ymin=binnigCurvesCN$`INS1_M13-1.F`-binnigCurvesCNSEM$`INS1_M13-1.F`,
                    ymax=binnigCurvesCN$`INS1_M13-1.F`+binnigCurvesCNSEM$`INS1_M13-1.F`))+theme_classic()
  
m131pr

m132pr<-ggplot()+geom_point(aes(y=binnigCurvesCN$`INS1_M13-2.E`,x=binnigCurvestid$`INS1_M13-2.E`*0.1),color="#006600",size=4)+
  geom_point(aes(y=binnigCurvesCN$`INS1_M13-2.F`,x=binnigCurvestid$`INS1_M13-2.F`*0.1),color="#999999",size=4)+xlab("Time(s)")+ylab("CPN")+
  geom_errorbar(aes(y=binnigCurvesCN$`INS1_M13-2.E`,x=binnigCurvestid$`INS1_M13-2.E`*0.1,
                    ymin=binnigCurvesCN$`INS1_M13-2.E`-binnigCurvesCNSEM$`INS1_M13-2.E`,
                    ymax=binnigCurvesCN$`INS1_M13-2.E`+binnigCurvesCNSEM$`INS1_M13-2.E`))+
  geom_errorbar(aes(y=binnigCurvesCN$`INS1_M13-2.F`,x=binnigCurvestid$`INS1_M13-2.F`*0.1,
                    ymin=binnigCurvesCN$`INS1_M13-2.F`-binnigCurvesCNSEM$`INS1_M13-2.F`,
                    ymax=binnigCurvesCN$`INS1_M13-2.F`+binnigCurvesCNSEM$`INS1_M13-2.F`))+theme_classic()
m132pr

## to bin for affinity

pr<-ggplot(data=measuredCopynumbers,aes(x=SDens,y=CopyNumbers, color=Cndtn))+geom_jitter() #not so clean
nbin=9
binnigCurvesCNS<-matrix(NaN,nrow=nbin,ncol=nlevels(measuredCopynumbers$Cndtn))
binnigCurvesSdens<-matrix(NaN,nrow=nbin,ncol=nlevels(measuredCopynumbers$Cndtn))
binnigCurvesDFS<-matrix(NaN,nrow=nbin,ncol=nlevels(measuredCopynumbers$Cndtn))

binnigCurvesCNSSEM<-matrix(NaN,nrow=nbin,ncol=nlevels(measuredCopynumbers$Cndtn))
binnigCurvesSdensSEM<-matrix(NaN,nrow=nbin,ncol=nlevels(measuredCopynumbers$Cndtn))
binnigCurvesDFSSEM<-matrix(NaN,nrow=nbin,ncol=nlevels(measuredCopynumbers$Cndtn))
for (i in 1:nlevels(measuredCopynumbers$Cndtn)) {
  binnig<-measuredCopynumbers[measuredCopynumbers$Cndtn==levels(measuredCopynumbers$Cndtn)[i],]
  binnig<-binnig[order(binnig$SDens),]
  binnig <- binnig %>% mutate(QuantileBins = ntile(SDens, nbin))
  for (ii in 1:nbin) {
    binnigCurvesCNS[ii,i]<-mean(binnig$CopyNumbers[binnig$QuantileBins==ii])
    binnigCurvesSdens[ii,i]<-mean(binnig$SDens[binnig$QuantileBins==ii])
    binnigCurvesDFS[ii,i]<-mean(binnig$DFovS[binnig$QuantileBins==ii])
    
    binnigCurvesCNSSEM[ii,i]<-sd(binnig$CopyNumbers[binnig$QuantileBins==ii])/sqrt(length(binnig$CopyNumbers[binnig$QuantileBins==ii]))
    binnigCurvesSdensSEM[ii,i]<-sd(binnig$SDens[binnig$QuantileBins==ii])/sqrt(length(binnig$CopyNumbers[binnig$QuantileBins==ii]))
    binnigCurvesDFSSEM[ii,i]<-sd(binnig$DFovS[binnig$QuantileBins==ii])/sqrt(length(binnig$CopyNumbers[binnig$QuantileBins==ii]))
  }
}
binnigCurvesCNS<-as.data.frame(binnigCurvesCNS)
binnigCurvesCNSSEM<-as.data.frame(binnigCurvesCNSSEM)
binnigCurvesSdens<-as.data.frame(binnigCurvesSdens)
binnigCurvesSdensSEM<-as.data.frame(binnigCurvesSdensSEM)
binnigCurvesDFS<-as.data.frame(binnigCurvesDFS)
binnigCurvesDFSSEM<-as.data.frame(binnigCurvesDFSSEM)


names(binnigCurvesCNS)<-levels(measuredCopynumbers$Cndtn)
names(binnigCurvesCNSSEM)<-levels(measuredCopynumbers$Cndtn)
names(binnigCurvesSdens)<-levels(measuredCopynumbers$Cndtn)
names(binnigCurvesSdensSEM)<-levels(measuredCopynumbers$Cndtn)
names(binnigCurvesDFS)<-levels(measuredCopynumbers$Cndtn)
names(binnigCurvesDFSSEM)<-levels(measuredCopynumbers$Cndtn)

m131aff<-ggplot()+geom_point(aes(y=binnigCurvesCNS$`INS1_M13-1.E`,x=binnigCurvesSdens$`INS1_M13-1.E`,color=binnigCurvesDFS$`INS1_M13-1.E`),size=6)+
  scale_color_gradientn(colours=rainbow(7), limits=c(0.00,0.12), guide = "none" )+xlab("MCP/um^2")+ylab("CPN")+
  geom_errorbar(aes(y=binnigCurvesCNS$`INS1_M13-1.E`,x=binnigCurvesSdens$`INS1_M13-1.E`,
                    ymin=binnigCurvesCNS$`INS1_M13-1.E`-binnigCurvesCNSSEM$`INS1_M13-1.E`,
                    ymax=binnigCurvesCNS$`INS1_M13-1.E`+binnigCurvesCNSSEM$`INS1_M13-1.E`,
                    xmin=binnigCurvesSdens$`INS1_M13-1.E`-binnigCurvesSdensSEM$`INS1_M13-1.E`,
                    xmax=binnigCurvesSdens$`INS1_M13-1.E`+binnigCurvesSdensSEM$`INS1_M13-1.E`))+theme_classic()
m131aff

m132aff<-ggplot()+geom_point(aes(y=binnigCurvesCNS$`INS1_M13-2.E`,x=binnigCurvesSdens$`INS1_M13-2.E`,color=binnigCurvesDFS$`INS1_M13-2.E`),size=6)+
  scale_color_gradientn(colours=rainbow(7), limits=c(0.00,0.15), guide = "none" )+xlab("MCP/um^2")+ylab("CPN")+
  geom_errorbar(aes(y=binnigCurvesCNS$`INS1_M13-2.E`,x=binnigCurvesSdens$`INS1_M13-2.E`,
                    ymin=binnigCurvesCNS$`INS1_M13-2.E`-binnigCurvesCNSSEM$`INS1_M13-2.E`,
                    ymax=binnigCurvesCNS$`INS1_M13-2.E`+binnigCurvesCNSSEM$`INS1_M13-2.E`,
                    xmin=binnigCurvesSdens$`INS1_M13-2.E`-binnigCurvesSdensSEM$`INS1_M13-2.E`,
                    xmax=binnigCurvesSdens$`INS1_M13-2.E`+binnigCurvesSdensSEM$`INS1_M13-2.E`))+theme_classic()
m132aff

colfunc<-colorRampPalette(c(rainbow(7)))
colfunc(10)

plot(seq(0.000,0.15,0.0025),rep(1,61),col=colfunc(61),pch=15,cex=16)
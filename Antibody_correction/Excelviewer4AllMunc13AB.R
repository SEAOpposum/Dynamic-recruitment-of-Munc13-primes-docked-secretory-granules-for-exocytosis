#This is just to see how good the data is working
library(quantreg)
library(rcompanion)
library(conquer)
#installing package, if not yet available
library(devtools)
library(drcSeedGerm)
library(sandwich)
library(lmtest)
library(chngpt)
library(R.rsp)
library(RUnit)
library(mvtnorm)
library(segmented)
library(MASS)
library(nlme)
library(Rpdb)
library(writexl)
library(roll)
library(ggplot2)
library(MKinfer)
library(readxl)
library(car)
library(BSDA)
library(ggplot2)


m132r<-read_excel("E:/SubmissionM13 Paper/Revision Cell reports/M131vsM132/Exocytosis exp/Expression 250116/Align/Munc13abIn.xlsx", sheet = "1600_M13ab_Red")
m132g<-read_excel("E:/SubmissionM13 Paper/Revision Cell reports/M131vsM132/Exocytosis exp/Expression 250116/Align/Munc13abIn.xlsx", sheet = "1600_M13ab_Green")
m131r<-read_excel("E:/SubmissionM13 Paper/Revision Cell reports/M131vsM132/Exocytosis exp/Expression 250116/Align/Munc13abIn.xlsx", sheet = "1031_M13ab_Red")
m131g<-read_excel("E:/SubmissionM13 Paper/Revision Cell reports/M131vsM132/Exocytosis exp/Expression 250116/Align/Munc13abIn.xlsx", sheet = "1031_M13ab_Green")
m13n<-read_excel("E:/SubmissionM13 Paper/Revision Cell reports/M131vsM132/Exocytosis exp/Expression 250116/Align/Munc13abIn.xlsx", sheet = "M13ab_Red")

greenlaser=108*0.010
redlaser=99.7*0.010


isAsymptotic<-FALSE#TRUE#
linearCut<- 700

# in this case all the data was taken with 100ms exp constant laser power
# So normalizations aren't need here
# the bg and cell ROi where the same so just use the mean values for secondary antibody


m131g$meanRed<-as.numeric(m131g$meanRed)
m131g$meanGreen<-as.numeric(m131g$meanGreen)
m131g$meanbgGreen<-as.numeric(m131g$meanbgGreen)
m131g$meanbgRed<-as.numeric(m131g$meanbgRed)
m131g$stt<-rep("m13-1",dim(m131g)[1],1)


m132g$meanRed<-as.numeric(m132g$meanRed)
m132g$meanGreen<-as.numeric(m132g$meanGreen)
m132g$meanbgGreen<-as.numeric(m132g$meanbgGreen)
m132g$meanbgRed<-as.numeric(m132g$meanbgRed)
m132g$stt<-rep("m13-2",dim(m132g)[1],1)

m131r$meanRed<-as.numeric(m131r$meanRed)
m131r$meanGreen<-as.numeric(m131r$meanGreen)
m131r$meanbgGreen<-as.numeric(m131r$meanbgGreen)
m131r$meanbgRed<-as.numeric(m131r$meanbgRed)
m131r$stt<-rep("m13-1",dim(m131r)[1],1)

m132r$meanRed<-as.numeric(m132r$meanRed)
m132r$meanGreen<-as.numeric(m132r$meanGreen)
m132r$meanbgGreen<-as.numeric(m132r$meanbgGreen)
m132r$meanbgRed<-as.numeric(m132r$meanbgRed)
m132r$stt<-rep("m13-2",dim(m132r)[1],1)



m13n$meanRed<-as.numeric(m13n$meanRed)
m13n$meanGreen<-as.numeric(m13n$meanGreen)
m13n$meanbgGreen<-as.numeric(m13n$meanbgGreen)
m13n$meanbgRed<-as.numeric(m13n$meanbgRed)
m13n$stt<-rep("Native",dim(m13n)[1],1)

#Native

m13n$RedCorrection<-(m13n$meanRed - m13n$meanbgRed)/redlaser## The substraction is secondary mean + one SD
m13n$GreenCorrection<- (m13n$meanGreen - m13n$meanbgGreen)/greenlaser## The substraction is secondary mean + one SD

m13n<-m13n[which(m13n$RedCorrection>= 0),]
meanN<-c(mean(m13n$RedCorrection),mean(m13n$GreenCorrection))
SdN<-c(sd(m13n$RedCorrection),sd(m13n$GreenCorrection))
NativeResults<-data.frame(meanN,SdN, row.names = c("Red","Green"))

m13n$GreenOVRed<-m13n$GreenCorrection/m13n$RedCorrection

plot(m13n$RedCorrection,m13n$GreenCorrection)


# M13-1

m131r$RedCorrection<- (m131r$meanRed - m131r$meanbgRed)/redlaser## The substraction is secondary mean + one SD #unit is AU/mW
m131r$GreenCorrection<- (m131r$meanGreen - m131r$meanbgGreen)/greenlaser## The substraction is secondary mean + one SD

if(isAsymptotic==FALSE){
  m131r<-m131r[which(m131r$RedCorrection>= 0),] #this is the normal
}else if(isAsymptotic==TRUE){
  m131r<-m131r[which(m131r$RedCorrection>= 0 & m131r$GreenCorrection<= linearCut),] # this is when the data is asymptotic and 
}                                                   #the upper limit is for the point where de data lost linearity 

plot(m131r$RedCorrection,m131r$GreenCorrection)

m131g$RedCorrection<- (m131g$meanRed - m131g$meanbgRed)/redlaser## The substraction is secondary mean + one SD
m131g$GreenCorrection<- (m131g$meanGreen - m131g$meanbgGreen)/greenlaser## The substraction is secondary mean + one SD

if(isAsymptotic==FALSE){
  m131g<-m131g[which(m131g$RedCorrection>= 0),] #this is the normal
}else if(isAsymptotic==TRUE){
  m131g<-m131g[which(m131g$RedCorrection>= 0 & m131g$GreenCorrection<= linearCut),] # this is when the data is asymptotic and 
}                                                   #the upper limit is for the point where de data lost linearity 

plot(m131g$GreenCorrection,m131g$RedCorrection)#the upper limit is for the point where de data lost linearity 




##M13-2
m132r$RedCorrection<- (m132r$meanRed - m132r$meanbgRed )/redlaser## The substraction is secondary mean + one SD
m132r$GreenCorrection<- (m132r$meanGreen - m132r$meanbgGreen)/greenlaser## The substraction is secondary mean + one SD

if(isAsymptotic==FALSE){
  m132r<-m132r[which(m132r$RedCorrection>= 0),] #this is the normal
}else if(isAsymptotic==TRUE){
  m132r<-m132r[which(m132r$RedCorrection>= 0 & m132r$GreenCorrection<= linearCut),] # this is when the data is asymptotic and 
}                                                   #the upper limit is for the point where de data lost linearity 

plot(m132r$GreenCorrection,m132r$RedCorrection)

m132g$RedCorrection<- (m132g$meanRed - m132g$meanbgRed )/redlaser## The substraction is secondary mean + one SD
m132g$GreenCorrection<- (m132g$meanGreen - m132g$meanbgGreen)/greenlaser## The substraction is secondary mean + one SD

if(isAsymptotic==FALSE){
  m132g<-m132g[which(m132g$RedCorrection>= 0),] #this is the normal
}else if(isAsymptotic==TRUE){
  m132g<-m132g[which(m132g$RedCorrection>= 0 & m132g$GreenCorrection<= linearCut),] # this is when the data is asymptotic and 
}                                                   #the upper limit is for the point where de data lost linearity 

plot(m132g$GreenCorrection,m132g$RedCorrection)#the upper limit is for the point where de data lost linearity 

### to plot the mix of cells
m131<-rbind(m131g,m131r)
m131$GreenOVRed<-m131$GreenCorrection/m131$RedCorrection
indx<-which(m131$GreenOVRed<1)
m131<-m131[indx,]
plot(m131$GreenCorrection,m131$RedCorrection)


modelm131<-lm(m131$RedCorrection~m131$GreenCorrection)
m131mod<-summary(modelm131)

m132<-rbind(m132g,m132r)
m132$GreenOVRed<-m132$GreenCorrection/m132$RedCorrection
indx<-which(m132$GreenOVRed<1)
m132<-m132[indx,]
plot(m132$GreenCorrection,m132$RedCorrection)

modelm132<-lm(m132$RedCorrection~m132$GreenCorrection)
m132mod<-summary(modelm132)

m131mod$coefficients[,2]<-m131mod$coefficients[,2]*sqrt(dim(m131)[1])
m132mod$coefficients[,2]<-m132mod$coefficients[,2]*sqrt(dim(m132)[1])

tsum.test(mean.x = m131mod$coefficients[2,1],s.x =m131mod$coefficients[2,2],n.x = dim(m131)[1],
          mean.y = m132mod$coefficients[2,1],s.y =m132mod$coefficients[2,2],n.y = dim(m132)[1],var.equal = FALSE)

# So the slope is bigger in M132 than M13-1

m13all<-rbind(m131,m132,m13n)
m13all$stt<-as.factor(m13all$stt)

linesm131<-seq(min(m131$GreenCorrection),max(m131$GreenCorrection),1)*m131mod$coefficients[2,1]+m131mod$coefficients[1,1]
linesm132<-seq(min(m132$GreenCorrection),max(m132$GreenCorrection),1)*m132mod$coefficients[2,1]+m132mod$coefficients[1,1]

plot<-ggplot()+geom_jitter(data=m13all,aes(x=GreenCorrection,y=RedCorrection,color=stt),size=3)+
  geom_line(aes(x=seq(min(m131$GreenCorrection),max(m131$GreenCorrection),1),y=linesm131),size=1,color="#FF9999")+
  geom_line(aes(x=seq(min(m132$GreenCorrection),max(m132$GreenCorrection),1),y=linesm132),size=1,color="#66cc66")+
  ylab("M13ab-signal (AU/mW)")+xlab("EGFP signal (AU/mW)")+theme_classic()
plot



m131mod
m132mod
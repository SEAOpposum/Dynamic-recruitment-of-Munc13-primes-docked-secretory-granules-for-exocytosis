## Analysis Calcium for human islets
#the stats part is on-going since the data acquisition is still happening
library(readxl)
library(stringr)
library(car)
library(RVAideMemoire)
library(ggplot2)

#to convert units to scientific notatio
Characters<-c("p","n","f","M","G","a")
CharactersValues<-c(10^-12,10^-9,10^-15,10^6,10^9,10^-18)
CharactersMeaning<-data.frame(Characters,CharactersValues)
rm(Characters,CharactersValues)#clean vectors keep df

# structure of the data set
GeneralInfo<-c(1,3)
First50ms<-c(4,14)
FirstTrainSum<-c(15,27)
FirstTrainCap<-c(28,46)
FirstTrainICa<-c(47,65)
IV<-c(66,95)
SecondTrainSum<-c(96,108)
SecondTrainCap<-c(109,127)
SecondTrainICa<-c(128,146)

strData<-data.frame(GeneralInfo,First50ms,FirstTrainSum,FirstTrainCap,FirstTrainICa,IV,
                    SecondTrainSum,SecondTrainCap,SecondTrainICa)
rm(GeneralInfo,First50ms,FirstTrainSum,FirstTrainCap,FirstTrainICa,IV,SecondTrainSum,
   SecondTrainCap,SecondTrainICa)


#Function space
substrRight <- function(x, n){
  if (missing(n)){
    n<-1
  }
  
  substr(x, nchar(x)-n+1, nchar(x))
}##Extract sub string from the right side

OrganizeAtable<-function(listLevel,ObjectName){
  for (i in 1:length(listLevel)){
    ci<-as.data.frame(listLevel[[i]])
    
    descriptors<-as.data.frame(t(unlist(strsplit(ObjectName[i], split=" "))))
    names(descriptors)<-c("date","donor","T2D/NA","CalciumSolution")
    descriptors2<-descriptors
    for (ii in 2:dim(ci)[1]) {
      descriptors2<-rbind(descriptors2,descriptors)
    }
    ci<-cbind(descriptors2,ci)
    if (i==1){
      df2<-ci
    }else{
      df2<-rbind(df2,ci)
    }
  }
  return(df2)
}# create a data frame to do statistics in r

twoFactorStats<-function(v1,f1){
  #to test parametric
  shprrst<-shapiro.test(v1)
  #see number of factors
  
  if (shprrst[2]>=0.05){
    print(paste("The data is parametric normality:",shprrst[2]))
    tt<-t.test(v1~f1)
    print(paste("The t-test p-value is:",tt$p.value))
  }else{
    print(paste("The data is NOT parametric normality:",shprrst[2]))
    tt<-perm.t.test(v1~f1,progress = FALSE)
    print(paste("The permutated t-test p-value is:",tt$p.value))
  }
  df<-data.frame(f1,v1)
  names(df)<-c("f1","v1")
  plt<-ggplot(df,aes(x=f1,y=v1,group=f1,colour=f1))+geom_boxplot(outlier.shape = NA)+geom_jitter(width = 0.1,height = 0)+
    theme_classic()
  return(list(tt,plt))
  
}# run stats for two factor hypothesis testing and make plot

processTrainCap<-function(df){
  dfnorm<-df
  dfnorm[,13:26]<-df[,13:26]/df$`Cslow[F]...29`
  dfcumSum<-df
  dfcumSum[,13:26]<-t(apply(dfnorm[,13:26],1,cumsum))
  dfnorp1<-df
  dfnorp1[,13:26]<-dfnorm[,13:26]/dfnorm[,13]
  
  dflist<-list(df,dfnorm,dfcumSum,dfnorp1)
  names(dflist)<-c("raw","norm","cumsum","normp1")
  return(dflist)
}# calculates the Cap train errands norm, cumsum, normp1 

# First indicate the tables and columns
dates<-c("20240429")
donor<-c("H2686")
Condition<-c("T2D","NA")
CalciumSolution<-c("0nmCa","400nmCa")
ObjectName<-rep(NA,(length(dates)*length(Condition)*length(CalciumSolution)))
count<-1
for (i in dates) {
  for (ii in donor) {
    for (iii in Condition) {
      for (iiii in CalciumSolution) {
        ObjectName[count]<-paste(i,ii,iii,iiii)
        count<-count+1 
      }
    }
  }
}#to get the possible objects name for import the data
rm(ii,iii,iiii, Condition,dates,count,CalciumSolution)
for (i in 1:length(ObjectName)) {
  if (i==1){
    Data<-list(try(read_excel("D:/HumanIslets/HumanisletCalcium.xlsx", sheet = ObjectName[i]))) 
  }else{
    Data2<-try(read_excel("D:/HumanIslets/HumanisletCalcium.xlsx", sheet = ObjectName[i]))
    if (class(Data2)[1]=="try-error"){
      ObjectName[i]<-NaN
    }else{
      Data<-c(Data,list(Data2)) 
    }
  }
}#Open data frames for days&condition&Calcium
ObjectName<-ObjectName[-which(ObjectName=="NaN")]
names(Data)<-ObjectName #Give names to the objects

# to translate the variables

for (ii in 1:length(Data)){
  Characters<-as.data.frame(lapply(Data[[ii]][,4:146],substrRight))##to know the respective symbol
  Data[[ii]][,4:146]<-lapply(Data[[ii]][,4:146],str_sub,end=-2)
  Data[[ii]][,4:146]<-matrix(as.double(unlist(Data[[ii]][,4:146])),dim(Data[[ii]][,4:146])[1],dim(Data[[ii]][,4:146])[2])
  
  for (i in 1:dim(CharactersMeaning)[1]){
    Characters[Characters==CharactersMeaning[i,1]]<-CharactersMeaning[i,2]
  }
  Characters<-matrix(as.numeric(unlist(Characters)),dim(Data[[ii]][,4:146])[1],dim(Data[[ii]][,4:146])[2])
  Data[[ii]][,4:146]<-Data[[ii]][,4:146]*Characters
  
}##Now data frames are in numbers

# to split the data by protocols
for (i in 1:length(Data)){
  
  First50ms<-cbind(Data[[i]][,strData[1,1]:strData[2,1]],Data[[i]][,strData[1,2]:strData[2,2]])
  FirstTrainSum<-cbind(Data[[i]][,strData[1,1]:strData[2,1]],Data[[i]][,strData[1,3]:strData[2,3]])
  FirstTrainCap<-cbind(Data[[i]][,strData[1,1]:strData[2,1]],Data[[i]][,strData[1,4]:strData[2,4]])
  FirstTrainICa<-cbind(Data[[i]][,strData[1,1]:strData[2,1]],Data[[i]][,strData[1,5]:strData[2,5]])
  IV<-cbind(Data[[i]][,strData[1,1]:strData[2,1]],Data[[i]][,strData[1,6]:strData[2,6]])
  SecondTrainSum<-cbind(Data[[i]][,strData[1,1]:strData[2,1]],Data[[i]][,strData[1,7]:strData[2,7]])
  SecondTrainCap<-cbind(Data[[i]][,strData[1,1]:strData[2,1]],Data[[i]][,strData[1,8]:strData[2,8]])
  SecondTrainICa<-cbind(Data[[i]][,strData[1,1]:strData[2,1]],Data[[i]][,strData[1,9]:strData[2,9]])
  Protocollist<-list(First50ms,FirstTrainSum,FirstTrainCap,FirstTrainICa,IV,SecondTrainSum,
                      SecondTrainCap,SecondTrainICa)
  names(Protocollist)<-c("First50ms","FirstTrainSum","FirstTrainCap","FirstTrainICa","IV","SecondTrainSum",
                          "SecondTrainCap","SecondTrainICa")
  if (i==1){
    DataOrganize<-list(Protocollist)
  }else{
    DataOrganize<-c(DataOrganize,list(Protocollist))
  }
}# this split the data in protocols and set the new list
Data<-DataOrganize
names(Data)<-ObjectName #Give names to the objects
rm(list=setdiff(ls(), c("Data","ObjectName", "dates","Condition","CalciumSolutions",
                        "twoFactorStats","substrRight","OrganizeAtable","processTrainCap")))

#The data is imported and organized Sampling Day(Donor)_Condition_Calcium Solution and protocol

# to process the first 50ms pulse
for (i in 1:length(Data)){
  Data[[i]]$First50ms$CapNorm<-Data[[i]]$First50ms$Cap1...6/Data[[i]]$First50ms$`C-slow[F]...12`
  Data[[i]]$First50ms$ICaNorm<-(Data[[i]]$First50ms$`ICa[A]...7`+Data[[i]]$First50ms$`Leak[A]...10`)/
    Data[[i]]$First50ms$`C-slow[F]...12`
  Data[[i]]$First50ms$INaaNorm<-(Data[[i]]$First50ms$`INa min[A]`+Data[[i]]$First50ms$`Leak[A]...10`)/
    Data[[i]]$First50ms$`C-slow[F]...12`  
}# Substract the leak and normalize the data for the first pulse
first50mspulse<-lapply(Data,"[[",1)
first50mspulse<-OrganizeAtable(first50mspulse,ObjectName)

first50mspulse$CalciumSolution<-as.factor(first50mspulse$CalciumSolution)
first50mspulse$`T2D/NA`<-as.factor(first50mspulse$`T2D/NA`)

# the stats part is preliminary
v1<-first50mspulse$CapNorm[first50mspulse$Good=="yes"]
f1<-first50mspulse$CalciumSolution[first50mspulse$Good=="yes"]  
twoFactorStats(v1,f1)#Cap
v1<-first50mspulse$ICaNorm[first50mspulse$Good=="yes"]
twoFactorStats(v1,f1)

#first train summary // Cap14-Cap1

TrainSummary<-lapply(Data,"[[", 2)
TrainSummary<-OrganizeAtable(TrainSummary,ObjectName)
#organize variables
TrainSummary$CalciumSolution<-as.factor(TrainSummary$CalciumSolution)
TrainSummary$`T2D/NA`<-as.factor(TrainSummary$`T2D/NA`)
TrainSummary$normTotalCap<-TrainSummary$`p14-pre...23`/TrainSummary$`C-slow[F]...20`
v1<-TrainSummary$normTotalCap[TrainSummary$Good=="yes"]
f1<-TrainSummary$CalciumSolution[TrainSummary$Good=="yes"]
twoFactorStats(v1,f1)

# First Train 
CapTrainOne<-lapply(Data,"[[",3)
CapTrainOne<-OrganizeAtable(CapTrainOne,ObjectName)
CapTrainOne<-processTrainCap(CapTrainOne)




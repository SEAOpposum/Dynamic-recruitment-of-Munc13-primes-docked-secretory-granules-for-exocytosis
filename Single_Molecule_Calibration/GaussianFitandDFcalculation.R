## To fit the Gaussian equation in R for the EGFP molecules
library(readxl)
library(survival)
library(fitdistrplus) # For fitting

Dates<-c("250114","250116")
ConversionData<-data.frame(Dates=c("250112",Dates),laser1mW=c(1119,1108,1119),exptime1=c(100,100,100),laser2mW=c(109,108,109),exptime2=c(100,100,100),
                           EGFPDF1=rep(NaN,1,3),EGFPDF1sd=rep(NaN,1,3),EGFPDF2=rep(NaN,1,3),EGFPDF2sd=rep(NaN,1,3))

for (i in 1:dim(ConversionData)[1]) {
  
  if (i==1){
    DFAll <- read_excel(paste("D:/SubmissionM13 Paper/Revision Cell reports/M131vsM132/Exocytosis exp/",Dates[1],"/SM/Out/DFAll.xlsx",sep=""), 
                        col_names = FALSE)  
  }else{
    DFAll <- read_excel(paste("D:/SubmissionM13 Paper/Revision Cell reports/M131vsM132/Exocytosis exp/",Dates[i-1],"/SM/Out/DFAll.xlsx",sep=""), 
                        col_names = FALSE)
  }
  
  
  DFAll<-unlist(DFAll)
  hist(DFAll, probability = TRUE, breaks = 500, col = "lightblue", border = "black",main = "Histogram DF", xlab = "Values")
  
  
  fit <- fitdist(DFAll, "norm")
  plot(fit)
  summary(fit)
  ConversionData$EGFPDF1[i]<-unlist(fit$estimate[1])
  ConversionData$EGFPDF1sd[i]<-unlist(fit$estimate[2])
  
}

ConversionData$EGFPDF2<-(ConversionData$EGFPDF1*ConversionData$laser2mW*ConversionData$exptime2)/(ConversionData$laser1mW*ConversionData$exptime1)
ConversionData$EGFPDF2sd<-(ConversionData$EGFPDF1sd*ConversionData$laser2mW*ConversionData$exptime2)/(ConversionData$laser1mW*ConversionData$exptime1)


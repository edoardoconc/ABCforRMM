#setwd("~/Documents/1university/Magistrale/Anno 2/Primo Semestre/Bayesian Statistics/Project/RCode")
#rm(list = ls())

set.seed(42)
source("slicedWass.R")

############ List of summary stats: 1 for mean, 2 for standard deviation, 3 for ... 

computeNormOfSummaryStat <-function(Y,Yobs, numberOfSummaryStatInList){
  if(numberOfSummaryStatInList == 1){
    computedSummaryStat = abs(mean(Y)-mean(Yobs))
  }
  else if (numberOfSummaryStatInList == 2){
    computedSummaryStat=abs((sd(Y))^{2}/length(Y)-(sd(Yobs))^{2}/length(Yobs))
  }
  else if (numberOfSummaryStatInList==3){
    p = seq(0,10,0.5)/10
     computedSummaryStat = abs(sum(abs(quantile(Y[,1],probs=p)- quantile(Yobs[,1],probs=p)))+sum(abs(quantile(Y[,2],probs=p)- quantile(Yobs[,2],probs=p)))) 
  }
  else if(numberOfSummaryStatInList==4){
    computedSummaryStat = slicedWass(Y,Yobs)
  }
  
  return(computedSummaryStat)
}

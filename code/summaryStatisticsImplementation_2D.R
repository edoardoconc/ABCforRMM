
source("slicedWass.R")

#Arguments:
#Y:                            generated data
#Yobs:                         observed data
#numberOfSummaryStatInList     List of summary stats: 1 for mean, 2 for standard deviation, 3 for equispaced quantiles, 4 for sliced Wasserstein

#Value:
#it returns the value of the selected distance between the 2 data.

computeNormOfSummaryStat2D <-function(Y,Yobs, numberOfSummaryStatInList){
  if (numberOfSummaryStatInList==2){
    computedSummaryStat = wasserstein(pp(Y),pp(Yobs),p=2)
  }
  if (numberOfSummaryStatInList==3){
    p = seq(0,10,0.5)/10
    computedSummaryStat = abs(sum(abs(quantile(Y[,1],probs=p)- quantile(Yobs[,1],probs=p)))+sum(abs(quantile(Y[,2],probs=p)- quantile(Yobs[,2],probs=p)))) 
  }
  else if(numberOfSummaryStatInList==4){
    computedSummaryStat = slicedWass(Y,Yobs)
  }
  
  return(computedSummaryStat)
}

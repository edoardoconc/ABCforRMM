

#Arguments:
#Y:                            generated data
#Yobs:                         observed data
#numberOfSummaryStatInList     List of summary stats: 1 for mean, 2 for standard deviation, 3 for equispaced quantiles

#Value:
#it returns the value of the selected distance between the 2 data.

computeNormOfSummaryStat1D <-function(Y,Yobs, numberOfSummaryStatInList){
  if(numberOfSummaryStatInList == 1){
    computedSummaryStat = abs(mean(Y)-mean(Yobs))
  }
  else if (numberOfSummaryStatInList == 2){
    computedSummaryStat=abs((sd(Y))^{2}/length(Y)-(sd(Yobs))^{2}/length(Yobs))
  }
  else if (numberOfSummaryStatInList == 3){
    p = seq(0,10)/10
    computedSummaryStat = sum(abs(quantile(Y,probs=p)- quantile(Yobs,probs=p))) 
  }
  
  return(computedSummaryStat)
}

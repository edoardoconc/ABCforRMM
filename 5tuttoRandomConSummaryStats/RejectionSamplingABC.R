#setwd("~/Documents/1university/Magistrale/Anno 2/Primo Semestre/Bayesian Statistics/Project/RCode")
#rm(list = ls())
set.seed(42)

source("dataModelGen.R")
source("acceptOrReject.R")

RejectionSamplingABC <- function(Yobs, iter,init) {
  acceptedORReject <- vector(length = iter)
  
  meanSample <- list(0)
  
  varSample <- list(0)
  weightSample <- list(0)
  numOfMixCompSample <- list(0)
  SampleMeanDist <- vector (length = iter, mode = "numeric")
  SampleVarDist <- vector (length = iter, mode = "numeric")
  SampleQuantileDist <- vector (length = iter, mode = "numeric")
  
  #Initialisation: needs to be set
  meanSample[[1]] <- unlist(init[1])
  varSample[[1]] <- unlist(init[2])
  weightSample[[1]]<- unlist(init[3])
  numOfMixCompSample[[1]] <- init[4]
  
  
  ThetaProposedZero = list(0)
  ThetaProposedZero[[1]] = unlist(meanSample[[1]]) 
  ThetaProposedZero[[2]] = unlist(varSample[[1]])
  ThetaProposedZero[[3]] = unlist(weightSample[[1]])
  
  SampleMeanDist[1] = computeNormOfSummaryStat(dataGeneration(length(Yobs),ThetaProposedZero),Yobs,1)
  SampleVarDist[1] =  computeNormOfSummaryStat(dataGeneration(length(Yobs),ThetaProposedZero),Yobs,2)
  SampleQuantileDist[1] =  computeNormOfSummaryStat(dataGeneration(length(Yobs),ThetaProposedZero),Yobs,3)
    
  pb <- txtProgressBar(min = 1, max = iter, initial = 1, style = 3)
  
  for(i in 2:iter){
    ThetaProposed <- samplingDensityProposalAndPrior()
    
    Y <- dataGeneration(length(Yobs),ThetaProposed)
    
    meanSample[[i]] = ThetaProposed[[1]]
    varSample[[i]] = ThetaProposed[[2]]
    weightSample[[i]] = ThetaProposed[[3]]
    numOfMixCompSample[[i]] = ThetaProposed[[4]]
    SampleMeanDist[i] = computeNormOfSummaryStat(Y,Yobs,1)
    SampleVarDist[i] = computeNormOfSummaryStat(Y,Yobs,2)
    SampleQuantileDist[i] =  computeNormOfSummaryStat(Y,Yobs,3)
    
    setTxtProgressBar(pb, i)
  }
  close(pb)
  return(data.frame(cbind("sampledMean" = meanSample,"SampledVar" = varSample,
                          "sampledWeight" = weightSample, "sampledNumOfMixComp" = numOfMixCompSample, 
                          "SampleMeanDist" = SampleMeanDist, "SampleVarDist" = SampleVarDist,"SampleQuantileDist" = SampleQuantileDist
                          )))
}


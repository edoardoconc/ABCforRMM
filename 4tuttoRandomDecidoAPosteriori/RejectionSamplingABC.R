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
  WassDist <-vector (length = iter, mode = "numeric")
  acceptedORRejected <- vector (length = iter, mode = "numeric")
  
  #Initialisation: needs to be set
  meanSample[[1]] <- init[1]
  varSample[[1]] <- init[2]
  weightSample[[1]]<-init[3]
  numOfMixCompSample[[1]] <- init[4]
  
  
  ThetaProposedZero = list(0)
  ThetaProposedZero[[1]] = unlist(meanSample[[1]]) 
  ThetaProposedZero[[2]] = unlist(varSample[[1]])
  ThetaProposedZero[[3]] = unlist(weightSample[[1]])
  
  WassDist[1] =  wasserstein1d(Yobs, dataGeneration(length(Yobs),ThetaProposedZero),p=2)
  
  pb <- txtProgressBar(min = 1, max = iter, initial = 1, style = 3)
  
  for (i in 2:iter){
    ThetaProposed <- samplingDensityProposalAndPrior()
    
    Y <- dataGeneration(length(Yobs),ThetaProposed)
    
    #acceptedORRejected[i] = acceptOrReject(Yobs, Y)

    
    meanSample[[i]] = ThetaProposed[[1]]
    varSample[[i]] = ThetaProposed[[2]]
    weightSample[[i]] = ThetaProposed[[3]]
    numOfMixCompSample[[i]] = ThetaProposed[[4]]
    WassDist[i] = wasserstein1d(Yobs,Y,p=2)
        
    setTxtProgressBar(pb, i)
  }
  close(pb)
  
  return(data.frame(cbind("sampledMean" = meanSample,"SampledVar" = varSample,
                          "sampledWeight" = weightSample, "sampledNumOfMixComp" = numOfMixCompSample, "WassDist" = WassDist
                          )))
}


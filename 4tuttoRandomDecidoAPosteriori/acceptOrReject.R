#setwd("~/Documents/1university/Magistrale/Anno 2/Primo Semestre/Bayesian Statistics/Project/RCode")
#rm(list = ls())

set.seed(42)

#source("computePrior.R")
#source("KernelComputation.R")
#source("summaryStatisticsImplementation.R")

library(gtools)
#library(Boom)

library(transport)
library(MCMCpack)

#The Prior Distribution is:
samplingDensityProposalAndPrior <- function(){
  lambda = 2
  K <- rpois(1, lambda)+1

  propWeight <- rdirichlet(1, rep(1,K))
  
  shape = 0.5
  scale = 1
  propVar <- rinvgamma(K, shape, scale)
  
  mu = 0
  sigma = 7
  
  propMean = list(0)
  
  for(i in 1:K){
    if(i ==1){
      propMean[[1]] = rnorm(1, mu, sd = sqrt(propVar[i]*sigma^2))
    }
    else{
      propMean[[i]] = rnorm(1, mu, sd = sqrt(propVar[i]*sigma^2))
    }
  }
  
  #return (propMean)
  return(list(propMean,propVar,propWeight,K))
}

#maxValParamSpace<- 1 #since the prior and the proposal are the same.

#acceptOrReject <-function(Yobs, Y){

  #Sobs <-computeNormOfSummaryStat(Yobs,1)
  #S<-computeNormOfSummaryStat(Y,1)
  
  #EpsThreshold = 1.5#2.7
  #if(wasserstein1d(Yobs,Y,p=2) <= EpsThreshold){
  #  return(TRUE)
  #}
  #else{
   # return(FALSE)
  #}
#}  

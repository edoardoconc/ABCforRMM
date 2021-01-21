#setwd("~/Documents/1university/Magistrale/Anno 2/Primo Semestre/Bayesian Statistics/Project/RCode")
#rm(list = ls())

set.seed(42)


Model_Likelihood_1D <- function(numPts,ThetaProposed) { 
  #LikeliHoodx <- function(x){  
    #return(0.5*dnorm(x, ThetaProposed[1], ThetaProposed[2]) + 0.5*dnorm(x, ThetaProposed[1], ThetaProposed[2]))
  #}
  #dataGen = LikeliHoodx(x = seq(-6, 6, length.out = numPts)) #fx or LikeliHoodx
  
  dataGen <- numeric(numPts)
  
  for(i in 1:numPts){
      k <- sample.int(length(ThetaProposed[[3]]), 1L, prob=ThetaProposed[[3]])
      dataGen[i] <- rnorm(1, ThetaProposed[[1]][[k]], sqrt(ThetaProposed[[2]][k]))
  }
  return(dataGen)  
  #return(rnorm(numPts, mean = ThetaProposed[1], sd = ThetaProposed[2]))
}


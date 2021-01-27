
set.seed(42)

#source("computePrior.R")
#source("KernelComputation.R")
#source("summaryStatisticsImplementation.R")
source("NormalInverseGammaPrior.R")


library(gtools)
#library(Boom)
library(transport)
library(MCMCpack)
library(spatstat)

#The Prior Distribution is:
RepulsivePrior <- function(type){

if(type == "StraussProcess"){
  
  outStrauss <- rStrauss(0.01,W =owin(c(-10,10),c(-10,10)))
  
  while (length(outStrauss$x) == 0 ) {

    outStrauss <- rStrauss(0.01,W =owin(c(-10,10),c(-10,10)))
    
  }
  
  propMean    <- as.list(outStrauss$x)
  K           <- length(propMean)
  propWeight  <- rdirichlet(1, rep(1,K))

  shape = 0.5
  scale = 1
  propVar <- rinvgamma(K, shape, scale)

  return(list(propMean,propVar,propWeight,K))
}
 
if(type == "PenttinenProcess"){
  
  outPenttinen <- rPenttinen(0.009, 1, 0.3, W =owin(c(-10,10),c(-10,10)))
  
  while (length(outPenttinen$x) == 0 ) {

    outPenttinen <- rPenttinen(0.009, 1, 0.3, W =owin(c(-10,10),c(-10,10)))
    
  }
  
  propMean    <- as.list(outPenttinen$x)
  K           <- length(propMean)
  propWeight  <- rdirichlet(1, rep(1,K))

  shape = 0.5
  scale = 1
  propVar <- rinvgamma(K, shape, scale)

  return(list(propMean,propVar,propWeight,K))
}

if(type == "DiggleGrattonProcess"){
  
  
  outDiggleGratton <- rDiggleGratton(0.01,0.85,0.9,W =owin(c(-10,10),c(-10,10)))

  while (length(outDiggleGratton$x) == 0 ) {

    outDiggleGratton <- rDiggleGratton(0.01,0.85,0.9,W =owin(c(-10,10),c(-10,10)))
 
  }
  
  propMean    <- as.list(outDiggleGratton$x)
  K           <- length(propMean)
  propWeight  <- rdirichlet(1, rep(1,K))

  shape = 0.5
  scale = 1
  propVar <- rinvgamma(K, shape, scale)

  return(list(propMean,propVar,propWeight,K))
}

return(NormalInverseGammaPrior())

}


set.seed(42)

#source("computePrior.R")
#source("KernelComputation.R")
#source("summaryStatisticsImplementation.R")

library(gtools)
#library(Boom)

library(transport)
library(MCMCpack)

library(spatstat)

#The Prior Distribution is:
StraussPrior <- function(){
  
  # pointP <- ppp(x=c(0, 0), y=c(1,0), window = owin(c(-5, 5), c(-5,5)))
  # pointDPP <- dppm(pointP~1,dppGauss)
  
  # propMean <- as.list(simulate.dppm(pointDPP)$x)
  # print(propMean)
  outStrauss <- rStrauss(0.01,W =owin(c(-10,10),c(-10,10)))
  
  while (length(outStrauss$x) == 0 ) {
    outStrauss <- rStrauss(0.01,W =owin(c(-10,10),c(-10,10)))
    
  }
  
  propMean <- as.list(outStrauss$x)

  K <- length(propMean)
  
  propWeight <- rdirichlet(1, rep(1,K))

  shape = 0.5
  scale = 1
  propVar <- rinvgamma(K, shape, scale)

  return(list(propMean,propVar,propWeight,K))
}
 
PenttinenPrior <- function(){
  
 
  outPenttinen <- rPenttinen(0.009, 1, 0.3, W =owin(c(-10,10),c(-10,10)))
  
  while (length(outPenttinen$x) == 0 ) {
    outPenttinen <- rPenttinen(0.009, 1, 0.3, W =owin(c(-10,10),c(-10,10)))
    
  }
  
  propMean <- as.list(outStrauss$x)
  K <- length(propMean)
  
  propWeight <- rdirichlet(1, rep(1,K))

  shape = 0.5
  scale = 1
  propVar <- rinvgamma(K, shape, scale)

  return(list(propMean,propVar,propWeight,K))
}

DiggleGrattonPrior <- function(){
  
  
  outDiggleGratton <- rDiggleGratton(0.01,0.85,0.9,W =owin(c(-10,10),c(-10,10)))

  
  while (length(outDiggleGratton$x) == 0 ) {
    outDiggleGratton <- rDiggleGratton(0.01,0.85,0.9,W =owin(c(-10,10),c(-10,10)))

    
  }
  
  propMean <- as.list(outStrauss$x)

  K <- length(propMean)
  
  propWeight <- rdirichlet(1, rep(1,K))

  shape = 0.5
  scale = 1
  propVar <- rinvgamma(K, shape, scale)

  return(list(propMean,propVar,propWeight,K))
}

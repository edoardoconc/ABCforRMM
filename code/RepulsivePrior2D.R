set.seed(42)

source("NormalInverseWishartPrior.R")


library(gtools)
#library(Boom)
library(transport)
library(MCMCpack)
library(spatstat)

#The Prior Distribution is:
RepulsivePrior2D <- function(type){
  
  if(type == "StraussProcess"){
    
    outStrauss <- rStrauss(0.01,W =owin(c(-10,10),c(-10,10)))
    
    while (length(outStrauss$x) == 0|| length(outStrauss$y) == 0 ) {
      
      outStrauss <- rStrauss(0.01,W =owin(c(-10,10),c(-10,10)))
      
    }
    
    tmp <-matrix(c(outStrauss$x,outStrauss$y), outStrauss$n)
    propMean <- lapply(seq_len(nrow(tmp)), function(i) tmp[i,])
    
    K           <- outStrauss$n
    propWeight  <- rdirichlet(1, rep(1,K))
    
    nu0DegOfFreedom = 2
    scaleMatrix = matrix(c(1,0,0,1),2,2)
    proprSigma <-list(0) 
    
    for(i in 1:K){
      
      proprSigma[[i]] = riwish(nu0DegOfFreedom,scaleMatrix)
      
    }
    
    
    return(list(propMean,proprSigma,propWeight,K))
  }
  
  if(type == "PenttinenProcess"){
    
    outPenttinen <- rPenttinen(0.009, 1, 0.3, W =owin(c(-10,10),c(-10,10)))
    
    while (length(outPenttinen$x)== 0  || length(outPenttinen$y)== 0 ) {
      
      outPenttinen <- rPenttinen(0.009, 1, 0.3, W =owin(c(-10,10),c(-10,10)))
      
    }
    
    tmp <-matrix(c(outPenttinen$x,outPenttinen$y), outPenttinen$n)
    propMean <- lapply(seq_len(nrow(tmp)), function(i) tmp[i,])

    K           <- outPenttinen$n
    propWeight  <- rdirichlet(1, rep(1,K))
    
    nu0DegOfFreedom = 2
    scaleMatrix = matrix(c(1,0,0,1),2,2)
    proprSigma <-list(0) 
    
    for(i in 1:K){
      
      proprSigma[[i]] = riwish(nu0DegOfFreedom,scaleMatrix)
      
    }
    
    return(list(propMean,proprSigma,propWeight,K))
  }
  
  if(type == "DiggleGrattonProcess"){
    
    
    outDiggleGratton <- rDiggleGratton(0.01,0.85,0.9,W =owin(c(-10,10),c(-10,10)))
    
    while (length(outDiggleGratton$x) == 0 || length(outDiggleGratton$y) == 0  ) {
      
      outDiggleGratton <- rDiggleGratton(0.01,0.85,0.9,W =owin(c(-10,10),c(-10,10)))
      
    }
    
    tmp <-matrix(c(outDiggleGratton$x,outDiggleGratton$y), outDiggleGratton$n)
    propMean <- lapply(seq_len(nrow(tmp)), function(i) tmp[i,])
    
    K           <- outDiggleGratton$n
    
    propWeight  <- rdirichlet(1, rep(1,K))
    
    nu0DegOfFreedom = 2
    scaleMatrix = matrix(c(1,0,0,1),2,2)
    proprSigma <-list(0) 
    
    for(i in 1:K){
      
      proprSigma[[i]] = riwish(nu0DegOfFreedom,scaleMatrix)
      
    }
    
    
    return(list(propMean,proprSigma,propWeight,K))
  }
  
  return(NormalInverseWishartPrior())
  
}
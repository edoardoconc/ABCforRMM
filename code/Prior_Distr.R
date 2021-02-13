#set.seed(42)

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
Prior_Distr <- function(type,lambda=2,shape=0.5,scale=1,mu=0,sigma=7,beta=NULL,W=owin(c(-10,10),c(-10,10)),gamma=1,R=0.3,delta=0.85,rho=0.9,rand_lambda=FALSE,min_lambda=1,max_lambda=8){
  
  if(type=="NIG"){
    #Default case:
    #lambda = 2 
    #shape = 0.5
    #scale = 1
    #mu = 0
    #sigma = 7
    if (rand_lambda==TRUE){
      lambda<-rdunif(n=1,min=min_lambda,max=max_lambda)
    }
    K <- rpois(1, lambda)+1
    
    if(K==1){
      propWeight<-1
    }else{
    propWeight <- rdirichlet(1, rep(1,K))}
    propVar <- rinvgamma(K, shape, scale)
    propMean = list(0)
    
    for(i in 1:K){
      if(i ==1){
        propMean[[1]] = rnorm(1, mu, sd = sqrt(propVar[i]*sigma^2))
      }
      else{
        propMean[[i]] = rnorm(1, mu, sd = sqrt(propVar[i]*sigma^2))
      }
    }
    return(list(propMean,propVar,propWeight,K))
  }
  
  
  if(type == "StraussProcess"){
    #Default case:
    #lambda = 2 
    #shape = 0.5
    #scale = 1
    if(is.null(beta)){beta=0.01} #beta = 0.01
    #W = owin(c(-10,10),c(-10,10))
    
    
    outStrauss <- rStrauss(beta=beta,gamma=gamma,W=W)
    
    while (length(outStrauss$x) == 0 ) {
      
      outStrauss <- rStrauss(beta=beta,gamma=gamma,W=W)
      
    }
    
    propMean    <- as.list(outStrauss$x)
    K           <- length(propMean)
    if(K==1){
      propWeight<-1
    }else{
      propWeight <- rdirichlet(1, rep(1,K))}
    propVar <- rinvgamma(K, shape, scale)
    
    return(list(propMean,propVar,propWeight,K))
  }
  
  if(type == "PenttinenProcess"){
    #Default case:
    #shape = 0.5
    #scale = 1
    if(is.null(beta)){beta=0.009} #beta = 0.009
    #W = owin(c(-10,10),c(-10,10))
    #gamma = 1
    #R = 0.3
    
    outPenttinen <- rPenttinen(beta=beta, gamma=gamma,R=R,W=W)
    
    while (length(outPenttinen$x) == 0 ) {
      
      outPenttinen <- rPenttinen(beta=beta, gamma=gamma,R=R,W=W)
      
    }
    
    propMean    <- as.list(outPenttinen$x)
    K           <- length(propMean)
    if(K==1){
      propWeight<-1
    }else{
      propWeight <- rdirichlet(1, rep(1,K))}
    propVar <- rinvgamma(K, shape, scale)
    
    return(list(propMean,propVar,propWeight,K))
  }
  
  if(type == "DiggleGrattonProcess"){
    #Default case:
    #shape = 0.5
    #scale = 1
    if(is.null(beta)){beta=0.01} #beta = 0.01
    #W = owin(c(-10,10),c(-10,10))
    #delta = 0.85
    #rho=0.9
    
    outDiggleGratton <- rDiggleGratton(beta=beta,delta=delta,rho=rho,W=W)
    
    while (length(outDiggleGratton$x) == 0 ) {
      
      outDiggleGratton <- rDiggleGratton(beta=beta,delta=delta,rho=rho,W=W)
      
    }
    
    propMean    <- as.list(outDiggleGratton$x)
    K           <- length(propMean)
    if(K==1){
      propWeight<-1
    }else{
      propWeight <- rdirichlet(1, rep(1,K))}
    propVar <- rinvgamma(K, shape, scale)
    
    return(list(propMean,propVar,propWeight,K))
  }
  
  if (type!="NIG" & type!="StraussProcess" & type!= "PenttinenProcess" & type!="DiggleGrattonProcess"){
  stop("Wrong type:
       possible types: NIG , StraussProcess , PenttinenProcess , DiggleGrattonProcess")}
  
}

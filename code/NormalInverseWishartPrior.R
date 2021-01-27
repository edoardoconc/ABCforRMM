############### Prior definition 2-D model ####################
## We choose a normal-inverse-wishart distribution as prior  ##
## for scale and shape parameter of a gaussian kernel.       ##
## (μ_k , Σ_K ) ∼ NIW (m_0 , k _0 , ν_0 , Λ_0 )              ##
##                                                           ##
## A Dirichlet prior for the weight.                         ##
## w_1 , . . . , w_K |K ∼ Dirichlet(1, . . . , 1)            ##
##                                                           ##
## And a Poisson for the number of components.               ##
## K − 1 ∼ Poi(λ)                                            ##
###############################################################

set.seed(42)

library(gtools)
library(transport)
library(MCMCpack)

#The Prior Distribution is:
NormalInverseWishartprior <- function(){

  lambda = 2
  K <- rpois(1, lambda)+1

  propWeight <- rdirichlet(1, rep(1,K))
  
  nu0DegOfFreedom = 2
  scaleMatrix = matrix(c(1,0,0,1),2,2)
  proprSigma <-list(0) 

  for(i in 1:K){

    proprSigma[[i]] = riwish(nu0DegOfFreedom,scaleMatrix)

  }
  
  mu = c(0,0)
  k0 = 1/42
    

  propMean <- list(0)
  propMean[[1]] = mvrnorm(1, mu, proprSigma[[1]]/k0 )

  if(K>1){

    for(i in 2:K){

        propMean[[i]] = mvrnorm(1, mu, proprSigma[[i]]/k0 )

    }

  }

  return(list(propMean,proprSigma,propWeight,K))
  
}
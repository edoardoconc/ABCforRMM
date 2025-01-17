############### Prior definition 1-D model ##################
## We choose a normal-inverse-gamma distribution as prior  ##
## for scale and shape parameter of a gaussian kernel.     ##
## (μ_k , σ_k^2 ) ∼ NIG (a_0 , b_0 , m_0 , k_0 )           ##
##                                                         ##
## A Dirichlet prior for the weight.                       ##
## w_1 , . . . , w_K |K ∼ Dirichlet(1, . . . , 1)          ##
##                                                         ##
## And a Poisson for the number of components.             ##
## K − 1 ∼ Poi(λ)                                          ##
#############################################################


library(gtools)
library(transport)
library(MCMCpack)

#Value:
#it returns a list with the proposed mean, variance, weights and number of components k.

NormalInverseGamma <- function(){

  lambda = 2
  K <- rpois(1, lambda)+1

  propWeight <- rdirichlet(1, rep(1,K))
  
  shape = 0.5
  scale = 1
  propVar <- rinvgamma(K, shape, scale)
  
  mu    = 0
  sigma = 7
  
  propMean = list(0)
  
  for(i in 1:K){

    if(i ==1 ){

      propMean[[1]] = rnorm(1, mu, sd = sqrt(propVar[i]*sigma^2))

    }
    else{

      propMean[[i]] = rnorm(1, mu, sd = sqrt(propVar[i]*sigma^2))

    }

  }
  
  return(list(propMean,propVar,propWeight,K))

}

#Value:
#it returns a list with the proposed mean, variance, weights and number of components k.

Normal_ind_InverseGamma <- function(){

  lambda = 2
  K <- rpois(1, lambda)+1

  propWeight <- rdirichlet(1, rep(1,K))
  
  shape = 0.5
  scale = 1
  propVar <- rinvgamma(K, shape, scale)
  
  mu    = 0
  sigma = 7
  
  propMean = list(0)
  
  for(i in 1:K){

    if(i ==1 ){

      propMean[[1]] = rnorm(1, mu, sd = sigma)

    }
    else{

      propMean[[i]] = rnorm(1, mu, sd = sigma)

    }

  }
  
  return(list(propMean,propVar,propWeight,K))

}

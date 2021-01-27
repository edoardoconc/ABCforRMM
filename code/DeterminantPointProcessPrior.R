

library(spatstat)

DeterminantPointProcess <- function(data, ){

	
	dat = ppp(x=c(-10, 10), y=c(0,0), window=owin(c(-10,10),c(-10,10))) 
  # Fit a determinantal point process model to a point pattern dataset as described in Lavancier et al. (2015).  
	fit = dppm(dat ~ 1, dppGauss, method = "mincon")
	sim_fit = simulate.dppm(fit)
  
  while (length(sim_fit$x) == 0 ) {
    
    fit = dppm(dat ~ 1, dppGauss, method = "mincon")
    sim_fit = simulate.dppm(fit)
    
  }
  
  propMean <- as.list(sim_fit$x)
  K <- length(propMean)
  
  propWeight   = rdirichlet(1, rep(1,K))
  propVar      = rinvgamma(K, shape_IG, scale_IG)
  
  return(list(propMean,propVar,propWeight,K))
  
}



DPP_manual <- function(){

  lambda = 2
  shape_IG  = 0.5
  scale_IG  = 1
  
  K            = rpois(1, lambda) + 1
  propWeight   = rdirichlet(1, rep(1,K))
  propVar      = rinvgamma(K, shape_IG, scale_IG) # questi parametri li fissiamo in modo che 
                                                  # la prior mean di propvar matches the var of data  
  theta        = rnorm(1, 0, 50)
  sigma        = rnorm(1, 0, 50)
  
  propMean = list(0)
  propMean[[1]] = rnorm(1, mu, sd = sqrt(propVar[i]*sigma^2))
  
  for(i in 1:K){

  	propMean[[i]] = rnorm(1, mu, sd = sqrt(propVar[i]*sigma^2))

  }
  
  #return (propMean)
  return(list(propMean,propVar,propWeight,K))

}
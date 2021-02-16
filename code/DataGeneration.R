# Generation of the default dataset

library(extraDistr)
rdirichlet<-MCMCpack::rdirichlet

#Arguments:
#dimension:                        dimension of the data
#type:                             possible types: gauss tds laplace
#n:                                number of generated data
#components:                       number of components of the mixture
#weights:                          weights of the components of the mixture
#mean,sd,dof,sigma,mean2d,var2d    parameters of the selected distribution

#Value:
#it returns a matrix (n x dimension) with the generated data selected

DataGeneration <- function(dimension,type,n,components,weights,mean=NULL,sd=NULL,dof=NULL,sigma=NULL,mean2d=NULL,var2d=NULL) {
  
  if (type!="gauss" & type!="tds" & type!="laplace"){

    stop("invalid type")

  }

  Yobs<-NA
  if (dimension == 1){

    Yobs<-numeric(n)
    
    #gaussian
    if (type=="gauss"){

      if (is.empty(mean) || is.empty(sd)){

        stop("mean or variance are missing")

      }

      if (components!=length(weights) || length(mean)!=components || length(sd)!=components){
        stop("dimension does not corresponds")

      }
      
      for (i in 1:n) {

          dummy<-sample.int(components,1,prob=weights)
          Yobs[i]=rnorm(1,mean[dummy],sd[dummy])

      }

    }
    
    #t di student
    if (type=="tds"){

      if (is.empty(dof)){

        stop("dof is missing")

      }

      if (components!=length(dof)){

        stop("dimension does not corresponds")

      }

      for (i in 1:n) {

        dummy<-sample.int(components,1,prob=weights)
        Yobs[i]=rt(1,dof[dummy])

      }
      
    }
    
    
    #laplace
    if (type=="laplace"){

      if (is.empty(sigma) || is.empty(mean)){

        stop("mean or sigma are missing")

      }

      if (components!=length(weights) || length(sigma)!=components){

        stop("dimension does not corresponds")

      }

      for (i in 1:n) {

        dummy<-sample.int(components,1,prob=weights)
        Yobs[i]=extraDistr::rlaplace(1,mean[dummy],sigma[dummy])

      }
      
    }
     
  }
  
  
  
  
  
  if (dimension == 2){

    if (is.empty(mean2d) || is.empty(var2d)){

      stop("mean2d or var2d are missing")

    }

    if (components!=length(mean2d) || length(var2d)!=components){

      stop("dimension does not corresponds")

    }

    if (!is.list(mean2d) || !is.list(var2d)){

      stop("mean2d and var2d must be lists")

    }

    Yobs<-matrix(data=NA,n,2)

    for (i in 1:n) {
      
      dummy<-sample.int(components,1,prob=weights)
      if(type=="gauss"){
        Yobs[i,]=mvrnorm(1,mean2d[[dummy]], var2d[[dummy]])
        
      }
      if (type=="laplace"){
        Yobs[i,]=LaplacesDemon::rmvl(1, mu=mean2d[[dummy]], Sigma=var2d[[dummy]])
      }
      if (type=="tds"){
        Yobs[i,]= LaplacesDemon::rmvt(1, mu=mean2d[[dummy]], S=var2d[[dummy]], df=dof)
      }
    }

  }
  return(Yobs)
  
}
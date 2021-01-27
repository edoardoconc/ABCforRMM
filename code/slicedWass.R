set.seed(42)

library(transport)

slicedWass <- function (Y,Yobs){

  n=10
  theta<-runif(n,min=0,max=2*pi) 

  x<-cos(theta)
  y<-sin(theta)
  xy<-rbind(x,y)

  M_obs<-Yobs%*%xy
  M<-Y%*%xy

  computedSummaryStat<-0

  for (i in 1:n) {

    computedSummaryStat=computedSummaryStat + wasserstein1d(M_obs[,i],M[,i],p=2)

  }

  return(computedSummaryStat)
  
}
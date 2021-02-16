#Arguments:
#SampledPosteriorwithABC:   is the output of RejectionSamplingABC_1D
#Yobs:                      observed data
#tol:                       tollerance

#the function will plot the contour of the mean and the scatterplot of the generated data

PostProcessing_2d <- function(SampledPosteriorWithABC,Yobs,tol=0.002,k=2,x_min=-16,x_max=16,breaks=70) {
  load("LapRepulsive200K.Rdata")
  
  outDF = do.call("rbind", SampledPosteriorWithABC)
  
  out<-outDF[which(outDF[,5]<= quantile(unlist(outDF[,5]),0.0005)),]
  out = as.data.frame(out, nrow = length(out[,1]), ncol =5, byrow = TRUE)
  usare<-out[which(out$sampledNumOfMixComp == k),]
  medie<-usare[1]
  xy<-matrix(data=NA,nrow=(length(unlist(medie))/2),ncol = 2)
  for (i in (1:length(unlist(medie)))) {
    xy[ceiling(i/2),(i+1)%%2+1]<-unlist(medie)[i]
  }
  
  bivn.kde <- kde2d(xy[,1], xy[,2], n = 200,lims=c(-15,17,-15,17) ) 
  image(bivn.kde,main="Contour of Mean")       # from base graphics package
  contour(bivn.kde, add = TRUE)     # from base graphics packag
  
  # threejs Javascript plot         
  library(threejs)
  # Unpack data from kde grid format
  x <- bivn.kde$x; y <- bivn.kde$y; z <- bivn.kde$z
  # Construct x,y,z coordinates
  xx <- rep(x,times=length(y))
  yy <- rep(y,each=length(x))
  zz <- z; dim(zz) <- NULL
  # Set up color range
  ra <- ceiling(16 * zz/max(zz))
  col <- rainbow(16, 2/3)
  # 3D interactive scatter plot
  scatterplot3js(x=xx,y=yy,z=zz,size=0.22,color = col[ra],bg="white")
  
  
}
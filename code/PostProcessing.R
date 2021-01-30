

PostProcessing <- function(SampledPosteriorWithABC, d, tol) {
  
  
  
  out<-SampledPosteriorWithABC[which(
    SampledPosteriorWithABC$SampleQuantileDist <= quantile(unlist(SampledPosteriorWithABC$SampleQuantileDist),tol)),] 
  
  sMean = list(0)
  sVar = list(0)
  sWeight = list(0)
  sK = list(0)
  
  for(i in 1:length(out[[1]])){
    sMean[[i]] = unlist(out[i,][1])
    sVar[[i]] = unlist(out[i,][2])
    sWeight[[i]] = unlist(out[i,][3]) 
    sK[[i]] = unlist(out[i,][4])
  }
  
table(unlist(sK))  

m = max(table(unlist(sK)))
k = which(table(unlist(sK)) == m)

usare<-out[which(out$sampledNumOfMixComp == k),]
x= seq(-12, 12, length.out=10000 )
fx = matrix(data = NA, nrow = length(usare[,1]), ncol = 10000, byrow = TRUE,
            dimnames = NULL)

# da generalizzare
for (j in 1:k){

  for(i in 1:length(usare[,1])){

    fx[i,] = fx[i,] + (unlist(usare[i,3])[j])*dnorm(x, unlist(usare[i,1])[j], unlist(usare[i,2])[j]) 
    
  }

}

avgOutLikelihood = colMeans(fx) 
plot(x,avgOutLikelihood,type='l',ylim = c(0,0.14))
par(new=TRUE)
lines(density(Yobs), col="red")

  
  par(mfrow=c(1,4),cex=0.25)
  par(mar=c(4,6,4,2))
  
  
  a<-mcmc(unlist(usare$sampledMean))
  traceplot(a,main="Traceplot of Mean",cex.main=4,xlab="",cex.axis=4,ylab="",mgp=c(1,2,0))
  hist(a[1:length(a)],breaks = 25,main="Histogram of Mean",xlab="",cex.main=4,cex.axis=4,ylab="",mgp=c(1,2,0),prob=TRUE)
  lines(density(a[1:length(a)]),main="",xlab="",cex.main=4,cex.axis=4,ylab="",mgp=c(1,2,0),col="navyblue",lwd=2)
  plot(table(unlist(sK)),col = "blue4", lwd=10,main="Frequency of K",cex.main=4,cex.axis=4,ylab="",mgp=c(1,2,0),ylim = c(0,max(table(unlist(sK)))+10))
  
  plot(density(Yobs),main = "Density Estimate",cex.main=4,xlab="",cex.axis=4,ylab="",mgp=c(1,2,0))
  lines(x,avgOutLikelihood,type='l',col = "red")
  

}
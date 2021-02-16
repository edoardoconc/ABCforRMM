#Arguments:
#SampledPosteriorwithABC:   is the output of RejectionSamplingABC_1D
#Yobs:                      observed data
#tol:                       tollerance
#k:                         number of components to analize, if NULL it will select the most accepted k

#Value:
#the function plots the traceplot and the histogram of the mean, the frequency of K and the density estimate. 

PostProcessing_1d <- function(SampledPosteriorWithABC,Yobs,tol=0.002,k=NULL,sum_stat=1,x_min=-16,x_max=16,breaks=70) {
  
if(sum_stat==0){
out<-SampledPosteriorWithABC[which(SampledPosteriorWithABC$WassDist <= 
                 quantile(unlist(SampledPosteriorWithABC$WassDist),tol)),]
}else{
out<-SampledPosteriorWithABC[which(SampledPosteriorWithABC$SampleQuantileDist <= 
                 quantile(unlist(SampledPosteriorWithABC$SampleQuantileDist),tol)),]
}
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
if(is.null(k)){
m = max(table(unlist(sK)))
k = min(as.numeric(rownames(table(unlist(sK))))[which(table(unlist(sK))==m)])#se ho più componenti prendo la mistura minore 
}
print(paste("k =",k),quote=FALSE)


post_out <- sorted_data_frame(out[which(out$sampledNumOfMixComp == k),],k) #2 si vede dalla moda della tabella sopra
usare<-out[which(out$sampledNumOfMixComp == k),]
print(paste("analising",dim(usare)[1],"accepted data"),quote=FALSE)
x= seq(x_min,x_max, length.out=10000 )
fx = matrix(data = 0, nrow = length(usare[,1]), ncol = 10000, byrow = TRUE,
            dimnames = NULL)


for(i in 1:length(usare[,1])){
  for (j in 1:k) {
  fx[i,] = fx[i,] +(unlist(usare[i,3])[j])*dnorm(x, unlist(usare[i,1])[j], unlist(usare[i,2])[j])
  }
}

  
avgOutLikelihood = colMeans(fx) 
x11()
par(mfrow=c(1,4),cex=0.25)
par(mar=c(4,6,4,2))

a<-mcmc(unlist(usare$sampledMean))
traceplot(a,main="Traceplot of Mean",cex.main=4,xlab="",cex.axis=4,ylab="",mgp=c(1,2,0))
hist(a[1:length(a)],breaks = breaks,main="Histogram of Mean",xlab="",cex.main=4,cex.axis=4,ylab="",mgp=c(1,2,0),prob=TRUE)
lines(density(a[1:length(a)]),main="",xlab="",cex.main=4,cex.axis=4,ylab="",mgp=c(1,2,0),col="navyblue",lwd=2)


plot(table(unlist(sK)),col = "blue4", lwd=10,main="Frequency of K",cex.main=4,cex.axis=4,ylab="",mgp=c(1,2,0),ylim = c(0,max(table(unlist(sK)))+10))

plot(density(Yobs),main = "Density Estimate",cex.main=4,xlab="",cex.axis=4,ylab="",mgp=c(1,2,0),xlim = c(x_min,x_max))
lines(x,avgOutLikelihood,type='l',col = "red")

}
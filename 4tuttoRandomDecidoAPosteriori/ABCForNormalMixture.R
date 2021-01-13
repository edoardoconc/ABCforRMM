setwd("/home/edoc/Documenti/Bayesian Statistics/Progetto/4tuttoRandomDecidoAPosteriori(1)/4tuttoRandomDecidoAPosteriori")
rm(list = ls())

set.seed(42)               

library(coda)

source("RejectionSamplingABC.R")
source("sorted_data_frame.R")

Yobs<-numeric(1000)
for (i in 1:1000) {
  c = sample.int(2,1,prob=c(0.5,0.5))

  if(c==1){
    Yobs[i]=rnorm(1,-5,1)
  }
  if(c==2){
    Yobs[i]=rnorm(1,5,1)
  }
}
plot(density(Yobs))

SampledPosteriorWithABC = RejectionSamplingABC(Yobs,150000,c(0,1,0.5,2)) ### (0,1,0.5,2) are the starting point 
save(SampledPosteriorWithABC, file = "150000TuttoRand11.Rdata")
save(Yobs, file = "yobs.Rdata")
################################################################################################################################################################
################################################################################################################################################################
out<-SampledPosteriorWithABC[which(SampledPosteriorWithABC$WassDist <= quantile(unlist(SampledPosteriorWithABC$WassDist),0.0015)),]
hist(unlist(out[i,][3]))
################################################################################################################################################################


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

plot(table(unlist(sK)),col = "blue4", lwd=10, ylab = "Frequency of K")
hist(unlist(sK))




k = 2
post_out <- sorted_data_frame(out[which(out$sampledNumOfMixComp == k),],k) #2 si vede dalla moda della tabella sopra


medie <- post_out[,1:k]
varianze <- post_out[,(k+1):(2*k)]
pesi <- post_out[,(2*k+1):(3*k)]

medie = unlist(out$sampledMean)
varianze = unlist(out$SampledVar)
pesi = unlist(out$sampledWeight)

plot(mcmc(unlist(medie))) 
plot(mcmc(unlist(varianze))) 
plot(mcmc(unlist(pesi)))

hist(unlist(medie))
hist(unlist(varianze)) 
hist(unlist(pesi))

k=2
usare<-out[which(out$sampledNumOfMixComp == k),]
x= seq(-12, 12, length.out=10000 )
fx = matrix(data = NA, nrow = length(usare[,1]), ncol = 10000, byrow = TRUE,
            dimnames = NULL)

for(i in 1:length(usare[,1])){
  fx[i,] =(unlist(usare[i,3])[1])*dnorm(x, unlist(usare[i,1])[1], unlist(usare[i,2])[1]) + (unlist(usare[i,3])[2])*dnorm(x, unlist(usare[i,1])[2], unlist(usare[i,2])[2])
}
avgOutLikelihood = colMeans(fx) 
plot(x,avgOutLikelihood,type='l',ylim = c(0,0.14))
par(new=TRUE)
lines(density(Yobs), col="red")

  par(mfrow=c(1,4))
  a<-mcmc(unlist(out$sampledMean))
  traceplot(a,main="Traceplot of Mean")
  hist(a[1:length(a)],breaks = 50,main="Histogram of Mean",xlab="")
  plot(table(unlist(sK)),col = "blue4", lwd=10, ylab = "Frequency of K")
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
  
  

# ################################################################################################################################################################



# ################################################################################################################################################################

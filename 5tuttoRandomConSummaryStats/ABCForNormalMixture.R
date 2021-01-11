setwd("~/Documents/1university/Magistrale/Anno 2/Primo Semestre/Bayesian Statistics/Project/RCode/5tuttoRandomConSummaryStats")
rm(list = ls())

set.seed(42)               

library(coda)

source("RejectionSamplingABC.R")
source("sorted_data_frame.R")
Yobs<-numeric(1000)
for (i in 1:1000) {
  if(sample.int(2,1,prob=c(0.5,0.5))==1){
    Yobs[i]=rnorm(1,-5,1)
  }
  else{
    Yobs[i]=rnorm(1,5,1)
  }
}
plot(density(Yobs))

SampledPosteriorWithABC = RejectionSamplingABC(Yobs,150000,c(list(0,0),list(1,1),
                                                          list(0.5,0.5),2))

save(SampledPosteriorWithABC, file = "150Ksamplemeanvarquantileoutput.Rdata")

out<-SampledPosteriorWithABC[which(#SampledPosteriorWithABC$SampleMeanDist <= quantile(unlist(SampledPosteriorWithABC$SampleMeanDist),0.0525)  
                                   #& SampledPosteriorWithABC$SampleVarDist <= quantile(unlist(SampledPosteriorWithABC$SampleVarDist),0.0525)
                                    SampledPosteriorWithABC$SampleQuantileDist <= quantile(unlist(SampledPosteriorWithABC$SampleQuantileDist),0.005)
                            ),]

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

k = 2
post_out <- sorted_data_frame(out[which(out$sampledNumOfMixComp == k),],k) #2 si vede dalla moda della tabella sopra


medie <- post_out[,1:k]
varianze <- post_out[,(k+1):(2*k)]
pesi <- post_out[,(2*k+1):(3*k)]

plot(mcmc(unlist(medie))) 
plot(mcmc(unlist(varianze))) 
plot(mcmc(unlist(pesi)))

hist(unlist(medie))
hist(unlist(varianze)) 
hist(unlist(pesi))

# ################################################################################################################################################################

plot(mcmc(post_out[,1]))
plot(mcmc(post_out[,2]))

# ################################################################################################################################################################







# 
# 
# 
# ################################################################################################################################################################
# ################################################################################################################################################################
# out<-SampledPosteriorWithABC[which(SampledPosteriorWithABC$WassDist <= quantile(SampledPosteriorWithABC$WassDist,0.01)),]
# hist(unlist(out[i,][4]))
# ################################################################################################################################################################
# 
# 
# sMean = list(0)
# sVar = list(0)
# sWeight = list(0)
# sK = list(0)
# 
# for(i in 1:length(out[[1]])){
#   sMean[[i]] = unlist(out[i,][2])
#   sVar[[i]] = unlist(out[i,][3])
#   sWeight[[i]] = unlist(out[i,][4]) 
#   sK[[i]] = unlist(out[i,][5])
# }
# 
# table(unlist(sK))
# hist(unlist(sK))
# 
# #now consider only the accepted sample for K=2 Then
# actualMean = list(0)
# actualVar = list(0)
# actualWeight = list(0)
# 
# cont =1
# for(i in 1:length(out[[1]])){
#   if(length(sMean[[i]])==2){ ### 2 chosen based on the mode
#     actualMean[[cont]] = sMean[[i]]
#     actualVar[[cont]] = sVar[[i]]
#     actualWeight[[cont]] = sWeight[[i]]
#     
#     cont = cont+1
#   }
# }
# 
# splittedMean = matrix(unlist(actualMean), ncol=2, byrow = TRUE)
# splittedVar = matrix(unlist(actualVar), ncol=2, byrow = TRUE)
# splittedWeights = matrix(unlist(actualWeight), ncol=2, byrow = TRUE)
# 
# final = data.frame(cbind("sampledMean1" = splittedMean[,1],"sampledMean2" = splittedMean[,2],
#                         "sampledVar1" = splittedVar[,1],"sampledVar2" = splittedVar[,2],"sampledWeight1" = splittedWeights[,1],"sampledWeight2" = splittedWeights[,2]
# ))
# 

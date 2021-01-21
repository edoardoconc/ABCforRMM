

PostProcessing <- function(d,tol) {
  
  
  
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

}
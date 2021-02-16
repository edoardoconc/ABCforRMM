
#Arguments:
#data_set:    the output of RejectionSamplingABC_1D, with the same K for each data
#K:           number of components of the muxture considerated

#Value:
#it returns a data frame with the Sample mean, variance, weights and number of components.
#Furthermore it wil order the data set in such a way that for every theta proposed, the mean will be ascending.

sorted_data_frame <- function(data_set,K){
  medie<-matrix(data=NA,nrow=dim(data_set)[1],ncol = K)
  varianze<-matrix(data=NA,nrow=dim(data_set)[1],ncol = K)
  pesi<-matrix(data=NA,nrow=dim(data_set)[1],ncol = K)
  
  m1<-data_set[1]
  v1<-data_set[2]
  p1<-data_set[3]
  for (i in 1:dim(data_set)[1]) {
    for (j in 1:K) {
      
    medie[i,j]<-unlist(m1[,][i])[j]
    varianze[i,j]<-unlist(v1[,][i])[j]
    pesi[i,j]<-unlist(p1[,][i])[j]
    }}
  
  for (i in 1:dim(data_set)[1]) {
    come<-order(t(medie)[,i],decreasing=FALSE)
    medie[i,]=medie[i,come]
    varianze[i,]=varianze[i,come]
    pesi[i,]=pesi[i,come]
    
  }
  
  return(data.frame(cbind("sampledMean" = medie,"SampledVar" = varianze,
                         "sampledWeight" = pesi, "sampledNumOfMixComp" = K
  )))
}
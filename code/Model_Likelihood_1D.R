### Model_Likelihood.R
# We use the Theta proposed to generate a new set of data from the model
# Y|θ ∼  sum(k=1:K){ w_k * N(μ_k , σ_k^2 )} for i = 1, . . . , n

#set.seed(42)

#Arguments: 
#numPts:         number of generated data
#ThetaProposed   parameters of the proposed gaussian mixture

#Value:
#it returns the data generated from the specified mixture

Model_Likelihood_1D <- function(numPts,ThetaProposed) { 
 
  dataGen <- numeric(numPts)
  
  for(i in 1:numPts){

      k <- sample.int(length(ThetaProposed[[3]]), 1L, prob=ThetaProposed[[3]])
      dataGen[i] <- rnorm(1, ThetaProposed[[1]][[k]], sqrt(ThetaProposed[[2]][k]))

  }

  return(dataGen)  
}
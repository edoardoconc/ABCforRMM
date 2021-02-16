### Model_Likelihood_2D.R
# We use the Theta proposed to generate a new set of data from the model
# Y|θ ∼  sum(k=1:K){ w_k * N 2(μ_k , Σ_k )} for i = 1, . . . , n

#set.seed(42)
library(wrswoR)

#Arguments: 
#numPts:         number of generated data
#ThetaProposed   parameters of the proposed 2-D gaussian mixture

#Value:
#it returns the data generated from the specified mixture

Model_Likelihood_2D <- function(numPts,ThetaProposed) { 
 
  out = matrix(data=NA,numPts,2)

  for(i in 1:numPts){

    k = sample_int_crank(length(ThetaProposed[[3]]),1,ThetaProposed[[3]])
    out[i,] = mvrnorm(1, ThetaProposed[[1]][[k]], ThetaProposed[[2]][[k]])

  }

  return(out)

}
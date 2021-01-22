############################# ABC Sampler ##########################################
## We initialize the mc and then at each iteration we generate a new set          ##
## of data from the model (see Model_Likelihood_1D.R) using proposed parameters   ##
## obtained from the priors (see NormalInverseGammaPrior.R).                      ##
## 1. Simulate θ i for i = 1, . . . , N from the prior π(θ)                       ##
## 2. Simulate z i = (z 1 i , . . . , z n i ) for i = 1, . . . , N where iid      ##
##    z j i ∼ f (Y j |θ = θ i ) ∀j = 1, . . . , n                                 ##
## We compute the distance the Dataset and the proposed one using                 ##
## summaries statistics or Wasserstein distance.                                  ##
## 3. For each i = 1, . . . , N compute d(η(z i ), η(y obs ))                     ##
## The acception or rejection step is computed in the post-prossessing in         ##
## order to mantain more flexibility on the posterior inference                   ##
## 4. accept θ i if d(η(z i ), η(y obs )) ≤ tol                                   ##
####################################################################################
set.seed(42)

source("Model_Likelihood_2D.R")
source("NormalInverseWishardPrior.R")
source("summaryStatisticsImplementation_2D.R")


RejectionSamplingABC <- function(Yobs, iter, sum_stat, init) {

  total <-list(0) 
  
  if (sum_stat == 0){ 

    total[[1]] = list("sampledMean" = init[[1]],"SampledSigma" = init[[2]],
                      "sampledWeight" = init[[3]], "sampledNumOfMixComp" = init[[4]], 
                      "SampleQuantileDist" = computeNormOfSummaryStat(dataGeneration(length(Yobs),init),Yobs,3))# modifica

  }
  if (sum_stat == 1){

    total[[1]] = list("sampledMean" = init[[1]],"SampledSigma" = init[[2]],
                      "sampledWeight" = init[[3]], "sampledNumOfMixComp" = init[[4]], 
                      "SampleQuantileDist" = computeNormOfSummaryStat(dataGeneration(length(Yobs),init),Yobs,3))

  }
  
  pb <- txtProgressBar(min = 1, max = iter, initial = 1, style = 3)

  for(i in 2:iter){

    ThetaProposed <- NormalInverseWishardPrior()

    Y <- dataGeneration(length(Yobs),ThetaProposed)

    if (sum_stat == 0){

      total[[1]] = list("sampledMean" = init[[1]],"SampledSigma" = init[[2]],
                        "sampledWeight" = init[[3]], "sampledNumOfMixComp" = init[[4]], 
                        "SampleQuantileDist" = computeNormOfSummaryStat(dataGeneration(length(Yobs),init),Yobs,3))

    }
    if (sum_stat == 1){

      total[[i]] = list("sampledMean" = ThetaProposed[[1]],"SampledSigma" = ThetaProposed[[2]],
                                 "sampledWeight" =ThetaProposed[[3]], "sampledNumOfMixComp" =ThetaProposed[[4]], 
                                  "SampleQuantileDist" = computeNormOfSummaryStat(Y,Yobs,3))

    }

    setTxtProgressBar(pb, i)

  }

  close(pb)

  return(total)

}
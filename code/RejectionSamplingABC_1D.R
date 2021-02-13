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

#set.seed(42)

source("Model_Likelihood_1D.R")
source("NormalInverseGammaPrior.R")
source("RepulsivePrior.R")
source("Prior_Distr.R")


RejectionSamplingABC_1D <- function(Yobs, iter, sum_stat, init, prior_type) {
  
  #Allocation mc
  meanSample              = list(0)
  varSample               = list(0)
  weightSample            = list(0)
  numOfMixCompSample      = list(0)
  
  #Initialisation mc
  meanSample[[1]]         = unlist(init[1])
  varSample[[1]]          = unlist(init[2])
  weightSample[[1]]       = unlist(init[3])
  numOfMixCompSample[[1]] = init[4]
  
  #Initialization ThetaProposed
  ThetaProposedZero       = list(0)
  ThetaProposedZero[[1]]  = unlist(meanSample[[1]]) 
  ThetaProposedZero[[2]]  = unlist(varSample[[1]])
  ThetaProposedZero[[3]]  = unlist(weightSample[[1]])
  
  #Initialization distance
  if (sum_stat == 0){

    WassDist    = vector (length = iter, mode = "numeric")

    WassDist[1] = wasserstein1d(Yobs, Model_Likelihood_1D(length(Yobs),ThetaProposedZero),p=2)

  }
  if (sum_stat == 1){

    SampleMeanDist          = vector (length = iter, mode = "numeric")
    SampleVarDist           = vector (length = iter, mode = "numeric")
    SampleQuantileDist      = vector (length = iter, mode = "numeric")

    SampleMeanDist[1]       = computeNormOfSummaryStat(Model_Likelihood_1D(length(Yobs),ThetaProposedZero),Yobs,1)
    SampleVarDist[1]        = computeNormOfSummaryStat(Model_Likelihood_1D(length(Yobs),ThetaProposedZero),Yobs,2)
    SampleQuantileDist[1]   = computeNormOfSummaryStat(Model_Likelihood_1D(length(Yobs),ThetaProposedZero),Yobs,3)

  }
  
  pb <- txtProgressBar(min = 1, max = iter, initial = 1, style = 3)
  

  for(i in 2:iter){

    #ThetaProposed = RepulsivePrior(prior_type)
    ThetaProposed = Prior_Distr(prior_type, )
    
    Y = Model_Likelihood_1D(length(Yobs),ThetaProposed)
    
    meanSample[[i]]         = ThetaProposed[[1]]
    varSample[[i]]          = ThetaProposed[[2]]
    weightSample[[i]]       = ThetaProposed[[3]]
    numOfMixCompSample[[i]] = ThetaProposed[[4]]

    if (sum_stat == 0){

      WassDist[i] = wasserstein1d(Yobs, Y, p=2)

    }
    if (sum_stat == 1){

      SampleMeanDist[i]     = computeNormOfSummaryStat(Y,Yobs,1)
      SampleVarDist[i]      = computeNormOfSummaryStat(Y,Yobs,2)
      SampleQuantileDist[i] = computeNormOfSummaryStat(Y,Yobs,3)

    }
    
    setTxtProgressBar(pb, i)

  }

  close(pb)
  
  if (sum_stat == 0){

    return(data.frame(cbind("sampledMean" = meanSample,"SampledVar" = varSample,
                            "sampledWeight" = weightSample, "sampledNumOfMixComp" = numOfMixCompSample, 
                            "WassDist" = WassDist)))

  }
  if (sum_stat == 1){

    return(data.frame(cbind("sampledMean" = meanSample,"SampledVar" = varSample,
                            "sampledWeight" = weightSample, "sampledNumOfMixComp" = numOfMixCompSample, 
                            "SampleMeanDist" = SampleMeanDist, "SampleVarDist" = SampleVarDist,
                            "SampleQuantileDist" = SampleQuantileDist)))

  }
   
}
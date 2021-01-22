setwd('/home/edoc/ABCforRMM/5tuttoRandomConSummaryStats')
rm(list = ls())
set.seed(42)               

library(coda)
library(gtools)
#library(Boom)
library(transport)
library(MCMCpack)
library(coda)
library(MASS)
library(ggplot2)

source("RejectionSamplingABC_1D.R")
source("RejectionSamplingABC_2D.R")
source("sorted_data_frame.R")
source("DataGeneration.R")
source("PostProcessing.R")

#########################################################################
### Parameter choice ####################################################

## Dimension (dim = 1 1-D ; dim = 2 2-D)

dim = 1

## Dataset ( data_choice = 0 default ; data_choice = 1 personalized data)

data_choice = 0

if (data_choice == 0)
  data = DataGeneration(dim)
# see function DataGEneration.R to modify other parameter

if (data_choice == 1)
  data = load(file = '...') # insert data

## Summary statistic 
# sum_stat = 0 use Wasserstein distance to evaluate the distance between data and proposed approximation
# sum_stat = 1 use Summary statistics to evaluate the distance between data and proposed approximation

sum_stat = 0

## Accepting Tollerance on the distance 
# usually we want to accept 5-10%

tol = 0.005

## Number of iteration

iter = 15000

## Initialization of the Markov chains

if (dim == 1){
  mean = list(0,0)
  var = list(1,1)
  weight = list(0.5,0.5)
  n_components = 2
}

if (dim == 2){
  mean = list(c(0,0),c(0,0))
  cov = list(matrix(c(1,0,0,1),2,2),matrix(c(1,0,0,1),2,2))
  weight = c(0.5,0.5)
  n_components = 2
}
  
########################################################################
### running the ABC sampler ############################################

if (dim == 1)
  SampledPosteriorWithABC = RejectionSamplingABC_1D(data, iter, sum_stat, c(mean, var, weight, n_components))

if (dim == 2)
  SampledPosteriorWithABC = RejectionSamplingABC_2D(data, iter, sum_stat, list(mean, cov, weight, n_components))

########################################################################
### Post-Processing ####################################################

PostProcessing(d,tol)

########################################################################
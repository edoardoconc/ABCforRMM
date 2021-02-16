setwd('C:/Users/Admin/Desktop/polimi/Magistrale/~Bayesian statistics/Progetto/R/PER GITHUB')
rm(list = ls())
set.seed(42)               

library(coda)
library(gtools)
library(transport)
library(MCMCpack)
library(coda)
library(MASS)
library(ggplot2)

source("RejectionSamplingABC_1D.R")
source("RejectionSamplingABC_2D.R")
source("sorted_data_frame.R")
source("DataGeneration.R")
source("PostProcessing_1d.R")
source("PostProcessing_2d.R")
#########################################################################
### Parameter choice ####################################################

## Dimension (dim = 1 1-D ; dim = 2 2-D)

dim = 1

## Dataset ( data_choice = 0 default ; data_choice = 1 personalized data)

data_choice = 0

if (data_choice == 0){
  data = DataGeneration(dimension=1,type="gauss",n=1000,components=2,weights = c(0.5,0.5),mean=c(-5,5),sd=c(1,1))
}
# see function DataGEneration.R to modify other parameter

if (data_choice == 1)
  data = load(file = '...') # insert data

## Summary statistic 
# sum_stat = 0 use Wasserstein distance to evaluate the distance between data and proposed approximation
# sum_stat = 1 use Summary statistics to evaluate the distance between data and proposed approximation

sum_stat = 1

## Prior choice (type of prior available for the 1-D case)
# non-repulsive: "NIG"
# repulsive: "StraussProcess", "PenttinenProcess", "DiggleGrattonProcess"

prior_type = "NIG"


## Number of iteration

iter = 200000

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
  SampledPosteriorWithABC = RejectionSamplingABC_1D(data, iter, sum_stat, c(mean, var, weight, n_components), prior_type)

if (dim == 2)
  SampledPosteriorWithABC = RejectionSamplingABC_2D(data, iter, sum_stat, list(mean, cov, weight, n_components),prior_type)

########################################################################
### Post-Processing ####################################################

## Accepting Tollerance on the distance 
# usually we want to accept 5-10%

tol = 0.01

PostProcessing_1d(SampledPosteriorWithABC,Yobs=data, tol=tol, sum_stat=sum_stat)

########################################################################

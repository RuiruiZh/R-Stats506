## STATS506 Fall 2018 
## Problem Set 4 Q2
##
## This R script documents a practice of 
## using mclapply to run parallel simulations.
##
## Author: Ruirui Zhang, ruiruiz@umich.edu
## Updated: December 12, 2018 - Last modified date

#! Limit lines to 80 characters with rare exceptions. 
# 80: -------------------------------------------------------------------------

# Remove objects from current environment
rm(list = ls())
ls()

# libraries: ------------------------------------------------------------------
# Load Libraries --------------------------------------------------------------

if (!require("dplyr")) {
  install.packages('dplyr')
  library("dplyr")
}

if (!require("tidyr")) {
  install.packages('tidyr')
  library("tidyr")
}

if (!require("MASS")) {
  install.packages('MASS')
  library("MASS")
}


if (!require("parallel")) {
  install.packages('parallel')
  library("parallel")
}

# Q2: -------------------------------------------------------------------------
# In this question you will modify your answer to Problem Set 3, Q2 to practice
# parallel, asynchronous, and batch computing. Copy the functions from
# part a and c of PS3 Q2 to a new file ps4_q2_funcs.R
# In each of the parts below, let $\beta$ be defined so that 
#   $\beta_i$ = 0.1 if i <= 10, $\beta_i$ = 0 otherwise. 
# and $\Sigma$ be block diagonal with $\Sigma_{ij}$ = $\rho$ 


# Q2a: ------------------------------------------------------------------------
#      Write an R script ps4_q2a.R that sources ps4_q2_funcs.R, and then 
#      uses mclapply to run parallel simulations for 
#      $\rho \in \{.25i\}_{i= -3}^{3}$.
#
#      Let $\sigma$ = 1 and use 10,000 Monte Carlo replications.
#
#      Reorganize the results into a long data frame results_q4a with columns: 
#      "rho", "sigma", "metric", "method", "est", and "se". 
#
#      "Metric" should contain the assessment measure: 
#      FWER, FDR, Sensitivity or Specificity and 
#
#      "method" the multiple comparison method used.
#      The columns "est" and "se" should contain the Monte Carlo estimate and 
#      its standard error, respectively. 
#
# Q2a: Write function----------------------------------------------------------

# set working directory 
setwd("C:/Users/zhang/Desktop/UMich F18/STATS 506/HW4/")

# source file ps4_q2_funcs.R
source(file = "ps4_q2_funcs.R")

# funciton
myFunc = function (case){
  # Parameters: -----------------------------------------------------------------
  i = case
  n = 1e3; p = 1e2; r = .1; rho = 0.25 * i 
  beta = c( rep(.1, floor(r*p)), rep(0, p - floor(r*p)) ) 
  dim(beta) = c(p, 1)
  
  # X ~ N(0, Sigma): -----------------------------------------------------------
  Sigma = p*diag(p)/2
  
  # Ensure it is symmetric, then rescale to give variances all equal to one
  Sigma[lower.tri(Sigma)] = rho
  Sigma = {Sigma + t(Sigma)} / p
  R = chol(Sigma)
  
  # Here is an X for testing: ---------------------------------------------------
  X = matrix( rnorm(n*p), n, p) %*%  R
  
  # Output: A p by mc_rep matrix of p-values
  sigma = 1
  mc_rep = 1e2
  P = sim_beta(X, beta, sigma = sigma, mc_rep = mc_rep)
  
  # Output: A list of fwer, fdr, sens, spec
  metric =
    lapply( c('holm', 'bonferroni', 'BH', 'BY'), function(x){
      evaluate( apply(P, 2, p.adjust, method = x), tp_ind = 1:10)
    })
  
  metric = matrix(unlist(metric), nrow = 16, ncol = 2, byrow = TRUE)
  
  case_result = data.frame(rho = rho,
                           sigma = sigma,
                           method = rep(c('holm', 'bonferroni', 'BH', 'BY'),c(4,4,4,4)), 
                           metric = rep(c("fwer", "fdr", "sens", "spec"),4),
                           est = metric[,1], 
                           se = metric[,2])
  return(case_result)
}

# run in parallel
results = mclapply(-3:3, myFunc)

# reorgnize the results: ------------------------------------------------------
final_result = results[[1]]
for(i in 2:length(results)){
  final_result = rbind(final_result, results[[i]])
}

final_result





## STATS506 Fall 2018 
## Problem Set 4 Q2b
##
## This R script documents a practice of using 
## doParallel to setup a 4 core cluster and then 
## using nested foreach loops to run simulations
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
library("doParallel")

# Q2: -------------------------------------------------------------------------
# In this question you will modify your answer to Problem Set 3, Q2 to practice
# parallel, asynchronous, and batch computing. Copy the functions from
# part a and c of PS3 Q2 to a new file ps4_q2_funcs.R
# In each of the parts below, let $\beta$ be defined so that 
#   $\beta_i$ = 0.1 if i <= 10, $\beta_i$ = 0 otherwise. 
# and $\Sigma$ be block diagonal with $\Sigma_{ij}$ = $\rho$ 


# Q2b: ------------------------------------------------------------------------
#      Use your script from part a as the basis for a new script ps4_q2b.R. 
#      Setup a 4 core cluster using doParallel and then use nested foreach loops to 
#      run simulations for $\rho \in \{.25i\}_{i = -3}^{3}$ and 
#      $\sigma$ = {0.25, 0.5, 1}.
# 
#      Reshape the results as before into results_q4b saved to a file 
#      results_q4b.RData. 
#      Use a PBS file to run this script on the Flux cluster. 
#
#
# Q2b: ------------------------------------------------------------------------

# set working directory 
setwd("/home/ruiruiz/ps4_q2")

# source file ps4_q2_funcs.R
source(file = "ps4_q2_funcs.R")

# how many cores to use in the cluster?
ncores = 4

# set up a cluster called 'c1
c1 = makeCluster(ncores)

# register the cluster
registerDoParallel(c1)

# funciton
myFunc = function (case, sigma_v){
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
  sigma = sigma_v
  mc_rep = 1e3
  P = sim_beta(X, beta, sigma = sigma, mc_rep = mc_rep)

  # Output: A list of fwer, fdr, sens, spec
  metric =
    lapply( c('holm', 'bonferroni', 'BH', 'BY'), function(x){
      evaluate( apply(P, 2, p.adjust, method = x), tp_ind = 1:10)
    })

  metric = matrix(unlist(metric), nrow = 16, ncol = 2, byrow = TRUE)

  case_result = data.frame(i = rep(i,16),
                           rho = rep(rho,16),
                           sigma = rep(sigma,16),
                           method = rep(c('holm', 'bonferroni', 'BH', 'BY'),c(4,4,4,4)),
                           metric = rep(c("fwer", "fdr", "sens", "spec"),4),
                           est = metric[,1],
                           se = metric[,2])
  return(case_result)
}

# do parallel computations with for each
results = foreach(case = -3:3) %:%
  foreach(sigma = c(0.25, 0.5,1))%dopar%{
    result_case = myFunc(case = case, sigma_v = sigma)
    result_case
}



# shut the cluster down when done
stopCluster((c1))

# reorgnize the results: ------------------------------------------------------
temp = results[[1]]
final_result = temp[[1]]
for(i in 2:length(temp)){
  final_result = rbind(final_result, temp[[i]])
}

for(j in 2:length(results)){
  temp = results[[j]]

  final_result = rbind(final_result, temp[[1]])
  for(i in 2:length(temp)){
    final_result = rbind(final_result, temp[[i]])
  }

}


final_result



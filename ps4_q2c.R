## STATS506 Fall 2018 
## Problem Set 4 Q2
##
## This R script documents a practice of using
## the futures package for parallelism.
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
if (!require("parallel")) {
  install.packages('parallel')
  library("parallel")
}

if (!require("future")) {
  install.packages('future')
  library("future")
}

# Q2: -------------------------------------------------------------------------
# In this question you will modify your answer to Problem Set 3, Q2 to practice
# parallel, asynchronous, and batch computing. Copy the functions from
# part a and c of PS3 Q2 to a new file ps4_q2_funcs.R
# In each of the parts below, let $\beta$ be defined so that 
#   $\beta_i$ = 0.1 if i <= 10, $\beta_i$ = 0 otherwise. 
# and $\Sigma$ be block diagonal with $\Sigma_{ij}$ = $\rho$ 


# Q2c: ------------------------------------------------------------------------
#      Modify your script from part a to create ps4_q2c.R which 
#      reads the following arguments from the command line:
#      sigma, mc_rep, and n_cores. 
#
#      Also modify the script to use the futures package for parallelism. 
#      Use a PBS file to run this script as a job array for
#      $\sigma$ = {0.25, 0.5, 1}.
#
# Q2c: ------------------------------------------------------------------------

# set working directory 
setwd("/home/ruiruiz/ps4_q2")

# source file ps4_q2_funcs.R
source(file = "ps4_q2_funcs.R")

# default arguments
args_list = list(
  sigma = 0.25,
  mc_rep = 1e3,
  ncores=1
)

## get parameters from command line
args = commandArgs(trailingOnly = TRUE)
print(args)

# functions for finding named arguments
args_to_list = function(args){
  ind = grep('=', args)  
  args_list = strsplit(args[ind], '=')
  names(args_list) = sapply(args_list, function(x) x[1])
  
  args_list = lapply(args_list, function(x) as.numeric(x[2]))
  args_list
}

# get named arguments
args_list_in = args_to_list(args)

# update non default arguments
ignored = c()
for ( arg in names(args_list_in) ) {
  # Check for unknown argument
  if ( is.null(args_list[[arg]]) ) {
    ignored = c(ignored, arg)
  } else{
    # update if known
    args_list[[arg]] = args_list_in[[arg]]
  }
}

# Print warning message about unknown arguments
if ( length(ignored) > 0 ) {
  cat('Ignoring unkown arguments:',paste(ignored,collapse=', '), '\n')
}


# funciton
myFunc = function (case, sigma, mc_rep){
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
  # sigma = sigma_v
  # mc_rep = mc_rep_v
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


# use package future
ncores = args_list[['ncores']]
sigma = args_list[['sigma']]
mc_rep = args_list[['mc_rep']]
cl = makeCluster(ncores)
plan(cluster, workers = cl)


result = list()
case = seq(-3,3,1)


for (i in 1:length(case)){
  result[[i]] = with(args_list, 
                     value(future({myFunc(case[i], sigma, mc_rep)})))
}



# reorgnize the results: ------------------------------------------------------
final_result = result[[1]]
for(i in 2:length(result)){
  final_result = rbind(final_result, result[[i]])
}
final_result











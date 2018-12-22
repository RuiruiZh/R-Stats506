## STATS506 Fall 2018 
## Problem Set 3 Q1
##
## This R script documents a Monte Carlo study in R
## to compare the performance of different methods that 
## adjust for multiple comparisons. 
##
## Author: Ruirui Zhang, ruiruiz@umich.edu
## Updated: November 10, 2018 - Last modified date

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

if (!require("ggplot2")) {
  install.packages('ggplot2')
  library("ggplot2")
}

if (!require("data.table")) {
  install.packages('data.table')
  library("data.table")
}


# Q2a: ------------------------------------------------------------------------
#      Write a function that accepts matrices X and beta and
#      returns a p by mc_rep matrix of p-values corresponding to 
#      p-values for the hypothesis tests: 
#      H0: beta_i = 0, H1: beta_i != 0
#
#      In addition to X and beta, your function should have arguments
#      sigma and mc_rep controlling 
#      the error variance of Y and number of Monte Carlo replicates.
#
# Q2a: Write function----------------------------------------------------------

pValue_matrix = function(X, beta, sigma, mc_rep) {
  # (0) generate Y 
  n = dim(X)[1]
  p = dim(X)[2]
  
  set.seed(123)
  Y = mvrnorm(n = mc_rep, 
              mu = as.vector(X %*% beta), 
              Sigma = diag(sigma, nrow = n, ncol = n)) 
  dim(Y) # mc_rep * n
  Y = t(Y)
  dim(Y) # n * mc_rep
  
  # (i) compute beta hat
  ## compute the QR decomposition
  QR = qr(t(X) %*% X)
  Q = qr.Q(QR)
  R = qr.R(QR)
  
  ## compute the beta_qrXX
  beta_qrXX = solve(R, t(Q) %*% t(X) %*% Y)
  dim(beta_qrXX) # p * mc_rep
  
  # (ii) estimate the error variance for each Monte Carlo trial

  ## compute Y_hat
  Y_hat = X %*% beta_qrXX
  dim(Y_hat) # n * mc_rep
  
  ## compute estimated error variance
  sigma2_hat = 1/(n-p) * colSums((Y - Y_hat)^2)
  length(sigma2_hat) # mc_rep
  
  # (iii) find the variance of beta_hat
  ## qr decomposition of t(X)* X
  QR = qr(t(X) %*% X)
  Q = qr.Q(QR)
  R = qr.R(QR)
  
  ## variance of beta_hat
  ### inverse of t(X) %*% X
  c_XX = chol(t(X) %*% X)
  invXX = chol2inv(c_XX)
  dim(invXX) # p * p
  
  ### list of var(beta_hat) for each mc_rep
  v = list()
  for(i in 1:mc_rep){v[[i]] = sigma2_hat[i]* invXX }
  length(v) # mc_rep
  dim(v[[1]]) # p * p

  
  # (iv) p-value
  z = list()
  p_value = list()
  for(i in 1:mc_rep) {
    # for each mc_rep
    z[[i]] = beta_qrXX[,i] / sqrt(diag(v[[i]]))
    p_value[[i]] = 2*(1 - pnorm(abs(z[[i]]), 0, 1))
  }
  
  length(z) # mc_rep
  length(z[[1]]) # p * mc_rep
  length(p_value) #mc_rep
  length(p_value[[1]]) # p * mc_rep
  
  class(p_value) 
  
  out = matrix(unlist(p_value), nrow = length(p_value[[1]]), ncol = length(p_value))
  dim(out) # p * mc_rep
  
  return(list(p_val = out, beta_hat = beta_qrXX))
}

# Q2a: Test--------------------------------------------------------------------
# define variables
mc_rep = 3
n = 1000
p = 100
beta = c(rep(1,10), rep(0, p-10))

sigma = 10
I = diag(x = 1, nrow = n, ncol = n)
sigma_X = matrix(data = 0, nrow = p, ncol = p); diag(sigma_X) = 1
sigma_Y = sigma * I

X = mvrnorm(n = n, mu = rep(0, p), Sigma = sigma_X)

set.seed(123)
Y = mvrnorm(n = mc_rep, 
            mu = as.vector(X %*% beta), 
            Sigma = sigma_Y)

# calculate p-value using function pValue_matrix
p_fct_qr = pValue_matrix(X, beta, sigma, mc_rep)


# Q2a: Check-------------------------------------------------------------------

check = list()
for(i in 1: mc_rep){
  # calculate p-value using lm()
  fit = lm(Y[i,] ~ 0 + X); #summary(fit)
  p_lm = summary(fit)$coefficients[,4]
  
  
  # check beta_hat
  beta_fct = p_fct_qr[[2]][,i] 
  beta_lm = coef(fit)
  
  # check p-value
  p_fct = p_fct_qr[[1]][,i] 
  p_lm
  
  # add to check list
  check[[i]] = list(compare_beta_hat = cbind(fct = beta_fct,
                                             lm = beta_lm,
                                             diff = beta_fct - beta_lm),
                    compare_p_value = cbind(fct = p_fct,
                                            lm = p_lm,
                                            diff = p_fct - p_lm))
}

# Q2a: print check ------------------------------------------------------------
check



# Q2b: ------------------------------------------------------------------------
#      Choose sigma_X and simga_Y as you like.
#      Use the Cholesky factorization of sigma_X to generate a single X.
#      Pass X, beta and sigma_Y to your function from the previous part. 
#
#
# Q2b: Define variables--------------------------------------------------------

mc_rep = 500
n = 1000
p = 100
beta = c(rep(1,10), rep(0, p-10))

sigma = 10
I = diag(x = 1, nrow = n, ncol = n)
sigma_X = matrix(data = 0, nrow = p, ncol = p); diag(sigma_X) = 1
sigma_Y = sigma * I

# Q2b: Cholesky factorization of sigma_X---------------------------------------
R = chol(sigma_X)

# generate a single X
set.seed(123)
X = mvrnorm( n = n, mu = rep(0, p), Sigma = R)
dim(X)

# Q2b: Calculate p-value using function pValue_matrix--------------------------
p_fct_ch = pValue_matrix(X, beta, sigma, mc_rep)
p_fct_ch


# Q2c: ------------------------------------------------------------------------
#      Write a function 'evaluate' that 
#      takes your results; and 
#      a set of indices where beta != 0; an d
#      returns Monte Carlo estimates for the following quantities:
#        1. The family wise error rate: FWER
#        2. The false discovery rate: FDR
#        3. The sensitivity: Sensi
#        4. The specificity: Speci
#

# Q2c: function evaluate-------------------------------------------------------
list_measure = c("FWER", "FDR", "Sensi", "Speci")
evaluate = function(result, Ibeta, alpha){
  
  n = length(Ibeta)
  
  # True 
  Ibeta
  
  # Positive
  Rbeta = result< alpha
  
  TP = Ibeta * Rbeta
  TN = (1 - Ibeta) * (1 - Rbeta)
  FP = (1 - Ibeta) * Rbeta
  FN = Ibeta * (1 - Rbeta)
  cbind(colSums(TP), colSums(TN), colSums(FP), colSums(FN))
  
  # 1. The family wise error rate
  FWER = 1L * (colSums(Rbeta * (1 - Ibeta)) > 0)
  
  # 2. The false discovery rate
  FDR = colSums(FP) / colSums(TP+FP) 
  
  # 3. The sensitivity: (+ve|true) / 
  Sensi = colSums(TP) / max(1, colSums(TP + FN))
  
  # 4. The specificity
  Speci = colSums(TN) / max(1, colSums(TN + FP))
  
  # output
  output = round(cbind(FWER, FDR, Sensi, Speci),4)
  return(output)
} 

# Q2c: test function evaluate--------------------------------------------------
result = p_fct_qr[[1]]
Ibeta = beta
names(p_fct_qr)

evaluate(result, Ibeta, 0.05)


# Q2d: ------------------------------------------------------------------------
# Q2d: Apply your function from the previous part to 
#      the matrix of uncorrected P-values generated in part B
#      Use the function p.adjust() 
#      to correct these p-values for multiple comparisons using 
#        1. Bonferroni
#        2. Holm
#        3. BH (Benjamini-Hochberg)
#        4. BY
#     Use your evaluate() function for each set of adjusted p-values
#

# Q2d: list of adjustment method-----------------------------------------------
list_method = c("bonferroni", "holm", "BH", "BY")

list_p_value = p_fct_ch$p_val
dim(list_p_value) # n * mc_rep

# define variables
Ibeta = beta
alpha = 0.05

# Q2d: calculated adjusted P---------------------------------------------------


p_set = list()

p_set[[1]] = evaluate(list_p_value, Ibeta, alpha)

for(i in 1:length(list_method)){ # for each method
  
  method = list_method[i]
  adj_p_value = list_p_value
  
  for(j in 1: dim(list_p_value)[2]){ # for each mc_rep
    
    p_value = list_p_value[,j]
    adj_p_value[,j] = p.adjust(p_value, method)
  }
  
  p_set[[i+1]] = evaluate(adj_p_value, Ibeta, alpha)
}

names(p_set) = c("unadjusted", list_method)
p_set


# Q2e: ------------------------------------------------------------------------
# Q2e: Produce one or more nicely formatted graphs or tables
#      reporting your results. 
#      Briefly discuss what you found. 
#
#

#print check (mc_rep = 2)
check


#***********************************************************************
# combine test results under each p-value adj. method
gather_result = data.frame(p_set[1:5]) %>%
  gather( factor_key = TRUE) %>%
  group_by(key) %>%
  mutate(mc_rep = seq(1:n())) 


# new tibble for each test
test_name = colnames(p_set[[1]])
test_result = list()
for(name in test_name){
  test_result[[name]] =  gather_result[grep(name, gather_result$key),]
}

# plot 
par(mfrow = c(2,2))
for(i in 1:length(test_result)){
  temp = spread(test_result[[i]], key = key, value = value)
  names(temp) = c("Simulation ID", 
                  "Unadjusted", "Bonferroni", "Holm", "BH", "BY")
  boxplot(temp[,-1])
  summary(temp)
}



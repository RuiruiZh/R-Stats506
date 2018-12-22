## STATS506 Fall 2018 
## Problem Set 1 Q3
##
## This R script documents the analysis on the dataset about:
## RECS 2015 data
##
##
## Author: Ruirui Zhang, ruiruiz@umich.edu
## Updated: October 1, 2018 - Last modified date

#! Limit lines to 80 characters with rare exceptions. 
# 80: -------------------------------------------------------------------------

# Remove objects from current environment
rm(list = ls())
ls()

# libraries: ------------------------------------------------------------------
# Install Packages ------------------------------------------------------------
#install.packages('dplyr')

# Load Libraries --------------------------------------------------------------
if (!require("dplyr")) {
  install.packages('dplyr')
  library("dplyr")
}

if (!require("plyr")) {
  install.packages('plyr')
  library("plyr")
}

if (!require("ggplot2")) {
  install.packages('ggplot2')
  library("ggplot2")
}

# Load Data--------------------------------------------------------------------
setwd("C:/Users/zhang/Desktop/UMich F18/STATS 506/HW1")
recs15_Public <- read.csv(file="./recs2015_public_v3.csv", header=TRUE, sep=",")


###############################################################################
# Step 0a: a function to calculate BRR 95CI
###############################################################################

table_BRR_95CI = function (theta_hat, theta_r){
  
  # Define variables
  R = ncol(theta_r)
  Fay_coef = 0.5
  num_row = length(theta_hat)
  theta_SE_by_div = c()
  
  # calculate SE 
  for (i in 1:num_row) {
    
    theta_r_hat_diff = theta_r[i,] - rep(theta_hat[i],R)
    theta_diff_sq = theta_r_hat_diff^2
    theta_diff_sq_sum = sum(theta_diff_sq)
    theta_var = 1/(R * (1 - Fay_coef)^2) * theta_diff_sq_sum
    theta_SE = sqrt(theta_var)
    
    theta_SE_by_div = c(theta_SE_by_div,
                        theta_SE)  
  }
  
  # combine theta_hat and CI95
  result = cbind(
    theta_hat = theta_hat,
    CI_L = theta_hat - 1.96 * theta_SE_by_div,
    CI_U = theta_hat + 1.96 * theta_SE_by_div) 
  
  row.names(result) = label_DIVISION
  result
  
}


###############################################################################
# Step 0b: NWEIGHT, BRRweight
###############################################################################
num_obs = nrow(recs15_Public)
weight_sum = sum(recs15_Public$NWEIGHT)
  
# select "weight" columns
recs15_weight = c()
for (i in 475:571){
  recs15_weight = cbind(recs15_weight,
                        recs15_Public[1:num_obs, i]/ (sum(recs15_Public[,i])*0
                                                      +1))
}

weight_div = c()
for (i in sort(unique(recs15_Public[,1]))){
  weight_div = cbind(weight_div,
                     sum(recs15_Public[which(recs15_Public$DIVISION == i), 
                                       475])
                     )
}


###############################################################################
# Step 0C: Division name
###############################################################################

label_DIVISION = c(
  "New England",
  "Middle Atlantic",
  "East North Central",
  "West North Central", 
  "South Atlantic",
  "East South Central",
  "West South Central",
  "Mountain North", 
  "Mountain South",
  "Pacific")



###############################################################################
# Question 3 (a) --------------------------------------------------------------
# What percent of homes have stucco construction as 
# the major outside wall material 
# within each division?
###############################################################################

# Step 1: calculate theta_hat and theta_r using NWEIGHT & BRRweight
# theta = percent of homes have stucco construction as 
# the major outside wall material

recs15_Public_OW4 = cbind(
  recs15_Public$DIVISION,
  recs15_weight /weight_sum * ((as.numeric(recs15_Public$WALLTYPE == 4))*1+1*0) 
)


# Step 2: group theta_hat, theta_r: row number = division
temp_recs15_Public_OW4_pct_byDiv = c()

for (i in sort(unique(recs15_Public_OW4[,1]))){
  
  temp_recs15_Public_OW4_pct_byDiv = rbind(
    temp_recs15_Public_OW4_pct_byDiv,
    
    colSums(as.numeric(recs15_Public_OW4[,1] == i) * recs15_Public_OW4[,2:98])*
      weight_sum/
      colSums(as.numeric(recs15_Public_OW4[,1] == i)*recs15_weight))
  
}
recs15_Public_OW4_pct_byDiv = temp_recs15_Public_OW4_pct_byDiv
# recs15_Public_OW4_pct_byDiv = c()
# recs15_Public_OW4_pct_byDiv = 
#   for (i in unique(recs15_Public_OW4[,1])){
#     temp_recs15_Public_OW4_pct_byDiv[i,] * weight_sum / weight_div[i]
# }

# Step 3: generate summary table by division 
recs15_Public_OW4_theta_hat = recs15_Public_OW4_pct_byDiv[,1]
recs15_Public_OW4_theta_r = recs15_Public_OW4_pct_byDiv[,-1]

recs15_Public_OW4_summary = 
  table_BRR_95CI(recs15_Public_OW4_theta_hat,
                 recs15_Public_OW4_theta_r)



knitr::kable(recs15_Public_OW4_summary, 
             digits=3,
             row.names = NA)

# Step z: give plot
give_plot_Q3a =
  ggplot(data = NULL,aes(x=label_DIVISION, 
                         y=recs15_Public_OW4_summary[,1],
                         fill = "pink")) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=recs15_Public_OW4_summary[,2], 
                    ymax=recs15_Public_OW4_summary[,3]),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  ggtitle("Percentage of homes have stucco construction 
          as the major outside wall material") +
  xlab("Division") +
  ylab("Percentage of homes") +
  scale_fill_hue(name="Supplement type", # Legend label, use darker colors
                 breaks=c("OJ", "VC"),
                 labels=c("Orange juice", "Ascorbic acid")) 

###############################################################################
# Question 3 (b) --------------------------------------------------------------
# Calculate the average total electricity usage in kilowatt hours 
# in each division.
# Answer the same question stratified by urban and rural status.
###############################################################################

summary(recs15_Public$KWH)

# Step 1: calculate weighted electricity useage using NWEIGHT & BRRweight

recs15_Public_electri = cbind(
  recs15_Public$DIVISION,
  recs15_weight * recs15_Public$KWH
)

recs15_Public_electri_rural = cbind(
  recs15_Public$DIVISION,
  recs15_weight * recs15_Public$KWH *
    (recs15_Public$UATYP10 == "R")
)

recs15_Public_electri_urban = cbind(
  recs15_Public$DIVISION,
  recs15_weight * recs15_Public$KWH *
    (recs15_Public$UATYP10 == "U")
)
# Step 2: calculate theta_hat, theta_r: row number = division
# theta = average total electricity usage in kilowatt hours by division

recs15_Public_electri_avg_byDiv = c()
recs15_Public_electri_rural_avg_byDiv = c()
recs15_Public_electri_urban_avg_byDiv = c()

for (i in sort(unique(recs15_Public_electri[,1]))){
  
  # !all status
  recs15_Public_electri_avg_byDiv = rbind(
    
    recs15_Public_electri_avg_byDiv,
    
    # avg total electricity usage in division i
    colSums(as.numeric(recs15_Public_electri[,1] == i) * 
              recs15_Public_electri[,2:98])/
    colSums(as.numeric(recs15_Public_electri[,1] == i)*
              recs15_weight))
  
  # !rural status
  recs15_Public_electri_rural_avg_byDiv = rbind(
    
    recs15_Public_electri_rural_avg_byDiv,
    
    # avg total electricity usage in division i
    colSums(as.numeric(recs15_Public_electri_rural[,1] == i) * 
              recs15_Public_electri_rural[,2:98])/
      colSums(as.numeric(recs15_Public_electri_rural[,1] == i) * 
                as.numeric(recs15_Public$UATYP10 == "R")*
                recs15_weight))
      
  # !urban status
  recs15_Public_electri_urban_avg_byDiv = rbind(
    
    recs15_Public_electri_urban_avg_byDiv,
    
    # avg total electricity usage in division i
    colSums(as.numeric(recs15_Public_electri_urban[,1] == i) * 
              recs15_Public_electri_urban[,2:98])/
      colSums(as.numeric(recs15_Public_electri_urban[,1] == i) * 
                as.numeric(recs15_Public$UATYP10 == "U")*
                recs15_weight))
}


# Step 3: generate summary table by division 

recs15_Public_electr_theta_hat = recs15_Public_electri_avg_byDiv[,1]
recs15_Public_electr_theta_r = recs15_Public_electri_avg_byDiv[,-1]


recs15_Public_electr_rural_theta_hat = 
  recs15_Public_electri_rural_avg_byDiv[,1]
recs15_Public_electr_rural_theta_r = 
  recs15_Public_electri_rural_avg_byDiv[,-1]

recs15_Public_electr_urban_theta_hat = 
  recs15_Public_electri_urban_avg_byDiv[,1]
recs15_Public_electr_urban_theta_r = 
  recs15_Public_electri_urban_avg_byDiv[,-1]

recs15_Public_electr_summary = 
  table_BRR_95CI(recs15_Public_electr_theta_hat,
                 recs15_Public_electr_theta_r)

recs15_Public_electr_rural_summary = 
  table_BRR_95CI(recs15_Public_electr_rural_theta_hat,
                 recs15_Public_electr_rural_theta_r)

recs15_Public_electr_urban_summary = 
  table_BRR_95CI(recs15_Public_electr_urban_theta_hat,
                 recs15_Public_electr_urban_theta_r)

# cbind(recs15_Public_electr_summary[,1],
#       recs15_Public_electr_rural_summary[,1],
#       recs15_Public_electr_urban_summary[,1])

# step z: give plot

give_plot_Q3b_all =
  ggplot(data = NULL,aes(x=label_DIVISION, 
                         y=recs15_Public_electr_summary[,1],
                         fill = "pink")) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=recs15_Public_electr_summary[,2], 
                    ymax=recs15_Public_electr_summary[,3]),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  ggtitle("Average total electricity usage in kilowatt hours (All)") +
  xlab("Division") +
  ylab("Average total electricity usage in kilowatt hours") +
  scale_fill_hue(name="Supplement type", # Legend label
                 breaks=c("OJ", "VC"),
                 labels=c("Orange juice", "Ascorbic acid"))

give_plot_Q3b_rural =
  ggplot(data = NULL,aes(x=label_DIVISION, 
                         y=recs15_Public_electr_rural_summary[,1],
                         fill = "pink")) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=recs15_Public_electr_rural_summary[,2], 
                    ymax=recs15_Public_electr_rural_summary[,3]),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  ggtitle("Average total electricity usage in kilowatt hours 
          in rural areas") +
  xlab("Division") +
  ylab("Average total electricity usage in kilowatt hours") +
  scale_fill_hue(name="Supplement type", # Legend label, use darker colors
                 breaks=c("OJ", "VC"),
                 labels=c("Orange juice", "Ascorbic acid")) 

give_plot_Q3b_urban =
  ggplot(data = NULL,aes(x=label_DIVISION, 
                         y=recs15_Public_electr_urban_summary[,1],
                         fill = "pink")) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=recs15_Public_electr_urban_summary[,2], 
                    ymax=recs15_Public_electr_urban_summary[,3]),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  ggtitle("Average total electricity usage in kilowatt hours 
          in urban areas") +
  xlab("Division") +
  ylab("Average total electricity usage in kilowatt hours") +
  scale_fill_hue(name="Supplement type", # Legend label, use darker colors
                 breaks=c("OJ", "VC"),
                 labels=c("Orange juice", "Ascorbic acid")) 
###############################################################################
# Question 3 (c) --------------------------------------------------------------
# Which division has the largest disparity between urban and rural ares 
# in terms of the proportion of homes with internet access.
###############################################################################

# Step 1: calculate percent of homes have internet access 
# weighted by NWEIGHT & BRRweight

recs15_Public_INET = cbind(
  recs15_Public$DIVISION,
  recs15_Public$UATYP10,
  recs15_weight / weight_sum * (as.numeric(recs15_Public$INTERNET == 1)) 
)

# Step 2: calculate grouped theta_hat, theta_r, row = division
# theta is the difference in % of homes with internet between urban and rural
# by division.

recs15_Public_INET_pct_byDiv = c()

for (i in unique(recs15_Public_INET[,1])){
  
  recs15_Public_INET_pct_byDiv = rbind(
    recs15_Public_INET_pct_byDiv,
    
    # % for urban - % for rural
    colSums(as.numeric(recs15_Public_INET[,1] == i & 
                         recs15_Public_INET[,2] == 3 ) * 
              recs15_Public_INET[,3:99]) -
      colSums(as.numeric(recs15_Public_INET[,1] == i & 
                           recs15_Public_INET[,2] == 1 ) * 
                recs15_Public_INET[,3:99])
  )
}

# Step 3: generate summary table by division 
recs15_Public_INET_theta_hat = recs15_Public_INET_pct_byDiv[,1]
recs15_Public_INET_theta_r = recs15_Public_INET_pct_byDiv[,-1]

recs15_Public_INET_summary = 
  table_BRR_95CI(recs15_Public_INET_theta_hat,
                 recs15_Public_INET_theta_r)

# plot(recs15_Public_INET_summary)
# qplot(rownames(recs15_Public_INET_summary), 
#       (recs15_Public_INET_summary[,2]))



# Error bars represent standard error of the mean
give_plot_Q3c =
  ggplot(data = NULL,aes(x=label_DIVISION, 
           y=recs15_Public_INET_summary[,1],
           fill = "pink")) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=recs15_Public_INET_summary[,2], 
                    ymax=recs15_Public_INET_summary[,3]),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  ggtitle("Disparity in proportion of homes with internet access 
          between urban and rural (by division)") +
  xlab("Division") +
  ylab("Disparity between urban and rural") +
  scale_fill_hue(name="Supplement type", # Legend label, use darker colors
                 breaks=c("OJ", "VC"),
                 labels=c("Orange juice", "Ascorbic acid")) 


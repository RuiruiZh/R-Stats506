## Problem Set 3, Question 1
## Stats 506, Fall 2018
##
## RECS consumption data is available at:
## https://www.eia.gov/consumption/residential/data/2015/
##
## Author: Ruirui Zhang

# libraries: ------------------------------------------------------------------
library(tidyverse); 
library(data.table)
library(ggplot2)
# data: -----------------------------------------------------------------------
rm(list = ls())
setwd("C:/Users/zhang/Desktop/UMich F18/STATS 506/HW3/ReviseQ1")
file = '../recs2015_public_v3.csv'
if ( !file.exists(file) ){
  recs =  fread('https://www.eia.gov/consumption/residential/data/2015/csv/recs2015_public_v3.csv')
  fwrite(recs, file = file)
} else {
  recs = fread(file)
}

### Question 3a: --------------------------------------------------------------
#   What percent of homes have stucco construction as the major outside wall 
#   material within each division? Which division has the highest proportion? 
#   Which the lowest?
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Division name: ------------------------------------------------------------
label_DIVISION = data.table(DIVISION = seq(1:10),
                            label = c(
                              "New England",
                              "Middle Atlantic",
                              "East North Central",
                              "West North Central", 
                              "South Atlantic",
                              "East South Central",
                              "West South Central",
                              "Mountain North", 
                              "Mountain South",
                              "Pacific"))

# Point estimate: ------------------------------------------------------------
p_OW4 = recs[, .(p_OW4 = sum(NWEIGHT * {WALLTYPE == 4} )/ sum(NWEIGHT)), by = DIVISION]

# Replicate weights: ----------------------------------------------------------
brrwt_cols =  paste0('BRRWT',1:96)
weights =  recs[, c('DOEID', brrwt_cols), with = FALSE]
weights_long = melt(weights, id.vars = 'DOEID', measure.vars = brrwt_cols, 
                    variable.name = 'repl', value.name = 'w')

# Estimates from replicate weights: ------------------------------------------
p_OW4_hat = recs[weights_long, c('w', 'repl', 'WALLTYPE', 'DIVISION'), on = 'DOEID'] %>%
  .[, .(p_OW4_hat = sum( w *{WALLTYPE == 4}) / sum(w)), by = .(DIVISION, repl)]


# Compute CI: -----------------------------------------------------------------
p_OW4_summary = p_OW4[p_OW4_hat, , on = "DIVISION"] %>%
  .[, .(p_OW4_se = 2 * sqrt(mean({p_OW4 - p_OW4_hat}^2))), by = DIVISION] %>%
  .[p_OW4, , on = "DIVISION"] %>%
  .[label_DIVISION, , on = "DIVISION"] %>%
  .[, .("DIVISION(name)" = label,
        p_OW4 = p_OW4,
        p_OW4_lower = p_OW4 - 1.96 * p_OW4_se,
        p_OW4_upper = p_OW4 + 1.96 * p_OW4_se)] 

p_OW4_summary = p_OW4_summary[order(-p_OW4)]



# Table : ---------------------------------------------------------------------
knitr::kable(p_OW4_summary, 
             digits=3)

# Plot : ----------------------------------------------------------------------
give_plot_Q1a =
  ggplot(data = p_OW4_summary,aes(x=`DIVISION(name)`, 
                                  y=p_OW4,
                                  fill = "pink")) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=p_OW4_lower, 
                    ymax=p_OW4_upper),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  ggtitle("Percentage of homes have stucco construction 
          as the major outside wall material") +
  xlab("Division") +
  ylab("Percentage of homes") +
  scale_fill_hue(name="Supplement type", # Legend label, use darker colors
                 breaks=c("OJ", "VC"),
                 labels=c("Orange juice", "Ascorbic acid")) +
  theme( axis.text.x = element_text(size = 8, angle = 90))


### Question 3b: --------------------------------------------------------------
#   What is average total electricity usage in kilowatt hours in each division? 
#   Answer the same question stratified by urban and rural status.
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# function to calculate 95% CI
est_CI = function(theta, theta_hat ){
  
  # Compute standard errors: ----------------------------------------------------
  theta_summary = theta[theta_hat, , on = "DIVISION"] %>%
    .[, .(theta_hat_se = 1/ 0.5 * sqrt( mean({theta_est - theta_hat}^2))), 
      by = DIVISION] %>%
    .[theta, , on = "DIVISION"] %>%
    .[label_DIVISION, , on = "DIVISION"] %>%
    .[, .("DIVISION(name)" = label,
          theta = theta_est,
          theta_lower = theta_est - 1.96 * theta_hat_se,
          theta_upper = theta_est + 1.96 * theta_hat_se)] 
  
  theta_summary = theta_summary[order(-theta)]
  theta_summary
}

# Point estimate: ------------------------------------------------------------
kwh = recs[, .(theta_est = sum(NWEIGHT * KWH )/ sum(NWEIGHT)), keyby = DIVISION] 
kwh_urban = recs[UATYP10 != "R", .(theta_est = sum(NWEIGHT * KWH )/ sum(NWEIGHT)), keyby = DIVISION]
kwh_rural = recs[UATYP10 == "R", .(theta_est = sum(NWEIGHT * KWH )/ sum(NWEIGHT)), keyby = DIVISION]

# Estimates from replicate weights: ------------------------------------------
kwh_hat = recs[weights_long, c('w', 'repl', 'KWH', 'DIVISION'), on = 'DOEID'] %>%
  .[, .(theta_hat = sum(w * KWH) / sum(w)), keyby = .(DIVISION, repl)]
kwh_urban_hat = recs[weights_long, c('w', 'repl', 'KWH', 'DIVISION', 'UATYP10'), on = 'DOEID'] %>%
  .[UATYP10 != "R", .(theta_hat = sum(w * KWH) / sum(w)), keyby = .(DIVISION, repl)]
kwh_rural_hat = recs[weights_long, c('w', 'repl', 'KWH', 'DIVISION', 'UATYP10'), on = 'DOEID'] %>%
  .[UATYP10 == "R", .(theta_hat = sum(w * KWH) / sum(w)), keyby = .(DIVISION, repl)]

# Compute CI: -----------------------------------------------------------------
kwh_summary = est_CI(theta = kwh, theta_hat = kwh_hat)
kwh_urban_summary = est_CI(theta = kwh_urban, theta_hat = kwh_urban_hat)
kwh_rural_summary = est_CI(theta = kwh_rural, theta_hat = kwh_rural_hat)



### Question 3c: --------------------------------------------------------------
#   Which division has the largest disparity between urban and rural areas 
#   in terms of the proportion of homes with Internet access?
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Point estimate: ------------------------------------------------------------
p_INET = recs[, .(theta_est = sum(NWEIGHT * INTERNET )/ sum(NWEIGHT)), keyby = DIVISION]
p_INET_urban = recs[UATYP10 != "R", .(theta_est = sum(NWEIGHT * INTERNET )/ sum(NWEIGHT)), keyby = DIVISION]
p_INET_rural = recs[UATYP10 == "R", .(theta_est = sum(NWEIGHT * INTERNET )/ sum(NWEIGHT)), keyby = DIVISION]
p_INET_diff = p_INET_urban[p_INET_rural, , on = "DIVISION"] %>%
  .[, .(theta_est = theta_est - i.theta_est), by = 'DIVISION']

# Estimates from replicate weights: ------------------------------------------
p_INET_hat = recs[weights_long, c('w', 'repl', 'INTERNET', 'DIVISION'), on = 'DOEID'] %>%
  .[, .(theta_hat = sum( w *INTERNET) / sum(w)), keyby = .(DIVISION)]
p_INET_urban_hat = recs[weights_long, c('w', 'repl', 'INTERNET', 'DIVISION', 'UATYP10'), on = 'DOEID'] %>%
  .[UATYP10 != "R", .(theta_hat = sum( w *INTERNET) / sum(w)), keyby = .(DIVISION)]
p_INET_rural_hat = recs[weights_long, c('w', 'repl', 'INTERNET', 'DIVISION', 'UATYP10'), on = 'DOEID'] %>%
  .[UATYP10 == "R", .(theta_hat = sum( w *INTERNET) / sum(w)), keyby = .(DIVISION)]
p_INET_diff_hat = p_INET_urban_hat[p_INET_rural_hat, , on = c("DIVISION")] %>%
  .[, .(theta_hat = theta_hat - i.theta_hat), keyby = c('DIVISION')]

# Compute CI: -----------------------------------------------------------------
INET_summary = est_CI(theta = p_INET, theta_hat = p_INET_hat)
INET_urban_summary = est_CI(theta = p_INET_urban, theta_hat = p_INET_urban_hat)
INET_rural_summary = est_CI(theta = p_INET_rural, theta_hat = p_INET_rural_hat)
INET_diff_summary = est_CI(theta = p_INET_diff, theta_hat = p_INET_diff_hat)


### Question 3d: --------------------------------------------------------------
#   Which division has the largest disparity between urban and rural areas 
#   in terms of the proportion of having laptop at home?
# +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

# Point estimate: ------------------------------------------------------------
p_LAPT = recs[, .(theta_est = sum(NWEIGHT * ZNUMLAPTOP )/ sum(NWEIGHT)), keyby = DIVISION]
p_LAPT_urban = recs[UATYP10 != "R", .(theta_est = sum(NWEIGHT * ZNUMLAPTOP )/ sum(NWEIGHT)), keyby = DIVISION]
p_LAPT_rural = recs[UATYP10 == "R", .(theta_est = sum(NWEIGHT * ZNUMLAPTOP )/ sum(NWEIGHT)), keyby = DIVISION]
p_LAPT_diff = p_LAPT_urban[p_LAPT_rural, , on = "DIVISION"] %>%
  .[, .(theta_est = theta_est - i.theta_est), by = 'DIVISION']

# Estimates from replicate weights: ------------------------------------------
p_LAPT_hat = recs[weights_long, c('w', 'repl', 'ZNUMLAPTOP', 'DIVISION'), on = 'DOEID'] %>%
  .[, .(theta_hat = sum( w *ZNUMLAPTOP) / sum(w)), keyby = .(DIVISION)]
p_LAPT_urban_hat = recs[weights_long, c('w', 'repl', 'ZNUMLAPTOP', 'DIVISION', 'UATYP10'), on = 'DOEID'] %>%
  .[UATYP10 != "R", .(theta_hat = sum( w *ZNUMLAPTOP) / sum(w)), keyby = .(DIVISION)]
p_LAPT_rural_hat = recs[weights_long, c('w', 'repl', 'ZNUMLAPTOP', 'DIVISION', 'UATYP10'), on = 'DOEID'] %>%
  .[UATYP10 == "R", .(theta_hat = sum( w *ZNUMLAPTOP) / sum(w)), keyby = .(DIVISION)]
p_LAPT_diff_hat = p_LAPT_urban_hat[p_LAPT_rural_hat, , on = c("DIVISION")] %>%
  .[, .(theta_hat = theta_hat - i.theta_hat), keyby = c('DIVISION')]

# Compute CI: -----------------------------------------------------------------
LAPT_summary = est_CI(theta = p_LAPT, theta_hat = p_LAPT_hat)
LAPT_urban_summary = est_CI(theta = p_LAPT_urban, theta_hat = p_LAPT_urban_hat)
LAPT_rural_summary = est_CI(theta = p_LAPT_rural, theta_hat = p_LAPT_rural_hat)
LAPT_diff_summary = est_CI(theta = p_LAPT_diff, theta_hat = p_LAPT_diff_hat)

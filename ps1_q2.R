## STATS506 Fall 2018 
## Problem Set 1 Q2
##
## This R script documents the analysis on the dataset about:
## flights originating in NYC in 2013 and 2014 (up to Oct. 2014).
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
if (!require("nycflights13")) {
  install.packages('nycflights13')
  library("nycflights13")
}
if (!require("dplyr")) {
  install.packages("dplyr")
  library("dplyr")
}

# Load Libraries --------------------------------------------------------------
library(nycflights13)
library(dplyr)

# Load Data--------------------------------------------------------------------
data_airlines_13 = nycflights13::airlines
data_airports_13 = nycflights13::airports
data_flights_13 = nycflights13::flights
data_planes_13 = nycflights13::planes
data_weather_13 = nycflights13::weather

data_flights_14 = 
  read.csv(url(
    "https://raw.githubusercontent.com/wiki/arunsrinivasan/flights/NYCflights14/flights14.csv"))

# Explore Data ----------------------------------------------------------------
## data_airports_13
# head(data_airports_13)
# names(data_airports_13)
# sort(unique(data_airports_13$name))

## data_flights_13 ---------------------------------------------------------------
## year, month, day: date of departure
## carrier: two letter carrier abbreviation. See data_airlines_13 to get name
## origin: see data_airports_13 for additional metadata
# head(data_flights_13)
# names(data_flights_13)
# list(data_flights_13$origin)


# Question 2 (a) --------------------------------------------------------------
# Which airlines were responsible for at least 1% of the flights departing 
# any of the three NYC airports 
# between January 1 and October 31, 2013?
# JFK: John F Kennedy Intl
# EWR: Newark Liberty Intl
# LGA: La Guardia

# Step 1: select all flights 
# departing from any of the three NYC airports
# between January 1 and October 31, 2013
flights_fly_NYC_Oct13 = 
  data_flights_13 %>% 
  filter(year == 2013, month >=1, month <=10) 

# Step 2: select airlines were responsible for
# the flights 
# departing from any of the three NYC airports 
# between January 1 and October 31, 2013
carrier_fly_NYC_Oct13 = 
  flights_fly_NYC_Oct13 %>%
  select(carrier) %>%
  table() %>%
  data.frame()

names(carrier_fly_NYC_Oct13) = c("carrier_name", "carrier_frequency")

# Step 3: summarize airlines were responsible for
# at least 1% the flights 
# departing from any of the three NYC airports 
# between January 1 and October 31, 2013
carrier_fly_NYC_Oct13_0.01 = 
  carrier_fly_NYC_Oct13 %>% 
  mutate( carrier_weightage = 
            carrier_frequency / sum(carrier_frequency)) %>%
  filter(carrier_weightage > 0.01) %>%
  select(carrier_name) %>%
  merge(data_airlines_13,
        by.x = "carrier_name", by.y = "carrier",
        all.x = TRUE) %>%
  data.frame()


names(carrier_fly_NYC_Oct13_0.01) = c('Carrier_Code', 'Carrier_Full_Name')

# Step 4: print out the result
carrier_fly_NYC_Oct13_0.01
# Output a markdown table: ----------------------------------------------------
knitr::kable(carrier_fly_NYC_Oct13_0.01, 
             digits=1, 
             caption='airlines - More than 1% the flights in NYC (1 Jan. - 31 Oct. 2013)')
             
             
# Question 2 (b) --------------------------------------------------------------
# Among all airlines from part 'a',
# Compare the No. & % of annual flights 
# in the first 10mths of 2013 and the first 10 mths of 2014.


# Step 1: select all flights 
# Among all airlines from part 'a',
# between January 1 and October 31, 2013
flights_fly_Oct13 = 
  data_flights_13 %>% 
  filter(year == 2013, between(month, 1,10)) %>%
  filter( carrier %in% as.character(carrier_fly_NYC_Oct13_0.01[,1]) == TRUE)


flights_fly_Oct13_All = 
  data_flights_13 %>% 
  filter(year == 2013, between(month, 1,10))


#chk airlines of selected flights
test_partA_carrier_13 = 
  sort(as.character(unique(carrier_fly_NYC_Oct13_0.01$Carrier_Code)))
test_partB_carrier_13 = 
  sort(unique(flights_fly_Oct13$carrier))

if(all(test_partA_carrier_13 == test_partB_carrier_13)){
  print("Passed chking airplies of selected flights" )
} else{
  print("ALERT: Chk airplies of selected flights")
}
   

# Step 2: select airlines were responsible for
# the flights 
# Among all airlines from part 'a',
# between January 1 and October 31, 2013
carrier_fly_Oct13 = 
  flights_fly_Oct13 %>%
  select(carrier) %>%
  table() %>%
  data.frame()

names(carrier_fly_Oct13) <- c("carrier_name", "carrier_frequency")
carrier_fly_Oct13

# Step 3: summarize airlines were responsible for
# the flights 
# Among all airlines from part 'a',
# between January 1 and October 31, 2013

carrier_fly_Oct13_Summary = 
  carrier_fly_Oct13 %>% 
  mutate(carrier_weightage = 
           carrier_frequency / nrow(flights_fly_Oct13_All)) %>%
  data.frame()

carrier_fly_Oct13_Summary


# Step 4: select all flights 
# Among all airlines from part 'a',
# between January 1 and October 31, 2014
flights_fly_Oct14 = data_flights_14 %>% 
  filter(year == 2014, month >=1, month <=10) %>%
  filter(carrier %in% as.character(carrier_fly_NYC_Oct13_0.01[[1]]) == TRUE) 


flights_fly_Oct14_All = data_flights_14 %>% 
  filter(year == 2014, month >=1, month <=10)
#head(flights_fly_Oct14)
# length(unique(flights_fly_Oct14$carrier))

#chk airlines of selected flights
test_partC_carrier_14 = 
  as.character(
  sort(unique(flights_fly_Oct14$carrier)))

if(all(test_partC_carrier_14 %in% test_partA_carrier_13)){
  print("Passed chking airplies of selected flights" )
} else{
  print("ALERT: Chk airplies of selected flights")
}


# Step 5: select airlines were responsible for
# the flights 
# Among all airlines from part 'a',
# between January 1 and October 31, 2014
temp_carrier_fly_Oct14 = 
  flights_fly_Oct14 %>%
  select(carrier) %>%
  table() %>%
  data.frame() 

carrier_fly_Oct14 = 
  filter(temp_carrier_fly_Oct14,
         temp_carrier_fly_Oct14[,1] %in% 
           as.character(carrier_fly_NYC_Oct13_0.01[[1]]) == TRUE)

names(carrier_fly_Oct14) = c("carrier_name", "carrier_frequency")

carrier_fly_Oct14

# Step 6: summarize airlines were responsible for
# the flights 
# Among all airlines from part 'a',
# between January 1 and October 31, 2014

carrier_fly_Oct14_Summary = 
  carrier_fly_Oct14 %>% 
  mutate( carrier_weightage =
            carrier_frequency / nrow(flights_fly_Oct14_All)) %>%
  data.frame()

carrier_fly_Oct14_Summary

# Step 7: combine the airline summary for
# flights in the first 10 months of 
# 2013 & 2014
carrier_fly_Summary = 
  merge(carrier_fly_Oct13_Summary,carrier_fly_Oct14_Summary,
        by.x = "carrier_name", by.y = "carrier_name",
        all.x = TRUE,
        all.y = TRUE) %>%  
  merge( data_airlines_13,
         by.x = "carrier_name", by.y = "carrier",
         all.x = TRUE) %>%
  select('Airline_Name' = name, 
         '2013_frequency' = carrier_frequency.x, 
         '2013_Pct' = carrier_weightage.x,
         '2014_frequency' = carrier_frequency.y, 
         '2014_Pct' = carrier_weightage.y) 

carrier_fly_Summary

# Step 8: compute percents for each year with 95% CI
temp_2013_pct = unlist(carrier_fly_Summary %>% select("2013_Pct"))

temp_2013_SE = sqrt(
  temp_2013_pct * (1 - temp_2013_pct) / 
    nrow (flights_fly_Oct13_All))

temp_2013_95CI_Lower = temp_2013_pct - 1.96 * temp_2013_SE 
temp_2013_95CI_Upper = temp_2013_pct + 1.96 * temp_2013_SE 


temp_2014_pct = unlist(carrier_fly_Summary %>% select("2014_Pct"))

temp_2014_SE = sqrt(
  temp_2014_pct * (1 - temp_2014_pct) / 
    nrow (flights_fly_Oct14_All))

temp_2014_95CI_Lower = temp_2014_pct - 1.96 * temp_2014_SE 
temp_2014_95CI_Upper = temp_2014_pct + 1.96 * temp_2014_SE 


temp_95CI = cbind('2013_95CI_Lower' = temp_2013_95CI_Lower,
                  '2013_95CI_Upper' = temp_2013_95CI_Upper,
                  '2014_95CI_Lower' = temp_2014_95CI_Lower,
                  '2014_95CI_Upper' = temp_2014_95CI_Upper)

                                                             
carrier_fly_Summary_wCI = cbind(carrier_fly_Summary, temp_95CI)
carrier_fly_Summary_wCI

# Step 9: Compute change in Pct and its 95% CI
temp_SE_pct_change = sqrt(
  temp_2013_pct * (1- temp_2013_pct) / nrow(flights_fly_Oct13_All) +
    temp_2014_pct * (1- temp_2014_pct) / nrow(flights_fly_Oct14_All))

temp_pct_change =  
  carrier_fly_Summary_wCI$`2014_Pct` - 
  carrier_fly_Summary_wCI$`2013_Pct`

temp_pct_change_95CI_Lower = temp_pct_change - 1.96 * temp_SE_pct_change  
temp_pct_change_95CI_Upper = temp_pct_change + 1.96 * temp_SE_pct_change  


temp_pct_change_95CI = cbind (temp_pct_change, 
                              temp_pct_change_95CI_Lower, 
                              temp_pct_change_95CI_Upper)

carrier_fly_Summary_wCI_wPctChg = 
  cbind (carrier_fly_Summary_wCI, temp_pct_change_95CI) 


carrier_fly_Summary_wCI_wPctChg



# Step zzz: format the output table

carrier_fly_Summary_Final = 
  carrier_fly_Summary_wCI_wPctChg %>%
  select('Airline_Name', 
         'Number of Flights in 2013' = '2013_frequency',
         'Percentage in 2013' = '2013_Pct',
         '95% CI for Percentage in 2013 (lower)' = '2013_95CI_Lower',
         '95% CI for Percentage in 2013 (upper)' = '2013_95CI_Upper',
         'Number of Flights in 2014' = '2014_frequency',
         'Percentage in 2014' = '2014_Pct',
         '95% CI for Percentage in 2014 (lower)' = '2014_95CI_Lower',
         '95% CI for Percentage in 2014 (upper)' = '2014_95CI_Upper',
         'Change in Percentage (from 2013 to 2014)' = 'temp_pct_change',
         '95% CI for Change in Percentage in 2014 (lower)' = 'temp_pct_change_95CI_Lower',
         '95% CI for Change in Percentage in 2014 (upper)' = 'temp_pct_change_95CI_Upper'
         ) %>%
    arrange(Airline_Name)
                                         
row.names(carrier_fly_Summary_Final) = seq(1:nrow(carrier_fly_Summary_Final))
carrier_fly_Summary_Final


knitr::kable(carrier_fly_Summary_Final, 
             digits=3, 
             caption='Flights departing from NYC by top 10 airlines (Jan - Oct, 2013 VS 2014',
             row.names = NA)

arrange(carrier_fly_Summary_Final,
        carrier_fly_Summary_Final$`Change in Percentage (from 2013 to 2014)`)


# Biggest increate: 2.18% Delta Air Lines Inc.
# Biggest decreate: 5.41% Endeavor Air Inc. (No flights in 2014)
# Number decreased: 
# carrier_fly_Summary_Final$Airline_Name[c(1,2,4,5,6,7,9,10)]
# [1] "AirTran Airways Corporation" "American Airlines Inc."      "Endeavor Air Inc."          
# [4] "Envoy Air"                   "ExpressJet Airlines Inc."    "JetBlue Airways"            
# [7] "United Air Lines Inc."       "US Airways Inc."
# Number decreased but pct increased:
# carrier_fly_Summary_Final$Airline_Name[c(2,7, 9, 10)]
# [1] "American Airlines Inc." "JetBlue Airways"        "United Air Lines Inc." 
# [4] "US Airways Inc."
# Reason: total number of flights departured from NYC was decreased in 2014 (from 2013)

# temp_1 = carrier_fly_Summary_Final$`Percentage in 2014` - carrier_fly_Summary_Final$`Percentage in 2013`
# 
# temp_2 = carrier_fly_Summary_Final$`Number of Flights in 2014` -  
#         carrier_fly_Summary_Final$`Number of Flights in 2013`
# carrier_fly_Summary_Final$Airline_Name[temp_1 > 0 && temp_2 < 0]
# temp_1 * temp_2 < 0
# temp_2<0  


# Question 2 (c) --------------------------------------------------------------
# Produce a table showing the percent of flights
# each airline is responsible for.
# Limit the table to the airlines identified in part a, and
# include CI for your estimate.
# Which airline is the largest carrier in each airport.

# Step 1: select all flights 
# Among all airlines from part 'a',
# between January 1 and October 31, 2014
flights_fly_Oct14


# Step 2: summarize flights by airport
table_FlightByAirport = function (x_airport){
  
  # select corresponding airlines by requested airports
  flight_Fly_Oct14_AirportX=
    flights_fly_Oct14 %>%
    filter(carrier %in%
             as.character(carrier_fly_NYC_Oct13_0.01[[1]]) == TRUE) %>%
    filter(origin == x_airport) %>%
    select(carrier) %>%
    table() %>%
    data.frame() %>%
    filter(. %in%
             as.character(carrier_fly_NYC_Oct13_0.01[[1]]) == TRUE) 
    
  
  # Compute percentage
  temp_flight_Oct14_byAirport_pct = flight_Fly_Oct14_AirportX$Freq / nrow(flights_fly_Oct14_All)
  
  # Compute SE, 95%CI
  temp_flight_Oct14_byAirport_pct_SE = 
    sqrt(temp_flight_Oct14_byAirport_pct * (1- temp_flight_Oct14_byAirport_pct) /
           nrow(flights_fly_Oct14_All))
  
  temp_flight_Oct14_byAirport_pct_95CI_Lower = 
    temp_flight_Oct14_byAirport_pct -
    1.95 * temp_flight_Oct14_byAirport_pct_SE
  temp_flight_Oct14_byAirport_pct_95CI_Upper = 
    temp_flight_Oct14_byAirport_pct +
    1.95 * temp_flight_Oct14_byAirport_pct_SE
  
  # Create Table
  
  map_name = merge( flight_Fly_Oct14_AirportX, data_airlines_13,
                    by.x = ".", by.y = "carrier",
                    all.x = TRUE)
  
  result = data.frame(
    'Percent of Flight' = temp_flight_Oct14_byAirport_pct,
    '95% CI for Percentage (Lower)' = temp_flight_Oct14_byAirport_pct_95CI_Lower,
    '95% CI for Percentage (Upper)' = temp_flight_Oct14_byAirport_pct_95CI_Upper,
    row.names = map_name$name)
  

  return(result)

}

table_JFK = table_FlightByAirport('JFK')
table_EWR = table_FlightByAirport('EWR')
table_LGA = table_FlightByAirport('LGA')


## STATS506 Fall 2018 
## Problem Set 4 Q1
##
## This R script documents an SQL query to construct a table showing 
## the all-time leader in hits ("H" from the "batting" table) 
## for each birth country ("birthCountry" in the "master" table). 
##
## Author: Ruirui Zhang, ruiruiz@umich.edu
## Updated: December 11, 2018 - Last modified date

#! Limit lines to 80 characters with rare exceptions. 
# 80: -------------------------------------------------------------------------

# Remove objects from current environment
rm(list = ls())
ls()

# libraries: ------------------------------------------------------------------
# Load Libraries --------------------------------------------------------------
if (!require("Lahman")) {
  install.packages('Lahman')
  library("Lahman")
}


# Load Data--------------------------------------------------------------------

# Create a local SQLlite database of the Lahman data
lahman = lahman_sqlite()

# Query the batting table
lahman %>% tbl("BATTING")
lahman %>% tbl("Master")

# SQL 
Result = lahman %>%
  tbl(sql('
      SELECT M.nameFirst AS First_Name, M.nameLast AS Last_Name, M.debut AS Debut, 
             M.birthCountry AS Country_of_Birth, B.Hits
      FROM Master as M
      INNER JOIN (SELECT B1.PlayerID, sum(B1.H) AS Hits
                         FROM batting as B1
                         GROUP BY B1.playerID
                         HAVING Hits >= 200) as B
      ON B.PlayerID = M.PlayerID 
      GROUP BY  M.birthCountry
      HAVING Hits == max(B.Hits)
      ORDER BY B.Hits DESC
    
      ')) %>% collect()

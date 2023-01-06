##############################
# Data Analysis with R
# Michael Herrmann
# University of Konstanz
# Session 10
############################


# Working with hierarchical data:

#  - Clustered data
#  - Multilevel data
#  - Panel data



# remove everything
rm(list = ls())

setwd("/Users/CaitlynLim/Documents/RScripts/Session 10")


# Data set containing all Bundesliga matches from 1999 to 2001

library(rio)
BL <- data.frame(gather_attrs(factorize(import("Bundesliga.dta"))))



### Data aggregation

# aggregate
# 
# Applies a function to selected observations on one or more 
# variables. Observations are selected based on the values of 
# one or more other variables. The result is a data frame.


# Total goals scored by the home and away teams in each season

aggregate(BL[, c("goals_home", "goals_away")], 
          data.frame(BL$season), 
          sum
          )


# Total goals scored by the home and away teams on each matchday 
# in each season

aggregate(BL[, c("goals_home", "goals_away")], 
          data.frame(matchday = BL$matchday, season = BL$season), 
          sum
          )

# Distribution of fouls committed by the home and away teams at 
# each matchday in each season

aggregate(BL[, c("fouls_home", "fouls_away")], 
          list(matchday = BL$matchday, season = BL$season), 
          quantile
          )




### Reshaping data:  wide <--> long

# Suppose we wanted summary statistics of how each team performed 
# in each season. Specifically, we want summary stats of goals, 
# corners, attempts, fouls, etc. for each team. The current 
# layout of the data divides each of these measurements into two 
# columns: home-team measurements and away-team measurements. 
#
# Current data layout:
#
# Observations (rows): matches
# Variables (columns): home-team measurements, 
#                      away-team measurements,
#                      id-variables (season, matchday, etc.)
#
# We want the data to be in the following layout:
#
# Observations (rows): teams within matches
# Variables (columns): team measurements,
#                      id-variables (season, matchday, team, status)


# To achieve this, we reshape the data from "wide" to "long" format:

# First we create a vector containing the names of the columns to reshape

vars <- names(BL)[3:20]

# Then we reshape the data to "long" format

BL <- reshape(BL, direction = "long", varying = vars, sep = "_")

head(BL)

# Note:
#
# R always creates a new id-variable called "id" that identifies the
# observations (rows) in the original "wide" data set - in this case,
# the individual matches.
# 
# R always creates a new id-variable called "time" to identify the
# cluster to which each new observation belongs. Here, the clusters are 
# home and away status. In many other applications the clusters in a
# "wide" data set are the time points (year, month, or day) at which a
# measurement was taken, hence R's convention of naming the cluster
# variable "time".
#
# Note how R determines the levels of "time" from the names of the 
# original variables. This is possible because we told R that, in the 
# original data set, the symbol "_" is used to distiguish a variable's 
# name from its cluster-id.
# 
# The values of any variable not listed in the "varying" argument are
# simply replicated within each cluster.
# 
# We can manually set the names of "time" and "id" when reshaping the
# data: 

BL <- reshape(BL)   # this reverses the previous reshaping 
BL$id <- NULL       # drop the, now redundant, id-variable

# Now we repeat the desired reshaping while assigning names to time and id

BL <- reshape(BL, 
              direction = "long", 
              varying = vars, 
              sep = "_", 
              timevar = "status", 
              idvar = "matchID"
              )

head(BL)


# Now we are ready to compute summary statistics for each team in each 
# season, e.g., the average number of goals, attempts, and fouls per game:

aggregate(BL[, c("goals", "attempts", "fouls")], 
          list(team = BL$team, season = BL$season), 
          mean
          )




## Reshaping the data into a different "wide" layout

# Suppose we wanted the data in the following layout:

# Observations (rows): teams
# Variables (columns): measurements for a given matchday in a given season

# First we need to create a variable identifying season AND matchday

BL$time <- paste(BL$season, BL$matchday, sep = ".")

# Next we reshape BL by clustering on this variable (i.e., by using it as 
# "timevar") and we drop the - now redundant - variables season and 
# matchday during reshaping  

BL <- reshape(BL, 
              direction = "wide", 
              idvar = "team", 
              timevar = "time", 
              drop = c("season", "matchday")
              )

View(BL)



## Exercise 8

# 1. Load the data set "Bundesliga.dta" and create a new variable homeadv for 
#    home advantage by taking the difference between goals_home and goals_away.
# 2. Use an appropriate aggregation function to calculate the average value of 
#    homeadv for each season.
# 3. Reshape the data so that the new units of observation are teams within
#    matches (as we did in Session 10).
# 4. Use an appropriate aggregation function to create a data frame containing
#    the average number of fouls at each matchday in each season. 



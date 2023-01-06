## Exercise 8

# 1. Load the data set "Bundesliga.dta" and create a new variable homeadv for 
#    home advantage by taking the difference between goals_home and goals_away.

rm(list = ls())

setwd("/Users/CaitlynLim/Documents/RScripts/Session 10")
# Data set containing all Bundesliga matches from 1999 to 2001

library(rio)
BL <- data.frame(gather_attrs(factorize(import("Bundesliga.dta"))))

View(BL)

BL$homeadv <- BL$goals_home - BL$goals_away
          

# 2. Use an appropriate aggregation function to calculate the average value of 
#    homeadv for each season.

aggregate(BL[, c("homeadv")], 
          data.frame(BL$season), 
          sum
)

# 3. Reshape the data so that the new units of observation are teams within
#    matches (as we did in Session 10).
vars <- names(BL)[3:20]

# Then we reshape the data to "long" format

BL <- reshape(BL, direction = "long", varying = vars, sep = "_")

head(BL)

BL <- reshape(BL)   # this reverses the previous reshaping 
BL$id <- NULL       # drop the, now redundant, id-variable

BL <- reshape(BL, 
              direction = "long", 
              varying = vars, 
              sep = "_", 
              timevar = "status", 
              idvar = "matchID"
)

head(BL)

# 4. Use an appropriate aggregation function to create a data frame containing
#    the average number of fouls at each matchday in each season.

aggregate(BL[, c("matchday", "season", "fouls")], 
          list(team = BL$team, season = BL$season), 
          mean
)



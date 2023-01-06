getwd()                       # returns current working directory
setwd("/Users/CaitlynLim/Documents/RScripts/Enviroment")

airpol <-read.csv("airpollution.csv")

View(airpol)



ncol(airpol)
nrow(airpol) #526 months of rain data
rain <- airpol$rainfall 
meantemp <- (airpol$maxtemp + airpol$mintemp)/2
View(meantemp)

mean(airpol$rainfall, na.rm = TRUE) #mean rainfall
sd(airpol$rainfall, na.rm = TRUE)

hist(airpol$rainfall, 
     col = "lightblue", 
     breaks = 100
)
#Amount of times it was around 100mm is the highest 

max(airpol$rainfall) #765.9
min(airpol$rainfall) #0.2

lm(meantemp ~ rainfall, data = airpol)
#for every 1mm increase in rainfall, mean temp decrease by 0.003519 degrees

names(airpol)

airpol[1:5, c("maxtemp", "mintemp")]  

airpol[is.na(airpol$maxtemp) | is.na(airpol$mintemp), c("maxtemp","mintemp")] 

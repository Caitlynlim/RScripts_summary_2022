#############################
# Data Analysis with R
# Michael Herrmann
# University of Konstanz
# Session 12
############################


### Functions and loops


# remove everything
rm(list = ls())



### 1. Writing functions


# a simple function

fun <- function(x) x ^ 2

fun(1:10)


# Pythagorean theorem: length of hypotenuse in a right triangle

hyp <- function(a, b) {
  c <- a ^ 2 + b ^ 2
  sqrt(c)
}

hyp(3, 4)  


# sum of values in a vector/matrix

sumof <- function(x) {
  s <- 0
  for (i in 1:length(x)) {
    s <- s + x[i]
  }
  s
}

sumof(-4:4)
sumof(1:10)


# Laakso and Taagepera's "effective number of parties"

seatshares <- c(0.4, 0.3, 0.1, 0.1, 0.05, 0.05)

ENP <- function(x) 1 / sum(x ^ 2)

ENP(seatshares)



### 2. Implicit loops: applying functions repeatedly

# consider the following matrix
m <- matrix(1:12, nrow = 3)
m

# calculating the row sums of m
apply(m, 1, sum)
apply(m, 1, sumof)
rowSums(m)

# calculating the column sums of m
apply(m, 2, sum)
apply(m, 2, sumof)
colSums(m)

colMeans(m)

# calculating the column products
apply(m, 2, prod)


# applying a user-defined function to each row
apply(m, 1, function(x) x[1] + x[2] ^ 2 + x[3] ^ 3)


## Calculating the effective number of parties for three different parliaments

seats <- list(Germany = c(311, 193, 64, 63), 
              Spain = c(186, 110, 11, 5, 16, 7, 5, 3, 2, 2, 1, 1, 1), 
              UK = c(330, 232, 1, 8, 56, 1, 8, 3, 4, 2, 3, 1, 1)
              )
seats

# from seats to seat shares
seatshares <- lapply(seats, function(x) x / sum(x))
seatshares

# from seat shares to effective number of parties
lapply(seatshares, ENP)
sapply(seatshares, ENP)


# the same works with variables in a data frame
seatsdf <- data.frame(Germany = c(311, 193, 64, 63, 0, 0, 0, 0, 0, 0, 0, 0, 0), 
                      Spain = c(186, 110, 11, 5, 16, 7, 5, 3, 2, 2, 1, 1, 1), 
                      UK = c(330, 232, 1, 8, 56, 1, 8, 3, 4, 2, 3, 1, 1)
                      )
seatsdf


# sapply returns a matrix, which we then convert to a data frame
seatsharesdf <- data.frame(sapply(seatsdf, function(x) x / sum(x)))
seatsharesdf

lapply(seatsharesdf, ENP)
sapply(seatsharesdf, ENP)
apply(seatsharesdf, 2, ENP)  


# sapply vs. lapply ?

# Both functions can be used to repeat over elements of a vector, over
# components of a list, or over variables in a data frame. Unlike lapply,
# which always returns a list, sapply tries to "simplify" its output by 
# returning a vector or matrix whenever feasible. The latter means that 
# sapply will coerce factors to character or numeric values in order to 
# achieve a vector or matrix representation while lapply will retain 
# factors.



### 3. Using functions and implicit loops for data manipulation

setwd("/Users/CaitlynLim/Documents/RScripts/Session 12")

library(rio)
library(haven)
soep <- data.frame(as_factor(read_dta("data1.dta")))
soep <- spread_attrs(droplevels(gather_attrs(soep)))


# identifying every factor variable in a data frame
sapply(soep, is.factor)

# showing the names of all factor variables
names(soep[, sapply(soep, is.factor)])

# extracting all factor variables
soep_factors <- soep[, sapply(soep, is.factor)]



## Repeated frequency tables

# frequency tables of variables with similar values
lapply(soep[, 47:57], table, useNA = "always")
sapply(soep[, 47:57], table, useNA = "always")
t(sapply(soep[, 47:57], table, useNA = "always")) # more intuitive output
# frequency tables of variables with different values

lapply(soep[, 42:46], table, useNA = "always")
sapply(soep[, 42:46], table, useNA = "always")    # simplification impossible here
sapply(soep[, 47:58], table, useNA = "always")    # wor12 is different



## Repeating the same recoding procedure across similar variables


# Example 1: Recoding wor01 to wor12 into dummy variables

# First, we collect the variable names in a vector, and we create a 
# vector of names for the recoded variables:

oldnames <- names(soep)[47:58]
newnames <- paste(oldnames, "dummmy", sep = "_")

# Second, we change all variables into binary variables and store
# them in a separate data frame:

soep_recode <- data.frame(sapply(soep[, oldnames], 
                                 function(x) {
                                   levels(x)[levels(x) == 
                                               "Does not apply"
                                             ] <- NA
                                   levels(x) <- c("concerned", 
                                                  "concerned", 
                                                  "not concerned",
                                                  NA
                                                  )
                                   x
                                   }
                                 )
                          )

# Third, we name the new variables differently from the old ones and
# add them to the original data set:

names(soep_recode) <- newnames
soep <- data.frame(soep, soep_recode) 

# Finally, we verify that our recodings are correct:

for (i in 1:length(oldnames)) {
  print(table(soep[, oldnames[i]], soep[, newnames[i]], useNA = "always"))
}

# R does not display the results of the computations it performs inside
# a for-loop. To show these, we have to use print().



# Example 2: Recoding the three satisfaction variables by centering them  
# at zero

# First, we collect the variable names in a vector, and we create a 
# vector of names for the recoded variables:

oldnames <- c("dsat", "hsat", "lsat")
newnames <- paste(oldnames, "centered", sep = "_")

# Next, we do the recoding and renaming, and we add the recoded variables to 
# the data frame:

soep_recode <- data.frame(sapply(soep[, oldnames], 
                                 function(x) {
                                   levels(x)[levels(x) == "Refusal"] <- NA
                                   x <- as.numeric(x) - 6
                                 }
                                 )
                          )
names(soep_recode) <- newnames
soep <- data.frame(soep, soep_recode)

# Finally, we check our coding:

for (i in 1:length(oldnames)) {
  print(table(soep[, oldnames[i]], soep[, newnames[i]], useNA = "always"))
}




### Exercise 9

# run the following lines
library(rio)
library(haven)
soep <- data.frame(as_factor(read_dta("data1.dta")))
soep <- spread_attrs(droplevels(gather_attrs(soep)))


# 1. Use an appropriate "apply" function to tabulate each of the variables 
#    related to respondent dwelling (i.e., eqphea, eqpter, eqpbas, etc). 
#    Make sure to display NAs on each variable.
# 2. Use an appropriate "apply" function to rescale income and hhinc to 
#    thousand Euros (i.e., divide each variable by 1000). Name the new 
#    variables income1000 and hhinc1000 and add them to the soep data set. 
# 3. Check your results from Problem 2 in the usual way by listing the first
#    ten observations on the original variables and the new variables (no 
#    looping or "applying" is necessary to do this).



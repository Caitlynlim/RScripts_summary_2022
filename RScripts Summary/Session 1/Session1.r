############################
# Data Analysis with R
# Michael Herrmann
# University of Konstanz
# Session 1
############################


### Interacting with R

# simple arithmetic input
10 - 5
4 ^ 2
(20 / 6) - 4 ^ 2
4 + 4 *             # input running over two lines
  3             
# arithmetic operators:  +  -  *  /  ^  


# logical statements
10 < 5
10 + 5 == 15
10 + 5 <= 15
1 + 1 != 1
2 > 1 & 7 > 3
2 < 1 | 7 > 3 
# logical and relational operators: 
#  <  >  >=  <=  ==(equivalence)  !=  & (AND)  | (OR)  ! (NOT)


# assignment
x <- log(5) 
x
# "gets-operator": <- 
# The object x now exists in the workspace and can be called at any time within 
# the session (you can also use = in place of <- but this is not recommended)


## Functions and their arguments

# a simple integer sequence
seq(from = 0, to = 20, by = 1)

# arguments have a default ordering, which allows us to omit their names
seq(0, 20, 1)

# some arguments also have default values
seq(0, 20)


# argument ordering and default values can be looked up in the help file 
?seq()

# given the default argument ordering, "length.out" cannot be omitted here
seq(0, 10, length.out = 35)

# as a practical matter, we NEVER use seq() for by-1-sequences
0:20



### First steps in R
rm(list = ls())
# set working directory
getwd()                       # returns current working directory
setwd("/Users/CaitlynLim/Documents/RScripts/Session 1")

# load data set (written in Stata's format)
library(rio)                                         # load required package
library(haven)
soep <- data.frame(as_factor(read_dta("data1.dta"))) # read data set
soep <- spread_attrs(droplevels(gather_attrs(soep))) # cleaning up

?str()

str(soep) #compile in short, just another form of summary
summary(soep) #summary
head(soep) #show first and last part 

#Data Frame: MyDataFrame$HeaderOfColumn


# x <-read.csv("NameOfFile.csv")
# x <- read.table("NameOfFile.txt")

## Inspecting the data

# Displaying variables and values
View(soep)                    # data as spreadsheet
ncol(soep)                    # number of columns/variables
nrow(soep)                    # number of observations/rows
names`(soep)                   # variable names - same as colnames(soep)

# selecting/accessing content
soep[, 1]                     # first column/variable (respondent ID)
soep[10, ]                    # tenth row
soep[1:10, ]                  # first 10 obs. on all variables
soep$state                    # variable state 
soep[, "state"]               # variable state
soep[1:10, "state"]           # first 10 obs. on variable state

soep[1:20, 2:4]               # displays ...?


## Frequency tables and summary statistics

# one-way frequency tables

# employment status
table(soep$emp, useNA = "always")
# life satisfaction
table(soep$lsat, useNA = "always")

# frequencies as proportions

prop.table(table(soep$emp, useNA = "always"))
prop.table(table(soep$lsat, useNA = "always"))
# How would we get percentages instead of proportions?

# two-way frequency tables

# employment status by gender
table(soep$sex, soep$emp, useNA = "always")
# life satisfaction by health satisfaction
table(soep$lsat, soep$hsat, useNA = "always")

# frequencies as (row) proportions
prop.table(table(soep$sex, soep$emp, useNA = "always"), 1)

# mean and standard deviation of income
mean(soep$income, na.rm = TRUE)
sd(soep$income, na.rm = TRUE)

# if one or more values of income are NA, the entire result is NA
mean(soep$income)  

# summary statistics for income
summary(soep$income)  

# for factor variables (like employment), summary returns a table
summary(soep$emp)



### Analyzing data graphically

# univariate analysis

# distribution of income (for non-zero incomes below 200,000 EUR)
hist(soep$income[soep$income > 0 & soep$income < 2e+05], 
     col = "lightblue", 
     breaks = 100
     )

# bivariate analysis

# income by occupation (for non-zero incomes below 200,000 EUR)
boxplot(soep$income ~ soep$emp, 
        subset = soep$income > 0 & soep$income < 2e+05
        )

### Regression analysis: gender wage gap

# regression of income on gender and employment status
lm(income ~ sex + emp, data = soep)

# more detailed results
summary(lm(income ~ sex + emp, data = soep))

# restrict regression analysis to respondents with positive income
summary(lm(income ~ sex + emp, data = soep, subset = income > 0))  



### Exercise 1

# 1. Create a vector x with the following elements (0, 4, 8, 12, 16, 20), 
#    without typing the numbers.
#
# How do Germans live? 
#
# 2. Produce a frequency table of the number of rooms each respondent 
#    lives in, based on the variable "rooms".
# 3. Compute the average housing size (in square feet) in the sample, 
#    based on the variable "size".
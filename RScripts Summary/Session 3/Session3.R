#############################
# Data Analysis with R
# Michael Herrmann
# University of Konstanz
# Session 3
############################


### Data modification

#   - Creating new variables
#   - Changing values of variables
#   - Verifying our results!


# remove everything from the current environment
rm(list = ls())


# set working directory
setwd("/Users/CaitlynLim/Documents/RScripts/Session 3")


# load data set (written in Stata's format)
library(rio)                                         
library(haven) #allows you to read stata format with a read_dta into R
soep <- data.frame(as_factor(read_dta("data1.dta"))) 
soep <- spread_attrs(droplevels(gather_attrs(soep))) 



## Creating and changing variables

# changing variable names
names(soep)
names(soep)[5] <- "gender"
names(soep)

View(soep)

# household income in US dollars (2009 exchange rate)
soep$hhinc_USD <- soep$hhinc * 1.46028 #new variables added at the end
soep[1:100, c("hhinc", "hhinc_USD")]            # check (orig. vs. new var.)
# check missings in particular
soep[is.na(soep$hhinc) | is.na(soep$hhinc_USD), c("hhinc", "hhinc_USD")]


# respondent age at the time the survey was conducted
soep$age <- 2009 - soep$ybirth #2009-birth year = age 
soep[1:100, c("ybirth", "age")]                 # check (orig. vs. new var.)
# check missings in particular
soep[is.na(soep$ybirth) | is.na(soep$age), c("ybirth", "age")]                        


# per capita household income
soep$pchinc <- soep$hhinc / soep$hhsize
soep[100:200, c("hhinc", "hhsize", "pchinc")]   # check
# check missings
soep[is.na(soep$hhinc) | is.na(soep$hhsize) | is.na(soep$pchinc), 
     c("hhinc", "hhsize", "pchinc")
     ]



# dummy variable (factor) for married respondents
summary(soep$mar)                               # inspect var. (mar is a factor)
soep$married <- soep$mar                        # copy of original variable
levels(soep$married)                            # inspect levels
# collapse levels 2 to 5 into one category and code "Refusal" as missing
levels(soep$married)[2:6] <- c(rep("Not married", 4), NA)      
table(soep$mar, soep$married, useNA = "always") # check

class(soep$mar)

                             ## IMPORTANT ##

##  MISTAKES IN CODING VARIABLES HAVE A MASSIVE IMPACT ON LATER RESULTS !!!    ##

##  ALWAYS compare the new variable to the original variable using table !!!   ## 
##  If there are too many variables or values to be tabulated, print a sample  ##
##  of about 100 observations and compare the values of all variables.         ##     

                             ## IMPORTANT ##


# factor variable identifying married/unmarried men/women (4 levels) 

#ifelse if else 
soep$marriedXgender <- NA
soep$marriedXgender <- ifelse(soep$married == "Married" & soep$gender == "Male", 
                              "Married male", soep$marriedXgender
                              )
soep$marriedXgender <- ifelse(soep$married == "Married" & soep$gender == "Female", 
                              "Married female", soep$marriedXgender
                              )
soep$marriedXgender <- ifelse(soep$married == "Not married" & soep$gender == "Male", 
                              "Not married male", soep$marriedXgender
                              )
soep$marriedXgender <- ifelse(soep$married == "Not married" & soep$gender == "Female", 
                              "Not married female", soep$marriedXgender
                              )
soep$marriedXgender <- factor(soep$marriedXgender) #Convert it from a character to a factor 
soep[1:100, c("married", "gender", "marriedXgender")]                # check 
soep[is.na(soep$married), c("married", "gender", "marriedXgender")]  # check missings

mode(soep$marriedXgender)
class(soep$marriedXgender)

## Caution: When specifying values in ifelse-conditions, misspelling the outcome 
## categories leads to NAs on the resulting variable. R will throw no error message!


# grouping a numeric variable into a factor

# break age into ten groups at equal intervals
soep$agegroups <- cut(soep$age, 10)
table(soep$age, soep$agegroups, useNA = "always")                     # check

# break age into groups of ten years each
soep$agegroups <- cut(soep$age, c(seq(10, 100, 10)))
table(soep$age, soep$agegroups, useNA = "always")                     # check

# break age into deciles
soep$agegroups <- cut(soep$age, 
                      quantile(soep$age, seq(0, 1, 0.1)),
                      include.lowest = TRUE
                      )
addmargins(table(soep$age, soep$agegroups, useNA = "always"))         # check

options(max.print = 10000)
quantile(soep$age, seq(0, 1, 0.1))

#The 10 groups are as equal in size as possible (Sum)

### Deleting things 

# deleting variables

# one variable
soep$hhinc_USD <- NULL

# several variables
soep[, c("pchinc", "pic")] <- data.frame(NULL)

# all but some variables
soep_reduced <- soep[, c("gender", "income", "emp", "lsat")]  


# deleting observations

# respondents with zero or missing income
soep_earners <- soep[soep$income > 0 & !is.na(soep$income), ]     # or...
soep_earners2 <- subset(soep, income > 0 & !is.na(income))

# respondents with missing values on income, lsat, gender or emp
soep_nonmis <- soep[!is.na(soep$income) & !is.na(soep$lsat) 
                    & !is.na(soep$gender) & !is.na(soep$emp), 
                    ]

# same result
soep_nonmis2 <- soep[complete.cases(soep[, c("income", "lsat", "gender", "emp")]), ]

# same result
soep_nonmis3 <- subset(soep, complete.cases(income, lsat, gender, emp))


# deleting objects

# single object
rm(soep_reduced)

# multiple objects
rm(soep_earners, soep_earners2, soep_nonmis, soep_nonmis2, soep_nonmis3)



### Sorting dataframes

# NEVER use sort() to sort a data frame !!

# sort all observations in increasing order of income
soep[order(soep$income), ]             

# sort all obs. in increasing order of lsat and decreasing order of income
soep[order(soep$lsat, -soep$income), ]




### Exercise 3

# 1. Create a factor called minor identifying respondents under age 18. The factor 
#    should have two levels: "adult" and "under age 18". Check whether age contains 
#    missing values and make sure they are retained. Verify that you've created the 
#    variable correctly.
#    
# 2. Take a look at the variable lsat. Create a factor from lsat with three levels:  
#    "dissatisfied" (from "completely dissatisfied" to "4"), "neither/nor" (equal to
#    "intermediate"), "satisfied" (from "6" to "completely satisfied"). Make sure all 
#    missing values on lsat are retained. Verify that you've created the variable 
#    correctly.

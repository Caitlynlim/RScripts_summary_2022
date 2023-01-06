#############################
# Data Analysis with R
# Michael Herrmann
# University of Konstanz
# Session 13
############################



### Reading and writing data sets


# Commonly used data formats in the social sciences:

# - Excel spreadsheets
# - SPSS data sets
# - Stata data sets
# - Text files (CSV): comma/semicolon separated values, 
#   tab/space delimited values


#  New Package for data input/output
#
#  As of June, 18, 2017, there is an all-in-one solution
#  for data input/output: the rio package
# 
#    install.packages("rio")
#    library(rio)
#    import("filename.extension")
#    export("filename.extension")
#
#  For a quick intro and list of supported formats: 
#  https://cran.r-project.org/web/packages/rio/README.html



rm(list = ls())

setwd("C:/Users/Michael_2/Documents/Datenanalyse R/")



## rio works fine with Excel and text/CSV files

library(rio)

# Excel
cnd09 <- import("Bewerber0913.xlsx", sheet = "Bewerber2009")
cnd13 <- import("Bewerber0913.xlsx", sheet = "Bewerber2013")

rm(cnd09, cnd13)


# text/CSV
cnd13 <- import("Bewerber2013.csv")

rm(cnd13)

# If some characters in the data set do not display  
# properly/are not recognized by R, you can tell import
# how the text is encoded, for example:

# cnd13 <- import("Bewerber2013.csv", encoding = "Latin-1")



## rio also works well with "metadata rich" formats such as SPSS or Stata

# Stata
soep <- import("data1.dta")

View(soep)
rm(soep)


# SPSS
afb <- import("afrobarR3.sav")

View(afb)


# Metadata rich formats allow for storing more information about a variable
# than just its name and values. For example, Stata and SPSS allow numeric 
# variables to have so-called labels: 
 
# 1. Variable labels are short descriptions of what a variable contains.  
#    R-Studio reports this information underneath a variable's name when
#    the data are displayed with its Viewer.
# 2. Value labels explain what the numeric values of a variable mean, e.g., 
#    0 "male" 1 "female". Value labels are stored in R but they are not
#    shown in R-Studio's Viewer.
 
# In general, R allows for storing additional information about an object 
# (e.g., a variable) as an attribute. All variables in afb as well as soep
# have attributes that contain variable labels and, if appropriate, value
# labels. You can see them if you click on the triangle next to a data set's
# name in the environment.


# To access the attributes of an object, use the attributes function:

attributes(afb$region)

# The result is a list of all attributes of the variable. You can access a
# specific attribute, such as the value labels, by subsetting the result:

attributes(afb$region)$labels

# or
attr(afb$region, "labels")


# Converting variables with labels into factors

# Since region is not really a numeric variable, we should convert it either
# to factor or character. The rio package provides functions for this:

afb$region <- factorize(afb$region)
afb$region <- characterize(afb$region)

# Both functions can also be used to convert all labelled variables in a data
# set:

afb_new <- factorize(afb)
afb_new <- characterize(afb)

# Unfortunately, changing an R object results in the loss of its attributes. 
# Hence the variable labels are now gone for all variables that were 
# converted to factor (or character).

View(afb_new)
rm(afb_new)



## Loading and converting data without loss of information

# 1. Using rio

# One way to reinstate the variable labels (or other attributes) is to 
# extract them from the original data frame and assign them to the new data 
# frame. 

# First, we load the original data set and create a factorized copy of it.

afb_orig <- import("afrobarR3.sav")
afb <- factorize(afb_orig)

# Next, we loop over the variables in the original and the new data frame 
# to extract the variable labels from the original data and use them to
# replace the variable labels in the new data.

for (i in 1:length(afb)) {
  attr(afb[, i], "label") <- attr(afb_orig[, i], "label")
}


# 2. Using haven 

# An easier way to achieve the same thing is via the haven package and its 
# built in function as_factor:

library(haven)

afb_new <- data.frame(as_factor(read_sav("afrobarR3.sav")))

# as_factor does the same thing as factorize, but unlike the former, it 
# preserves all other attributes. On the downside, as_factor does not work
# on regular data frames; instead it requires a tibble - a particular data
# object designed by Hadley Wickham. The function read_spss returns such a
# tibble; rio, by contrast, returns a regular data frame. To get a regular
# data frame after converting all variables with a "labels" attribute to
# factors, we simply use the data.frame() function. 

rm(afb, afb_new, afb_orig)


## Additional complications: unused outcome categories

# 1. The problem

# Sometimes the value labels of variables define more outcome categories 
# than we actually observe in the data. This is the case with the soep data 
# set:

soep <- import("data1.dta")
attr(soep$sex, "labels")
table(soep$sex)

# Here gender has many more value labels than necessary. This creates 
# problems with factorize:

soep$gender <- factorize(soep$sex)


# 2. The solution

# In these cases, it's best to use the functions from the haven package:

soep <- read_dta("data1.dta")
soep$sex <- as_factor(soep$sex)

attributes(soep$sex)
table(soep$sex)

# Now sex is a factor with 8 levels. Usually, we are not interested in unused
# levels. The function droplevels from base R allows us to get rid of all 
# levels that do not occur in practice:

soep$sex <- droplevels(soep$sex)

table(soep$sex, useNA = "always")
attributes(soep$sex)

# Now the unused levels are gone but, unfortunately, so is the variable 
# label. One way to prevent droplevels from killing all attributes is to move  
# them to the data-frame level before using droplevels and to move them back 
# to the variable level afterwards. Moving attributes to the data-frame level
# and back is achieved via the gather_attrs and spread_attrs functions from 
# the rio package. For example, consider education:

attributes(soep$edu)
table(soep$edu, useNA = "always") 

soep$edu <- as_factor(soep$edu)
table(soep$edu, useNA = "always")

library(rio)
soep <- gather_attrs(soep)
soep$edu <- droplevels(soep$edu)
soep <- spread_attrs(soep)

# now the variable label "Education" is preserved
table(soep$edu, useNA = "always") 
attributes(soep$edu)


# We can use as_factor and droplevels on the entire data set and thus apply 
# the above steps to all labelled variables in one go:

soep <- data.frame(as_factor(read_dta("data1.dta")))
soep <- spread_attrs(droplevels(gather_attrs(soep)))

# inspection of values
lapply(soep[, c("sex", "edu", "mar")], table, useNA = "always")




## Saving/writing data sets

library(rio)

# save in Stata format
export(afb, "afrobarR3.dta")

# save in SPSS format
export(afb, "afrobarR3.sav")

# save as CSV file
export(afb, "afrobarR3.csv")


# save as R data set
save(afb, file = "afrobarR3.rda")

rm(afb)

# retrieve an R data set
load("afrobar3.rda")





#starting out
rm(list = ls())
setwd(" ")

#read files 
library(rio)                                         
library(haven) #allows you to read stata format with a read_dta into R
soep <- data.frame(as_factor(read_dta("data1.dta"))) 
soep <- spread_attrs(droplevels(gather_attrs(soep))) 

soep <- read.csv(" .csv")
read.table(" .txt")


#Arim
?seq()
seq(from = , to =, by = , length.out = , along.with =  )

#+ - * / < > | != & 
  
#summary 
str(soep)
summary(soep)
head(soep)

#Access content
soep[, 1]                     # first column/variable (respondent ID)
soep[10, ]                    # tenth row

soep[1:10, "state"] 
soep$state 

#Frequency tables
table(soep$emp, useNA = "always")
prop.table(table(soep$emp, useNA = "always"))

table(soep$gender, soep$emp, useNA = "always") #employment status by gender

# frequencies as (row) proportions
prop.table(table(soep$gender, soep$emp, useNA = "always"), 1)

mean(soep$income, na.rm = TRUE)

#graph

# distribution of income (for non-zero incomes below 200,000 EUR)
hist(soep$income[soep$income > 0 & soep$income < 2e+05], 
     col = "lightblue", 
     breaks = 100
)

# income by occupation (for non-zero incomes below 200,000 EUR)
boxplot(soep$income ~ soep$emp, 
        subset = soep$income > 0 & soep$income < 2e+05
)

#regression
# regression of income on gender and employment status
lm(income ~ sex + emp, data = soep)

# restrict regression analysis to respondents with positive income
summary(lm(income ~ sex + emp, data = soep, subset = income > 0))  

#conversion to other mode
as.numeric()
as.character()
as.logical()
























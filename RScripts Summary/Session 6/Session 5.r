#############################
# Data Analysis with R
# Michael Herrmann
# University of Konstanz
# Session 5
############################


# set working directory
setwd("/Users/CaitlynLim/Documents/RScripts/Session 5")

# run R script from last week
source("Session4.R", echo = TRUE)



# How do we explain the effect of body height on income? 

# Women have lower earnings than men and they are shorter, on average.
# The effect of body height might thus be spurious.



## Data preparation

# Factor variable for gender

is.factor(issp$v151)                            # v151 is a factor
table(issp$v151, useNA ="always")               # inspect values: no missings
issp$gender <- issp$v151                        # make a copy of v151
levels(issp$gender) <- c("male", "female")      # assign English labels (Rename the labels)
table(issp$v151, issp$gender, useNA = "always") # labels are correctly assigned



### Data analysis / Interpretation / Displaying results

# Are women shorter than men, on average?  

# graphical answer
plot(height ~ gender, issp, boxwex = 0.1)

# numerical answer
summary(issp$height[issp$gender == "male"])
summary(issp$height[issp$gender == "female"])  # Yes!

# storing average body height of men and women for later use
hgt_mf <- c(mean(issp$height[issp$gender == "male"], na.rm = TRUE), 
            mean(issp$height[issp$gender == "female"], na.rm = TRUE)
            )


# Do women make less money than men, on average?  

# graphical answer
plot(income ~ gender, issp, boxwex = 0.1)

# numerical answer
summary(issp$income[issp$gender == "male"])
summary(issp$income[issp$gender == "female"])  # Yes!

# storing average income of men and women for later use
inc_mf <- c(mean(issp$income[issp$gender == "male"], na.rm = TRUE), 
            mean(issp$income[issp$gender == "female"], na.rm = TRUE)
            )
inc_mf

# Does the gender wage gap explain the effect of height on income?

# Graphical analysis: income vs. height for male and female respondents

# using letters as markers for gender:
plot(income ~ height, issp, pch = c("m", "f")[gender], cex = 0.7)  

c("m", "f")[1]
c("m", "f")[2]

levels(issp$gender)

table(issp$gender, as.numeric(issp$gender)) #shows the underlying value 

# colors instead of letters:
plot(income ~ height, issp, col = c("blue", "hotpink")[gender])  

abline(h = inc_mf[1], col = "blue")        # avg. male income
abline(v = hgt_mf[1], col = "blue")        # avg. male height
abline(h = inc_mf[2], col = "hotpink")     # avg. female income
abline(v = hgt_mf[2], col = "hotpink")     # avg. female height
abline(lm(income ~ height, issp))          # regression line


#the points where the 2 lines cross is the avg income of an avg height of female/male

#' Interpretation
#' 
#' From this graph we see that the regression of income on body height closely 
#' reproduces the gender gap in income. The predicted income for an 
#' average-sized female is close to the average income of all females and the  
#' predicted income for an average-sized male is close to the average income of 
#' all males. Thus, the relationship we found between income and body height 
#' might simply reflect gender differences on both variables. To see if the
#' relationship between income and height is spurious, we control for gender.  



## Multiple Regression

model3 <- lm(income ~ height + gender, issp)
summary(model3)  #regression table

#' Interpretation
#' 
#' Holding gender constant, each additional cm of body height is associated 
#' with an increase in net income of 19 Euros, on average.
#' 
#' Holding body height constant, women make 328 Euros less, on average, than 
#' men do. 

# Comparing the effect of body height before and after controlling for gender...
coef(model1)
coef(model3)
# ...the effect shrinks by about 40 percent when gender is taken into account.


# Visual display of results

# Make sure the file ceplot.R is in your working directory!

# run R script for creating conditional effect plots 
source("ceplot.R")          

# conditional effect plots of height and gender
par(mfrow = c(1, 2))
ce.plot(model3, height, ylim = c(500, 2500))
ce.plot(model3, gender, ylim = c(500, 2500))
par(mfrow = c(1, 1))

#the gender difference, and the body height difference so shows that height may
#still be signficant, even after we control for gender

## Interactions

# Does the effect of body height on income differ between men and women?

# In the above model, we forced the effect of height to be the same for men   
# and women. But perhaps the relationship is driven by only one group. We can 
# test whether the effect of body height differs between men and women by 
# interacting height and gender.																		

# main effects plus interaction effect
summary(lm(income ~ height + gender + height:gender, issp))  

#female benefit 15+7, women get more for each cm

# same as above but less typing
summary(lm(income ~ height * gender, issp))

# effect of height separated by gender
summary(lm(income ~ gender + height:gender, issp))


#' Interpretation
#' 
#' For each additional cm of body height, income increases by 15 Euros among 
#' men and 23 Euros among women, on average. The difference is, however, not 
#' significantly different from zero at the 5 percent level. 		
#'
#' Note
#' 
#' Both models estimated above are mathematically and substantively equivalent. 
#' The first version allows for testing whether the effect of height differs 
#' significantly between men and women; the second version is easier to 
#' interpret as it shows the effect of height separately for men and women. 

# storing regression results 
model4 <- lm(income ~ height * gender, issp)


# Visualizing the interaction effect

# scatter plot of income and height
plot(income ~ height, issp, col = c("blue", "hotpink")[gender])
# including regression line for women
abline(lm(income ~ height, issp, gender == "female"), col = "hotpink")  
# including regression line for men
abline(lm(income ~ height, issp, gender == "male"), col = "blue")

# To make the graph more compelling, we zoom in by excluding very  
# large incomes and we assign more informative axis titles:
plot(income ~ height, 
     subset(issp, income < 4000), 
     col = c("blue", "hotpink")[gender], 
     ylab = "Monthly net income (Euro)", 
     xlab = "Body height (cm)"
     )
abline(lm(income ~ height, issp, gender == "female"), col = "hotpink")
abline(lm(income ~ height, issp, gender == "male"), col = "blue")


# conditional effect plots
par(mfrow = c(1, 2))
ce.plot(model4, 
        height, 
        fixvar = "gender", 
        fixval = "male", 
        ylim = c(400, 2500)
        )
ce.plot(model4, 
        height, 
        fixvar = "gender", 
        fixval = "female", 
        ylim = c(400, 2500)
        )
par(mfrow = c(1, 1))

#overlapping confidence intervAL implies that it's not so good 

## Exercise 5

# In Exercise 4 you showed that later birth cohorts grow taller than earlier 
# ones. This exercise asks you to control for gender.

# 1. Generate the variable ybirth (year of birth) using variable v153. Find 
#    out how missing values are coded on v153 and set ybirth to NA when v153 
#    is missing.
# 2. Run a linear regression of body height (dependent variable) on year of 
#    birth, controlling for gender. (Use the variables height and gender from 
#    Session 5.)
# 3. Interpret the effects of birthyear and gender in words.
# 4. Run a linear regression of body height on the interaction of gender and 
#    birthyear. (regression: dependent on the independent,
#                 effect of the independent on the dependent)
# 5. Produce a scatter plot of body height (y-axis) vs. birthyear (x-axis) and 
#    visually distinguish between men and women. Show the interaction of 
#    birthyear and gender by superimposing SEPARATE regression lines for men 
#    and women (you do not need to fine-tune the plot further for presentation).





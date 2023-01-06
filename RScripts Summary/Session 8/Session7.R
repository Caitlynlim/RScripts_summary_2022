#############################
# Data Analysis with R
# Michael Herrmann
# University of Konstanz
# Session 7
############################


#' Are taller people more likely to vote?

#' In this exemplary analysis we shall consider a binary 
#' regression analysis. Results from binary regression are 
#' more difficult to understand and communicate than those 
#' of linear regression. The same holds for other nonlinear 
#' regression models. Hence our main focus lies on  
#' techniques for calculating quantities of interest, such
#' as predicted values and effect sizes, based on 
#' regression results:

#' 1. Preparing data for analysis
#' 2. Regression analysis
#' 3. Interpreting results
#' 4. Computing predictions and effect sizes
#' 5. Displaying and exporting results  					



# remove everything from the environment

rm(list = ls())


# set working directory

setwd("/Users/CaitlynLim/Documents/RScripts/Session 7")


# import data set

library(haven)
library(rio)
issp <- data.frame(as_factor(read_dta("data3.dta")))
issp <- spread_attrs(droplevels(gather_attrs(issp))) 

# show data set
View(issp)



### Data preparation 

# restrict sample to German citizens

class(issp$v4)
table(issp$v4, useNA = "always")

issp <- issp[as.numeric(issp$v4) <= 2, ] 

table(issp$v4, useNA = "always")


# Turnout

class(issp$v535)
table(issp$v535, useNA = "always")

issp$turnout <- issp$v535
levels(issp$turnout) <- c("Yes", "No", NA) #assign new levels

issp$turnout <- relevel(issp$turnout, "No")       # change base level: 'No' first

table(issp$v535, issp$turnout, useNA = "always")  # check coding



# Body height

class(issp$v629)
table(issp$v629, useNA = "always")

issp$height <- issp$v629
issp$height[issp$height == "WEISS NICHT"] <- NA
issp$height <- as.numeric(as.character(issp$height))

issp[1:100, c("v629", "height")]


# Body weight

class(issp$v631)
table(issp$v631, useNA = "always")

issp$weight <- issp$v631
issp$weight[issp$weight == "WEISS NICHT"] <- NA
issp$weight <- as.numeric(as.character(issp$weight))

issp[1:100, c("v631", "weight")]


# Gender

class(issp$v151)
table(issp$v151, useNA ="always")

issp$gender <- issp$v151      
levels(issp$gender) <- c("male", "female") 

table(issp$v151, issp$gender, useNA = "always")


# Income

class(issp$v388)
table(issp$v388, useNA = "always")

issp$income <- issp$v388
issp$income[issp$income == "KEIN EINKOMMEN"
            | issp$income == "VERWEIGERT" 
            | issp$income == "KEINE ANGABE"
            ] <- NA
issp$income <- as.numeric(as.character(issp$income))

issp[1:100, c("v388", "income")]


# Age

class(issp$v154)
table(issp$v154, useNA = "always")

issp$age <- issp$v154
issp$age[issp$age == "KEINE ANGABE"] <- NA
issp$age <- as.numeric(as.character(issp$age))

issp[400:500, c("v154", "age")]


# Region

class(issp$v3)
table(issp$v3, useNA = "always")

issp$region <- issp$v3
levels(issp$region) <- c("west", "east")

table(issp$v3, issp$region, useNA = "always")


# Educational background (self, father, and mother)

table(issp$v173, as.numeric(issp$v173), useNA = "always")
table(issp$v374, as.numeric(issp$v374), useNA = "always")
table(issp$v375, as.numeric(issp$v375), useNA = "always")

table(issp$v182, as.numeric(issp$v182), useNA = "always")
table(issp$v183, as.numeric(issp$v183), useNA = "always")

table(issp$v376, as.numeric(issp$v376), useNA = "always")
table(issp$v377, as.numeric(issp$v377), useNA = "always")

# set levels for target variables
edulevels <- c("none", 
               "primary", 
               "secondary", 
               "tertiary (tech.)", 
               "tertiary", 
               "tech. university", 
               "university"
               )

# own education
issp$educ <- as.numeric(issp$v173) 
issp$educ <- ifelse(issp$educ >= 6, NA, issp$educ)
issp$educ <- ifelse(issp$v182 == "GENANNT" 
                    & issp$v183 != "GENANNT", 
                    6, 
                    issp$educ
                    )
issp$educ <- ifelse(issp$v183 == "GENANNT", 7, issp$educ)
issp$educ <- factor(issp$educ, labels = edulevels)

issp[1:200, c("v173", "v182", "v183", "educ")]
table(issp$v173, issp$educ, useNA = "always")
table(issp$v182, issp$educ, useNA = "always")
table(issp$v183, issp$educ, useNA = "always") 

# father's education
issp$feduc <- as.numeric(issp$v374) - 1
issp$feduc <- ifelse(issp$feduc == 0 | issp$feduc >= 6, 
                     NA, 
                     issp$feduc
                     )
issp$feduc <- ifelse(issp$v376 == "FACHHOCHSCHULABSCHL.", 
                     6, 
                     issp$feduc
                     )
issp$feduc <- ifelse(issp$v376 == "HOCHSCHULABSCHLUSS", 
                     7, 
                     issp$feduc
                     )
issp$feduc <- factor(issp$feduc, labels = edulevels)

issp[1:200, c("v374", "v376", "feduc")]

# mother's education
issp$meduc <- as.numeric(issp$v375)
issp$meduc <- ifelse(issp$meduc >= 6, NA, issp$meduc)
issp$meduc <- ifelse(issp$v377 == "FACHHOCHSCHULABSCHL.", 
                     6, 
                     issp$meduc
                     )
issp$meduc <- ifelse(issp$v377 == "HOCHSCHULABSCHLUSS", 
                     7, 
                     issp$meduc
                     )
issp$meduc <- factor(issp$meduc, labels = edulevels)

issp[1:200, c("v375", "v377", "meduc")]




### Logistic regression


## Effect of body height

#logit function makes it a probability, ensuring it's between 0 and 1
#Pr(vote) = logit^-1(a + bheight)
# --> 1/ (z + e^-(a+bheight))  --> Less than 1 for sure
#The higher the b( effects onn height) --> it gets more S like
#inifinity --> Step function
#Increasing alpha shifts the function left

model1 <- glm(turnout ~ height, family = binomial(link = "logit"), issp)
summary(model1)

#chnage in the logarithm of an odds of turnout, decreases by the 0.002
#0.791 = statiscally inefficient 

#Pr(vote) = logit^-1 (2.017-0.002height)



# visual inspection
plot(jitter(as.numeric(turnout) - 1, 0.1) ~ jitter(height, 2), issp)

x <- seq(min(issp$height, na.rm = TRUE), max(issp$height, na.rm = TRUE), 1)
x #min and max height

pr <- predict(model1, data.frame(height = x), type = "response")

#data.frame -- predict only accepts a data frame not a vector x, also it has
#to be height

lines(x, pr)


## Effect of age

model_age <- glm(turnout ~ age, family = binomial(link = "logit"), issp)
summary(model_age)

# visual inspection

#jitter makes it loose, so it doesn't overlap
plot(jitter(as.numeric(turnout) - 1, 0.1) ~ jitter(age, 2), issp)

x <- seq(min(issp$age, na.rm = TRUE), max(issp$age, na.rm = TRUE), 1)
pr <- predict(model_age, data.frame(age = x), type = "response")

lines(x, pr)

#' Interpretation
#' 
#' The probability of turnout increases with age. Due to the functional
#' form of the model, the relationship is nonlinear. From the graph, we  
#' can see that the effect is stronger in early years. For example:
#' 
#' Increasing age from 18 to 40 years is associated with an increase of
#' almost 20 percentage points, on average, in the probability of turnout.
#' 
#' Increasing age from 40 to 60 years is associated with an increase of
#' about 10 percentage points, on average, in the probability of turnout. 


#' Odds Ratios
#'  
#' In logistic regression (but not in probit regression) we can also 
#' interpet the change in the odds of turnout. To obtain the change in the 
#' odds of y for a one-unit change in x, we must exponentiate the 
#' coefficient of x:

exp(coef(model_age))

#' Interpretation
#' 
#' For each additional year of age, the odds that a person turns out to 
#' vote increase by 4 percent.											
#'
#' Note that, unlike in linear regression, the change in the odds is 
#' MULTIPLICATIVE. This means that for a one-unit change in x, the odds 
#' of y do not change by a fixed amount but by a fixed factor. Here, the 
#' odds of turnout increase by a factor of 1.04 for each additional year 
#' of age. This is equivalent to saying that the odds increases by 4 
#' percent (not 4 percentage points!).	



## Effect of body height controlling for age (Age affects both)

model2 <- glm(turnout ~ height + age, family = binomial(link = "logit"), issp)
summary(model2)      

#' Interpretation
#' 
#' Body height does have an effect on turnout once we control for age
#' In other words, holding age constant, the probability of turnout
#' increases with body height.
#' 
#' The reason why the effect of body height changes once we control for age 
#' is that higher age is associated with higher turnout but also with lower 
#' body height (e.g., because younger generations grow taller than their 
#' predecessors):

plot(jitter(height, 2) ~ jitter(age, 2), issp)
abline(lm(height ~ age, issp))


#' Odds ratio interpretation:

exp(coef(model2))

#' For each additional cm of body height, the odds of turnout increase
#' by 2 percent, on average, holding age constant.						
#' 
#' Likewise, for each additional year of age, the odds of turnout 
#' increase by 4.5 percent, on average, holding body height constant.



### Average predicted probabilities  

#' The odds ratio tells us little about the change in the probablity of
#' turnout. For example, what is the effect of an increase in body height
#' by 10 cm on the probablity of turnout? 
#' 
#' To answer this question, we want to create a plot that shows the
#' AVERAGE PREDICTED PROBABILITY of turnout for given values of body 
#' height, while holding all other predictors in the model at their 
#' observed values.
#' 
#' To cite the method (and to learn more about it):
#'  
#' Gelman, A., & Pardoe, I. (2007). Average predictive comparisons for 
#' models with nonlinearity, interactions, and variance components. 
#' Sociological Methodology, 37(1), 23-51.
#'  
#' Hanmer, M. J., & Ozan Kalkan, K. (2013). Behind the curve: Clarifying 
#' the best approach to calculating predicted probabilities and marginal 
#' effects from limited dependent variable models. American Journal of 
#' Political Science, 57(1), 263-277.


## Avg. pred. prob. of turnout over values of body height

# get the data used for estimating the model
mf <- model.frame(model2)

# create values of the predictor variable to make the predictions
x <- seq(min(mf$height), max(mf$height), 1)

# create a vector to store the avg. pred. probabilities of turnout 
avgpr <- numeric(length(x))


#' The following loop does four things: 
#' 
#' 1) set body height to a specific value for all cases in mf 
#' 2) predict the probability of turnout for each case in mf, given 
#'    body height, while holding all other variables (i.e., age in this
#'    example) at their observed values
#' 3) compute the average probability of turnout across all cases
#' 4) repeat!

for (i in 1:length(x)) {
  mf$height <- rep(x[i], nrow(mf))
  avgpr[i] <- mean(predict(model2, mf, type = "response"))
}

# plotting the predictions
plot(x, 
     avgpr, 
     type = "l", #line
     lwd = 1.5, 
     ylim = c(0.5, 1), 
     ylab = "Avg. Pred. Prob. of Turnout", 
     xlab = "Body Height (cm)"
     )

#' Instead of drawing a plot, we can also list the avg. pred. probabilities 
#' of turnout for each value of body height:

data.frame(x, avgpr)

#' Effect of body height on the probability of turnout:
#' 
#' Increasing body height from 160 cm to 170 cm is associated with an 
#' increase in the average probability of turnout of about 3 percentage 
#' points (i.e. the average probability of turnout increases from 81 
#' to 83.7 percent), holding all other variables at their observed values.


## Avg. pred. prob. of turnout over values of age

# get the data used for estimating the model
mf <- model.frame(model2)

# create values of the predictor variable to make the predictions
x <- seq(min(mf$age), max(mf$age), 1)

# create a vector to store the avg. pred. probabilities of turnout 
avgpr <- numeric(length(x))

# avg. pred. probabilities for different values of age, 
# holding all other variables at their observed values
for (i in 1:length(x)) {
  mf$age <- rep(x[i], nrow(mf))
  avgpr[i] <- mean(predict(model2, mf, type = "response"))
}

# plotting the predictions
plot(x, 
     avgpr, 
     type = "l", 
     lwd = 1.5, 
     ylim = c(0.5, 1), 
     ylab = "Avg. Pred. Prob. of Turnout", 
     xlab = "Age"
     )

# list the avg. pred. probability of turnout for each value of age
data.frame(x, avgpr)

#' Effect of age on the probability of turnout:
#' 
#' Increasing age from 20 to 30 years is associated with an increase 
#' in the average probability of turnout of about 10 percentage points
#' (i.e. the average probability of turnout increases from 62 percent 
#' to 72 percent), holding all other variables at their observed values.															 



# Combining the two graphs allows us to compare effect sizes: 

par(mfrow = c(1, 2))

mf <- model.frame(model2)
x <- seq(min(mf$height), max(mf$height), 1)
avgpr <- numeric(length(x))
for (i in 1:length(x)) {
  mf$height <- rep(x[i], nrow(mf))
  avgpr[i] <- mean(predict(model2, mf, type = "response"))
}
plot(x, 
     avgpr, 
     type = "l", 
     lwd = 1.5, 
     ylim = c(0.5, 1), 
     ylab = "Avg. Pred. Prob. of Turnout", 
     xlab = "Body Height (cm)"
     )

mf <- model.frame(model2)
x <- seq(min(mf$age), max(mf$age), 1)
avgpr <- numeric(length(x))
for (i in 1:length(x)) {
  mf$age <- rep(x[i], nrow(mf))
  avgpr[i] <- mean(predict(model2, mf, type = "response"))
}
plot(x, 
     avgpr, 
     type = "l", 
     lwd = 1.5, 
     ylim = c(0.5, 1), 
     ylab = "", 
     xlab = "Age"
     )

par(mfrow = c(1, 1))  








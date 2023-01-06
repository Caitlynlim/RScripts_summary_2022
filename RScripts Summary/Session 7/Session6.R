#############################
# Data Analysis with R
# Michael Herrmann
# University of Konstanz
# Session 6
############################



# set working directory
setwd("/Users/CaitlynLim/Documents/RScripts/Session 6")

# run R scripts from previous sessions
source("Session 5.R", echo = TRUE)

#Today: Data preparation, Categorical predictors with more than 2 values

## Multiple regression 

## Controlling for other variables that might correlate with income and body height


# Body weight (related to body height and possibly to income as well)

class(issp$v631)                                      # body height is a factor
table(issp$v631, useNA = "always")

issp$weight <- issp$v631                              # create height variable
issp$weight[issp$weight == "WEISS NICHT"] <- NA       # code missings as NA
issp$weight <- as.numeric(as.character(issp$weight))  # convert factor to numbers

issp[1:100, c("v631", "weight")]                      # check coding


# Regression

summary(lm(income ~ height + gender + weight, issp))

#to find last week's data could use coef(model3)


#Taller people weigh more, positive relationship

#' Interpretation
#' 
#' Holding constant gender and body weight, each additional cm of body height
#' is associated with a 24 Euro increase in monthly net earnings, on average.  	
#' 
#' Holding constant gender and body height, each additional kg of body weight
#' is associated with a 7 Euro decrease in monthly net earnings, on average.	
#'
#' Interpret the effect of gender!



# Context as a potential confounder 

# 1. Regional differences

# People who grew up in East Germany (after 1948) have lower earnings today
# and, due to poorer economic circumstances, they might be shorter.

class(issp$v161)
class(issp$v153)
table(issp$v161, as.numeric(issp$v161), useNA = "always")
table(issp$v153, useNA = "always")

# Indicator for people who grew up in the German Democratic Republic

#recode it you use their underlying values

# birthyear
issp$ybirth <- issp$v153
issp$ybirth[issp$ybirth == "KEINE ANGABE"] <- NA
issp$ybirth <- as.numeric(as.character(issp$ybirth))
issp[400:500, c("v153", "ybirth")]

# born in the GDR (Born in the area and after 1949)
issp$born <- ifelse(as.numeric(issp$v161) >= 12 #on the state avariable, have to convert to  
                    & as.numeric(issp$v161) <= 17 #numeric to use the value
                    & issp$ybirth >= 1949  #born after 1949
                    & !is.na(issp$ybirth), 
                    1, 
                    0
                    ) #If the condition holds we code as 1, otherwise 0 

issp$born <- factor(issp$born, labels = c("not GDR", "GDR"))

# check coding
table(issp$v161[issp$ybirth >= 1949], issp$born[issp$ybirth >= 1949], useNA = "always")
table(issp$v161[issp$ybirth < 1949], issp$born[issp$ybirth < 1949], useNA = "always")


# People who grew up in West Germany have higher incomes...
summary(issp$income[issp$born == "not GDR" & issp$ybirth >= 1949 & !is.na(issp$ybirth)])
summary(issp$income[issp$born == "GDR" & issp$ybirth >= 1949 & !is.na(issp$ybirth)])

# ...but they are only slightly taller than those who grew up in the East.
summary(issp$height[issp$born == "not GDR" & issp$ybirth >= 1949 & !is.na(issp$ybirth)])
summary(issp$height[issp$born == "GDR" & issp$ybirth >= 1949 & !is.na(issp$ybirth)])

#only half a cm differences not much
#Could test if it's satistically significant

t.test(height ~ born, issp, issp$ybirth >= 1949 & !is.na(issp$ybirth))

#no it's not large enough to qualify for 

# The difference in body height between those who grew up in the GDR and
# those who didn't is not statistically significant.



# 2. Family background 

# People raised in poorer families might be shorter and also have lower 
# earnings. We will approximate family background/upbringing through
# parent's education, as well as respondent education.

# We wish to create a factor variable with levels running from 
# "no degree" to "university degree". We wish to create a variable
# for the respondent, the respondent's mother, and father.


# How are education variables (all factors) coded?
table(issp$v173, as.numeric(issp$v173), useNA = "always")    # own level of schooling
table(issp$v374, as.numeric(issp$v374), useNA = "always")    # father's level of schooling
table(issp$v375, as.numeric(issp$v375), useNA = "always")    # mother's level of schooling

table(issp$v182, as.numeric(issp$v182), useNA = "always")    # tech. university degree?
table(issp$v183, as.numeric(issp$v183), useNA = "always")    # university degree?

table(issp$v376, as.numeric(issp$v376), useNA = "always")    # father's level of training 
table(issp$v377, as.numeric(issp$v377), useNA = "always")    # mother's level of training


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
issp$educ <- as.numeric(issp$v173)                           # factor to numeric 
issp$educ <- ifelse(issp$educ >= 6, NA, issp$educ)           # 'other', 'in school', nonresponse
issp$educ <- ifelse(issp$v182 == "GENANNT" 
                    & issp$v183 != "GENANNT", 
                    6, 
                    issp$educ
                    )                                        # tech. university degree
issp$educ <- ifelse(issp$v183 == "GENANNT", 7, issp$educ)    # university degree
issp$educ <- factor(issp$educ, labels = edulevels)           # convert to factor

issp[1:200, c("v173", "v182", "v183", "educ")]               # check
table(issp$v173, issp$educ, useNA = "always")                # check 
table(issp$v182, issp$educ, useNA = "always")                # check 
table(issp$v183, issp$educ, useNA = "always")                # check 


# father's education
issp$feduc <- as.numeric(issp$v374) - 1                      # - 1 ('father unknown')
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

issp[1:200, c("v374", "v376", "feduc")]                      # check
table(issp$v374, issp$feduc, useNA = "always")               # check 
table(issp$v376, issp$feduc, useNA = "always")               # check 


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

issp[1:200, c("v375", "v377", "meduc")]                      # check
table(issp$v375, issp$meduc, useNA = "always")               # check 
table(issp$v377, issp$meduc, useNA = "always")               # check 



## Age

class(issp$v154)
table(issp$v154, useNA = "always")

issp$age <- issp$v154
issp$age[issp$age == "KEINE ANGABE"] <- NA
issp$age <- as.numeric(as.character(issp$age))

issp[400:500, c("v154", "age")]




### Full regression model

model5 <- lm(income ~ height 
             + gender 
             + weight 
             + age 
             + I(age^2) 
             + educ 
             + feduc 
             + meduc 
             + born, 
             issp
             )
summary(model5)


#' Interpretation
#' 
#' Interviewees with primary-level education do not differ (substantively or    
#' statistically) in their monthly earnings from those with no educational degree.
#' 
#' Interviewees with tertiary-level education in a technical field earn, on 
#' average, 182 Euros more than those with no educational degree. However, the
#' effect is not statistically significant at the 5 percent level.
#' 
#' Interviewees with a university degree earn, on average, 1010 Euros more than 
#' those with no educational degree. The difference is statistically significant 
#' at the 5 percent level.

# changing the base category for education variables to "primary":
summary(lm(income ~ height 
           + gender 
           + weight 
           + age 
           + I(age^2) 
           + relevel(educ, "primary") 
           + relevel(feduc, "primary") 
           + relevel(meduc, "primary") 
           + born, 
           issp
           )
        )


#' Interpretation
#' 
#' Interviewees whose father holds a university degree earn, on average,
#' 319 Euros more than interviewees whose father only finished primary school.	
#' 
#' Note
#' 
#' Both models are equivalent. They merely differ in the choice of base category, 
#' i.e., the level of eduaction to which the other levels are compared in terms 
#' of income.



## Visual display of the effect of education

par(mfrow = c(1, 3))
ce.plot(model5, educ, xlab = "Education", ylim = c(350, 2400))
ce.plot(model5, feduc, xlab = "Father's Education", ylim = c(350, 2400))
ce.plot(model5, meduc, xlab = "Mother's Education", ylim = c(350, 2400))
par(mfrow = c(1, 1))

# Saving the above graph as a pdf file

# Open a connection to a pdf file
pdf("education.pdf", width = 9, height = 3)

#   This creates a pdf file and sets the size of the graphics region (in inches). 
#   Every plot we subsequently produce is written directly into this file until
#   we close the connection.

par(mfrow = c(1, 3))
ce.plot(model5, educ, xlab = "Education", ylim = c(350, 2400))
ce.plot(model5, feduc, xlab = "Father's Education", ylim = c(350, 2400))
ce.plot(model5, meduc, xlab = "Mother's Education", ylim = c(350, 2400))
par(mfrow = c(1, 1))

# Close connection to pdf file
dev.off()     
# If output does not say "null device", repeat this command until it does



## Testing for overall significance of father's education

library(lmtest)      # this requires that package lmtest is installed

# H0: all coefficients of father's education are equal to zero
# H1: at least one coefficient is not equal to zero

# fit a model without father's education 
reduced <- lm(income ~ height 
              + gender 
              + weight 
              + age 
              + I(age^2) 
              + educ 
              + meduc 
              + born, 
              data = model.frame(model5)
              )

# Note that the reduced model must be estimated on the same sample as the full 
# model. This can be achieved by restricting the sample to those observations 
# that were used in the estimation of model 5 (i.e., the "model.frame" of model 5).

# F-test of H0
waldtest(model5, reduced)

# The difference in model fit is statistically significant at the 5 percent 
# level. We can reject the null hypothesis that all coefficients are equal to 
# zero and conclude that father's education has a significant effect on income.




### Preparing results for presentation

# Table with results from previous regressions

stargazer(model1, 
          model2, 
          model3, 
          model4, 
          model5,
          out = "table.html", 
          digits = 2,
          dep.var.labels = "Monthly net income (Euros)",
          covariate.labels = c("Body height (cm)",
                              "Body height squared",
                              "Male",
                              "Male*height",
                              "Body weight (kg)",
                              "Age (years)",
                              "Age squared",
                              "Ed. (primary)",
                              "Ed. (secondary)",
                              "Ed. (tertiary tech.)",
                              "Ed. (tertiary)",
                              "Ed. (university tech.)",
                              "Ed. (university)",
                              "Father's ed. (primary)",
                              "Father's ed. (secondary)",
                              "Father's ed. (tertiary tech.)",
                              "Father's ed. (tertiary)",
                              "Father's ed. (university tech.)",
                              "Father's ed. (university)",
                              "Mother's ed. (primary)",
                              "Mother's ed. (secondary)",
                              "Mother's ed. (tertiary tech.)",
                              "Mother's ed. (tertiary)",
                              "Mother's ed. (university tech.)",
                              "Mother's ed. (university)",
                              "East Germany"
                             )
          )
          



## Exercise 6

# Consider your analysis of the effects of birth year and gender (independent 
# variables) on body height (dependent variable) from Exercise 5.

# 1. Include mother's education as a control variable for upbringing in the  
#    regression. Create the variable in exactly the same way that we've done 
#    in Session 6.
# 2. Change the base category for mother's education to "secondary".
# 3. Test whether mother's education has a significant effect on body height 
#    (i.e., test the null hypothesis that all coefficients of mother's 
#    education are zero). 



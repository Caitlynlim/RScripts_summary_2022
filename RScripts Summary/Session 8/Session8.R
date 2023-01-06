#############################
# Data Analysis with R
# Michael Herrmann
# University of Konstanz
# Session 8
############################


# remove everything
rm(list = ls())   

# set working directory
setwd("/Users/CaitlynLim/Documents/RScripts/Session 8")

# run R script from previous session
source("Session7.R", print.eval = TRUE)



### Adding further control variables

model3 <- glm(turnout ~ height 
              + weight 
              + age 
              + gender, 
              family = binomial(link = "logit"), 
              issp
              )
summary(model3)  

#If women, will turnout more
#Height doubled, not actually doubled because it's a log scale but still significant

coef(model2)

#' Interpretation
#' 
#' Controlling for age, body height, and body weight, men are less 
#' likely to turn out to vote. The effect is not statistically 
#' significant at the 5 percent level.


# Computing the average effect of gender:

# get data used for estimating the model
mf <- model.frame(model3)

# create values of the predictor variable to make the predictions #gl = generate levels
#(another way to create a factor vaariable with two observations(because gender), only occuring once)
x <- gl(nlevels(mf$gender), 1, labels = levels(mf$gender))
# the 1 means only once, don't replicate
#nlevel just extract number of levels 

# create a vector to store the avg. pred. probabilities of turnout
avgpr <- numeric(length(x))

# avg. pred. probabilities for different values of gender
# holding all other variables at their observed values
for (i in 1:length(x)) {
  mf$gender <- rep(x[i], nrow(mf))
  avgpr[i] <- mean(predict(model3, mf, type = "response"))
}

# show average predicted probabilities
data.frame(x, avgpr)

#keeping all other variables as they are, male would affect .825
#3% points difference is not nothing but not significant

#' Interpretation
#' 
#' Holding all other variables at their observed values, the average 
#' predicted probability of turnout is about three percentage points 
#' lower for men than for women (82.6 percent vs. 85.3 percent).


# the effect of gender visualized with ce.plot()

source("ceplot.R")         # run R script for ce.plot()
ce.plot(model3, gender, ylim = c(0, 1)) #impose limits 



### Interaction

## Interactions of body height and body weight with gender

model4 <- glm(turnout ~ height * gender 
              + weight * gender 
              + age, 
              family = binomial(link = "logit"), 
              issp
              )

summary(model4)

#interaction: is the how much the effect is stronger than men (is the difference)


#' Interpretation
#' 
#' There are significant differences in the effects of body height 
#' and body weight between men and women. 
#' 
#' To see the effects of height and weight in each gender group, 
#' we respecify the above model as follows:

summary(glm(turnout ~ height:gender 
            + weight:gender 
            + gender 
            + age, 
            family = binomial(link = "logit"), 
            issp
            )
        )


#' This is the same model as above, but it is more intuitive as it 
#' allows us to see the effects of body height and body weight in 
#' each gender group and to test whether each effect is significantly 
#' different from zero. In contrast, the previous model allows us to 
#' test whether the effects of body height and body weight differ 
#' significantly between men and women. 


#' The best way to understand and interpret interaction effects 
#' is by visualizing them:

par(mfrow = c(1, 2))   #qn?
ce.plot(model4, 
        weight, 
        fixvar = "gender", 
        fixval = "male", 
        ylim = c(0, 1)
        )
ce.plot(model4, 
        weight, 
        fixvar = "gender", 
        fixval = "female", 
        ylim = c(0, 1)
        )
par(mfrow = c(1, 1))

par(mfrow = c(1, 2))
ce.plot(model4, 
        height, 
        fixvar = "gender", 
        fixval = "male", 
        ylim = c(0, 1)
        )
ce.plot(model4, 
        height, 
        fixvar = "gender", 
        fixval = "female", 
        ylim = c(0, 1)
        )
par(mfrow = c(1, 1))


## Adding further control variables to rule out spurious correlation

model5 <- glm(turnout ~ height * gender 
              + weight * gender 
              + age 
              + income 
              + educ 
              + feduc 
              + meduc 
              + region, 
              family = binomial(link = "logit"), 
              issp
              )
summary(model5)

summary(glm(turnout ~ height * gender 
            + weight:gender 
            + gender 
            + age 
            + income 
            + educ 
            + feduc 
            + meduc 
            + region, 
            family = binomial(link = "logit"), 
            issp
            )
        )



#' What do the results tell us?
#' 
#' The effect of body weight disappears after controlling for income, 
#' education, and other variables. This suggests that the correlation 
#' between body weight and turnout is spurious: greater weight is 
#' associated with lower income and lower levels of education, both of
#' which decrease turnout.
#' 
#' The effect of body height on turnout among females is robust to the 
#' inclusion of income and education. This is remarkable since greater 
#' body height is related to higher income, which, in turn, is a strong 
#' predictor of turnout. Yet, the effect of body height among females 
#' cannot be "explained away".


# displaying and comparing the effect sizes of all variables in the model 

par(mfrow = c(3, 4))
ce.plot(model5, 
        height, 
        fixvar = "gender", 
        fixval = "male", 
        ylim = c(0, 1)
        )
ce.plot(model5, 
        height, 
        fixvar = "gender", 
        fixval = "female", 
        ylim = c(0, 1)
        )
ce.plot(model5, 
        weight, 
        fixvar = "gender", 
        fixval = "male", 
        ylim = c(0, 1)
        )
ce.plot(model5, 
        weight, 
        fixvar = "gender", 
        fixval = "female", 
        ylim = c(0, 1)
        )
ce.plot(model5, gender, ylim = c(0, 1))
ce.plot(model5, age, ylim = c(0, 1))
ce.plot(model5, income, ylim = c(0, 1))
ce.plot(model5, region, ylim = c(0, 1))
ce.plot(model5, educ, ylim = c(0, 1))
ce.plot(model5, feduc, ylim = c(0, 1))
ce.plot(model5, meduc, ylim = c(0, 1))
par(mfrow = c(1, 1))



### Testing for joint significance of respondent education

#' H0: all coefficients of respondent eduction are zero
#' H1: at least one coefficient of respondent education is not zero


# Two commonly employed tests: Wald test and likelihood ratio test

# fitting the restricted model (excluding educ)
model5restrict <- glm(turnout ~ height * gender 
                      + weight * gender 
                      + age 
                      + income 
                      + feduc 
                      + meduc 
                      + region, 
                      family = binomial(link = "logit"), 
                      model.frame(model5)
                      )

# ...or, more elegantly:
model5restrict <- update(model5, . ~ . - educ, data = model.frame(model5))

#subtract the variable education

library(lmtest)                   # load required package
waldtest(model5, model5restrict)  # Wald test of joint significance
lrtest(model5, model5restrict)    # Likelihood ratio test of joint significance

#the first test fail to reject
#the other test yes at 5%
#You can choose whichever you like
#lrtest is less based on algorithm assumptions
#the waldtest require the curvature of the likelihood function
#Likelioodr function: which can be on different based ont eh algoritm you use


### Table with regression results

library(stargazer)

# plain table without variable labels
stargazer(model1, 
          model2, 
          model3, 
          model4, 
          model5, 
          type = "text", 
          digits = 2, 
          order = c("height", 
                    "weight", 
                    "gender:height", 
                    "gender:weight", 
                    "gender", 
                    "age", 
                    "region", 
                    "income"
                    )
          )


# presentation-ready table
stargazer(model1, 
          model2, 
          model3, 
          model4, 
          model5, 
          out = "table2.html", 
          digits = 2, 
          dep.var.labels = "Turnout", 
          order = c("height", 
                    "weight", 
                    "gender:height", 
                    "gender:weight", 
                    "gender", 
                    "age", 
                    "region", 
                    "income"
                    ), 
          covariate.labels = c("Body height (cm)",
                               "Body weight (kg)",
                               "Female X height",
                               "Female X weight",
                               "Female",
                               "Age (years)",
                               "East Germany",
                               "Net income (Euro)",
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
                               "Mother's ed. (university)"
                               )
          )

# This creates an html file. Open the file with your browser, 
# and copy-paste the table into Word.

#std error decreases when you add more data

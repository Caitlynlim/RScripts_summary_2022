#############################
# Data Analysis with R
# Michael Herrmann
# University of Konstanz
# Session 4
############################


# Do taller people have higher incomes?


# In this exemplary analysis we shall go through the  
# typical steps of a data analysis in the social sciences:

#  1. Preparing the data for analysis
#  2. Regression analysis
#  3. Interpreting results
#  4. Computing predictions and effect sizes
#  5. Displaying results
#  6. Exporting results  					



# remove everything
rm(list = ls())


# set working directory

setwd("/Users/CaitlynLim/Documents/RScripts/Session 4")


# read data set

library(haven)
library(rio)
issp <- data.frame(as_factor(read_dta("data3.dta")))
issp <- spread_attrs(droplevels(gather_attrs(issp))) 



## 1. Data preparation


# restrict sample to German citizens

levels(issp$v4)

class(issp$v4)
table(issp$v4, useNA = "always")

issp <- issp[as.numeric(issp$v4) <= 2, ]      # drop non-German citizens

# check
table(issp$v4, useNA = "always")



# dependent variable: income

class(issp$v388)                              # income is a factor
table(issp$v388, useNA = "always")            # inspect values

issp$income <- issp$v388                      # create income var
issp$income[issp$income == "KEIN EINKOMMEN"   # code missings (Make those as NA)
            | issp$income == "VERWEIGERT" 
            | issp$income == "KEINE ANGABE"
            ] <- NA
issp$income <- as.numeric(as.character(issp$income))  # convert factor to numbers

# check coding
issp[1:100, c("v388", "income")]



# independent variable: body height

class(issp$v629)
table(issp$v629, useNA = "always")

issp$height <- issp$v629
issp$height[issp$height == "WEISS NICHT"] <- NA
issp$height <- as.numeric(as.character(issp$height))

# check coding
issp[1:100, c("v629", "height")]



## 2. Data analysis

# correlation of income and height

cor(issp$height, issp$income, use = "complete.obs")       # Pearson correlation coef.
cor(issp[, c("height", "income")], use = "complete.obs")  # Pearson correlation matrix

# bivariate linear regression

lm(income ~ height, data = issp)                # sparse output
model1 <- lm(income ~ height, data = issp)      # storing results
#income= -3987 + 31Height + error
#For each cm of height the income increases by 31

summary(model1)                                 # output in a more informative way

model.frame(model1)                             # data used in the estimation
dim(model.frame(model1))                        # N and number of vars used 


#When you analyse individuals 0.3 is actually really strong for individual data 
#but for group data then maybe different

## 3. Interpreting results

#' Interpretation
#'
#' Each additional cm of body height is associated with an 
#' increase in net income of 31 Euros, on average.
#' 
#' The effect of body height on income is statistically 
#' significant at the 5 percent level. 					



## 4. Computing predictions and effect sizes

# extracting estimation results

coef(model1)                        # model coefficients (as named vector)
confint(model1, level = 0.90)                     # 95% confidence intervals of coefs
vcov(model1)                        # variance covariance matrix of coefs
residuals(model1)                   # deviations from regression line


# calculating effect sizes

a <- as.vector(coef(model1)[1])     # intercept (without its name)
b <- as.vector(coef(model1)[2])     # coef of height (without its name)

a + 150 * b                         # predicted income for a short person
a + 200 * b                         # predicted income for a tall person
50 * b                              # income difference between both persons      

minyhat <- a + min(issp$height, na.rm = TRUE) * b  # pred. for shortest person in sample
maxyhat <- a + max(issp$height, na.rm = TRUE) * b  # pred. for tallest person in sample 
maxyhat - minyhat                                  # pred. difference

#na.rm excludes na on body height

## 5. Displaying results

# visualizing the effect of height on income

# scatterplot including the regression line
plot(income ~ height, issp)
abline(lm(income ~ height, issp))


## Testing and displaying a nonlinear relationship

# Is the relationship between height and income nonlinear?

# Linear regression including a quadratic term
model2 <- lm(income ~ height + I(height ^ 2), data = issp)
summary(model2)


#' Interpretation

#' The coefficient of body height squared is statistically significant. 
#' There is evidence of an inverse U-shaped relationship between body 
#' height and income. But how does the relationship look like?


# Displaying the nonlinear relationship

coef <- as.vector(coef(model2))

# create x-values for plotting 
hgt <- seq(min(issp$height, na.rm = TRUE), max(issp$height, na.rm = TRUE), 1)

# compute predicted y-values
yhat_model2 <- coef[1] + coef[2] * hgt + coef[3] * hgt ^ 2

# scatter plot with fitted regression line
plot(income ~ height, issp)                # scatter plot
lines(hgt, yhat_model2)                    # regression curve 

# Note: hgt and yhat are not in the data frame, and they don't need to be

# simpler generation of y-values for the above graph
yhat_model2 <- predict(model2, data.frame(height = hgt))

plot(income ~ height, issp)                # scatter plot
lines(hgt, yhat_model2)                    # regression curve



### 6. Exporting results

summary(model1)
summary(model2)

#' You can select/highlight individual coefficients with the mouse and 
#' copy-paste them into Excel. In Excel you would replace all decimal points  
#' with commas (if desired), set the number of decimal places, and then copy 
#' and paste the results into an MS Word table.
#' 
#' If you find all this too tedious, try the stargazer package. First, 
#' install the package: "Tools" -> "Install packages..." - Next, run:

library(stargazer)

stargazer(model1, model2, type = "text")
stargazer(model1, 
          model2, 
          type = "text", 
          dep.var.labels = "Monthly net income (Euros)", 
          covariate.labels = c("Body height (cm)", "Body height squared"), 
          digits = 2
          )

#' This looks better and can be copied and pasted into a Word document or 
#' into an email (to your instructor or supervisor). Note that if you try to 
#' change the layout of the table in MS Word, the formatting will break down.
#' 
#' Stargazer works best for those who use LaTeX to typset their documents, 
#' instead of MS Word. If you don't use LaTeX but still want a nice looking and 
#' editable table inside an MS Word document, try this:
#' 
#' 1. Save the stargazer table as an HTML file (see below)
#' 2. Open the HTML file and copy and paste the table from the browser window 
#'    into your (MS Word) document

stargazer(model1, 
          model2, 
          out = "table.html", 
          dep.var.labels = "Monthly net income (Euros)", 
          covariate.labels = c("Body height (cm)", "Body height squared"), 
          digits = 2
          )

View(issp)

### Exercise 4

# Do younger generations grow taller than older generations?

# 1. Generate a numeric variable age from variable v154. Make sure to code missing 
#    values on age as NA.
# 2. Compute the Pearson correlation coefficient of age and body height.
# 3. Run a linear regression of body height (dependent variable) on age (independent 
#    variable).
# 4. Produce a scatter plot of body height on the y-axis vs. age on the x-axis. (You
#    do not need to fine-tune the plot for presentation). Add a regression line to
#    the plot.
# 5. Do younger generations grow taller than older generations, on average? (Yes or No)

## Exercise 5

# In Exercise 4 you showed that later birth cohorts grow taller than earlier 
# ones. This exercise asks you to control for gender.

# 1. Generate the variable ybirth (year of birth) using variable v153. Find 
#    out how missing values are coded on v153 and set ybirth to NA when v153 
#    is missing.

View(issp$v153)
table(issp$v153, useNA ="always") #Check the responses?
issp$ybirth <- issp$v153
issp$ybirth[issp$ybirth == "KEINE ANGABE"   # code missings
            ] <- NA
issp[ c("v153", "ybirth")] #check code

# 2. Run a linear regression of body height (dependent variable) on year of 
#    birth, controlling for gender. (Use the variables height and gender from 
#    Session 5.)

model4 <- lm(height ~ ybirth + gender, issp)
summary(model4) 
abline(model4)

# 3. Interpret the effects of birthyear and gender in words.

#Females are 12.438cm shorter than males
#On average the later your birth year the taller you are

# 4. Run a linear regression of body height on the interaction of gender and 
#    birthyear. (regression: dependent on the independent,
#                 effect of the independent on the dependent)

model5 <- (lm(height ~ ybirth + gender + ybirth:gender, issp)) 
plot(model5)
abline(model5)
summary(lm(height ~ ybirth + gender + ybirth:gender, issp))

# 5. Produce a scatter plot of body height (y-axis) vs. birthyear (x-axis) and 
#    visually distinguish between men and women. Show the interaction of 
#    birthyear and gender by superimposing SEPARATE regression lines for men 
#    and women (you do not need to fine-tune the plot further for presentation).

plot(height ~ ybirth, issp, col = c("blue", "hotpink")[gender])
# including regression line for women
abline(lm(height ~ ybirth, issp, gender == "female"), col = "hotpink")  
# including regression line for men
abline(lm(height ~ ybirth, issp, gender == "male"), col = "blue")

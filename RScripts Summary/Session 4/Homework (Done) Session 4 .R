### Exercise 4

# Do younger generations grow taller than older generations?

# 1. Generate a numeric variable age from variable v154. Make sure to code missing 
#    values on age as NA.

class(issp$v154)
table(issp$v154, useNA = "always") 

issp$age <- issp$v154
issp$age[issp$age == "KEINE ANGABE"   # code missings (Make those as NA)
            ] <- NA
issp$age <- as.numeric(as.character(issp$age)) #convert to numeric


# 2. Compute the Pearson correlation coefficient of age and body height.

class(issp$v629)
table(issp$v629, useNA = "always")

issp$height <- issp$v629
issp$height[issp$height == "WEISS NICHT"   # code missings (Make those as NA)
         ] <- NA
issp$height <- as.numeric(as.character(issp$height))

class(issp$v629)

# check coding
issp[1:100, c("v154", "age", "v629", "height")]

cor(issp$age, issp$height, use = "complete.obs") #Pearson correlation coefficient

# 3. Run a linear regression of body height (dependent variable) on age (independent 
#    variable).

lm(height ~ age, data = issp) 

# 4. Produce a scatter plot of body height on the y-axis vs. age on the x-axis. (You
#    do not need to fine-tune the plot for presentation). Add a regression line to
#    the plot.

plot(height ~ age, issp)
abline(lm(height ~ age, issp))

# 5. Do younger generations grow taller than older generations, on average? (Yes or No)

Yes.



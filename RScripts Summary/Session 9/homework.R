# 1. Create a box plot of log(income) on the y-axis vs. employment 
#    status (emp) on the x-axis. Restrict the plot to persons who
#    have an income, i.e., exclude people with an income of zero.

setwd("/Users/CaitlynLim/Documents/RScripts/Session 9")

library(rio)
library(haven)
soep <- data.frame(as_factor(read_dta("data1.dta")))
soep <- spread_attrs(droplevels(gather_attrs(soep)))

class(soep$income)
table(soep$income)
soep$inc <- as.factor(soep$income) #why
table(soep$income, as.numeric(soep$income))
soep$inc1 <- as.numeric(soep$inc) - 1
table(soep$inc, soep$inc1, useNA = "always")

boxplot(soep$inc1 ~ soep$emp, 
        vertical = TRUE, 
        xlab = "Employment Status", 
        boxwex = 0.2, 
        las = 1
)

# 2. Replace the label of the y-axis with "Labor earnings (log)" 
#    and replace the label of the x-axis with "Employment status" 

boxplot(soep$inc1 ~ soep$emp, 
        vertical = TRUE, 
        xlab = "Employment Status",
        ylab = "Labor earnings (Log)",
        boxwex = 0.2, 
        las = 1
)


# 3. Make each box narrower, i.e., reduce the amount of horizontal 
#    space used up by each box.

boxplot(soep$inc1 ~ soep$emp, 
        vertical = TRUE, 
        xlab = "Employment Status",
        ylab = "Labor earnings (Log)",
        boxwex = 0.1, 
        las = 1
)


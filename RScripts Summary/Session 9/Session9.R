#############################
# Data Analysis with R
# Michael Herrmann
# University of Konstanz
# Session 9
############################


### R Graphics


setwd("/Users/CaitlynLim/Documents/RScripts/Session 9")

library(rio)
library(haven)
soep <- data.frame(as_factor(read_dta("data1.dta")))
soep <- spread_attrs(droplevels(gather_attrs(soep)))



### Examining life satifaction

# data preparation: factor to numeric
class(soep$lsat)
table(soep$lsat)
table(soep$lsat, as.numeric(soep$lsat))
soep$lifesat <- as.numeric(soep$lsat) - 1
soep$lifesat[soep$lifesat == 11] <- NA
table(soep$lsat, soep$lifesat, useNA = "always")


## Histogram

hist(soep$lifesat)

# bins centered on values of lsat
hist(soep$lifesat, breaks = seq(-0.5, 10.5))

# labels, (no) title and color
hist(soep$lifesat, 
     breaks = seq(-0.5, 10.5), 
     xlab = "Life satisfaction", 
     main = "", 
     col = "black"
     )

# improving the display of the x-axis
hist(soep$lifesat, 
     breaks = seq(-0.5, 10.5), 
     xlab = "Life satisfaction", 
     main = "", 
     col = "grey", 
     axes = FALSE
     )
axis(1, at = 0:10)
axis(2, at = seq(0, 1500, 500))

# suppressing the x-axis (if desired)
hist(soep$lifesat, 
     breaks = seq(-0.5, 10.5), 
     xlab = "Life satisfaction", 
     main = "", 
     col = "grey", 
     axes = FALSE
     )
axis(1, at = 0:10, line = -1, lwd = 0)
axis(2, at = seq(0, 1500, 500))



## Boxplot

boxplot(soep$lifesat, 
        horizontal = TRUE, 
        xlab = "Life satisfaction",
        boxwex = 0.1
        )

# boxplots over subgroups (typical use)

# life satisfaction by party
boxplot(soep$lifesat ~ soep$pib, 
        ylab = "Life satisfaction", 
        boxwex = 0.2
        )

# life satisfaction by state
boxplot(soep$lifesat ~ soep$state, 
        horizontal = TRUE, 
        xlab = "Life satisfaction", 
        boxwex = 0.4, 
        las = 1
        )

# The output does not fit into the graphics region: there's not 
# enough space for printing the state names. We can increase the 
# available space by resetting the plot margins:

par(mar = c(6, 9, 2, 2))       # bottom, left, top, right

boxplot(soep$lifesat ~ soep$state, 
        horizontal = TRUE, 
        xlab = "Life satisfaction", 
        boxwex = 0.4, 
        las = 1
        )

par(mar = c(5, 4, 4, 2) + 0.1) # restore default values



## Graphical parameters

par()                      # get values of all graphical parameters
?par                       # documentation on graphical parameters

par_default <- par()       # store default values for later use



## Drawing multiple plots

# Histograms of life satisfaction by party

# First, we store the default values of the graphical parameters 
# for later use

par_default <- par()

# We then divide the figure into six rows and one column and fill 
# cells by going down columns:

par(mfcol = c(6, 1))

# Next, we draw the six histograms:

hist(soep$lifesat[soep$pib == "Greens/B90"], 
     breaks = seq(-0.5, 10.5), 
     main = "Greens", 
     col = "grey", 
     xlab = "", 
     axes = FALSE
     )
axis(2, at = seq(0, 40, 20))

hist(soep$lifesat[soep$pib == "CDU/CSU"], 
     breaks = seq(-0.5, 10.5), 
     main = "CDU/CSU", 
     col = "grey", 
     xlab = "", 
     axes = FALSE
     )
axis(2, at = seq(0, 200, 100))

hist(soep$lifesat[soep$pib == "SPD"], 
     breaks = seq(-0.5, 10.5), 
     main = "SPD", 
     col = "grey", 
     xlab = "", 
     axes = FALSE
     )
axis(2, at = seq(0, 200, 100))

hist(soep$lifesat[soep$pib == "FDP"], 
     breaks = seq(-0.5, 10.5), 
     main = "FDP", 
     col = "grey", 
     xlab = "", 
     axes = FALSE
     )
axis(2, at = seq(0, 40, 20))

hist(soep$lifesat[soep$pib == "Linke"], 
     breaks = seq(-0.5, 10.5), 
     main = "Linke", 
     col = "grey", 
     xlab = "", 
     axes = FALSE
     )
axis(2, at = seq(0, 40, 20))

hist(soep$lifesat[soep$pib == "DVU, Rep., NPD"], 
     breaks = seq(-0.5, 10.5), 
     main = "DVU,Rep.,NPD", 
     col = "grey", 
     xlab = "Life satisfaction", 
     axes = FALSE
     )
axis(2, at = seq(0, 5, 5))

axis(1, at = 0:10)

# Finally, we restore the default values of the graphical 
# parameters:

par(par_default)

# Note: This might not run because the plot window in R Studio 
# is too small. You can try to enlarge the plot window, but it 
# might still be too small. To produce large graphs with multiple 
# plots, it is best not to plot the result in R Studio, but to 
# write the entire graph into a separate file, as shown in the 
# next example.



## Exporting Graphs

# Possible storage types:

# pdf("test.pdf", width = 5, height = 15)         # PDF (size given in inches)
# postscript("test.eps", width = 5, height = 15)  # postscript file (size in inches)
# tiff("test.tif", width = 300, height = 900)     # TIF file (size in pixels)
# jpeg("test.jpg", width = 300, height = 900)     # JPG file (size in pixels)
# bmp("test.bmp", width = 300, height = 900)      # bitmap file (size in pixels)


# First, we open a connection to a pdf file:

pdf("test.pdf", width = 5, height = 15)

# Note: This creates a PDF and sets the size of the graphics
# region (in inches). Every plot we subsequently produce is 
# written directly into this file, until we close the connection.


# Next, we apply the same steps as above to draw the graphs:

par_default <- par()

par(mfcol = c(6, 1))

hist(soep$lifesat[soep$pib == "Greens/B90"], 
     breaks = seq(-0.5, 10.5), 
     main = "Greens", 
     col = "grey", 
     xlab = "", 
     axes = FALSE
     )
axis(2, at = seq(0, 40, 20))

hist(soep$lifesat[soep$pib == "CDU/CSU"], 
     breaks = seq(-0.5, 10.5), 
     main = "CDU/CSU", 
     col = "grey", 
     xlab = "", 
     axes = FALSE
     )
axis(2, at = seq(0, 200, 100))

hist(soep$lifesat[soep$pib == "SPD"], 
     breaks = seq(-0.5, 10.5), 
     main = "SPD", 
     col = "grey", 
     xlab = "", 
     axes = FALSE
     )
axis(2, at = seq(0, 200, 100))

hist(soep$lifesat[soep$pib == "FDP"], 
     breaks = seq(-0.5, 10.5), 
     main = "FDP", 
     col = "grey", 
     xlab = "", 
     axes = FALSE
     )
axis(2, at = seq(0, 40, 20), labels = TRUE)

hist(soep$lifesat[soep$pib == "Linke"], 
     breaks = seq(-0.5, 10.5), 
     main = "Linke", 
     col = "grey", 
     xlab = "", 
     axes = FALSE
     )
axis(2, at = seq(0, 40, 20))

hist(soep$lifesat[soep$pib == "DVU, Rep., NPD"], 
     breaks = seq(-0.5, 10.5), 
     main = "DVU,Rep.,NPD", 
     col = "grey", 
     xlab = "Life satisfaction", 
     axes = FALSE
     )
axis(2, at = seq(0, 5, 5))

axis(1, at = 0:10)


# Finally, we close the connection to the pdf:

dev.off()       
# If the output does not say "null device", repeat this 
# command, until it does.

par(par_default)          # restore default values



## Exercise 7

# 1. Create a box plot of log(income) on the y-axis vs. employment 
#    status (emp) on the x-axis. Restrict the plot to persons who
#    have an income, i.e., exclude people with an income of zero.
# 2. Replace the label of the y-axis with "Labor earnings (log)" 
#    and replace the label of the x-axis with "Employment status" 
# 3. Make each box narrower, i.e., reduce the amount of horizontal 
#    space used up by each box.



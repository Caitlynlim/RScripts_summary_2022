### Exercise 2

# Run the following two lines to create vectors x and y:
x <- c(0, 4, 8, 12, 16, 20) 
y <- c(3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5)

# 1. Combine the first 5 elements of vector x with the 2nd to 12th element  
#    of vector y to the new vector z.

z <- c(x[1:5],y[2:12])
z

# 2. Set every third value of y to zero.

y[seq(from = 3, to = 12, by = 3)] <- 0
y

# Run the following line:
polint <- sample(rep(c(1:3, NA), c(55, 89, 78, 45)), 267)

polint
# Suppose polint is a variable measuring political interest on a sample of 
# 267 survey respondents using a scale from 1 (not interested) to 3 (very 
# interested).

# 3. Delete all missing values from polint.

polint3 <- polint[!is.na(polint)]

polint3

# 4. Convert polint to a factor with levels: low, mid, high

fp <- factor(polint3) #converts to factor 
fp

levels(fp) <- c("low", "mid", "high") #assign cat/levels

fp

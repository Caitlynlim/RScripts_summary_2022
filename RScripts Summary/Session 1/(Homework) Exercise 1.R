### Exercise 1

# 1. Create a vector x with the following elements (0, 4, 8, 12, 16, 20), 
#    without typing the numbers.
#
# How do Germans live? 
#
# 2. Produce a frequency table of the number of rooms each respondent 
#    lives in, based on the variable "rooms".
# 3. Compute the average housing size (in square feet) in the sample, 
#    based on the variable "size".

x=seq(from = 0, to = 20, by = 4)
print(x)

print("How do Germans live?")
table(soep$rooms, useNA = "always")
mean(soep$size, na.rm = TRUE)

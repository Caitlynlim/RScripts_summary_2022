############################
# Data Analysis with R
# Michael Herrmann
# University of Konstanz
# Session 2
############################

#Vectors #List of values where there's a fixed order/location,
#if you change the order, you change the vector
#each vector has elements
#Crafting, Accessing and changing content
#Types of information modes, Factors
#c (take the arguments and cantonate(glue them tgt))



### Vectors

# numeric vector
x <- c(4, 8, 15, 16, 23, 42)
x
length(x)                         # every data object has a length
y <- c(5, NA, 5, x, 4, NA, 8)     # substituting x into another vector
y
mode(y)                       #Vector can store either numbers/words but not both
# character vector
countries <- c("Germany", "Denmark", "Netherlands", "Portugal")
countries
mode(countries)

# logical vector
yesno <- x > 15       #x <- c(4, 8, 15, 16, 23, 42)     
yesno
# recycling: comparison is repeated for each element in x


## Modes #silent coersion where they change 

# vectors can only have one mode
z <- c(1, 2, "Merkel", 4)         # coersion to character
z
mode(z)
z <- c(1, 2, FALSE)               # coersion to numeric
z
mode(z)
z <- c(1, 2, "Merkel", FALSE)     # coersion to character
z
mode(z)


# switching modes (Conversion functions)
xchar <- as.character(x)          # numeric to character
xchar
xnum <- as.numeric(xchar)         # character to numeric
xnum
yesnonum <- as.numeric(yesno)     # logical to numeric 
yesnonum
as.logical(yesnonum)              # numeric to logical



## Subsetting/indexing 

# indexing by element number
countries
countries[2]               # second element
y
y[c(2, 5, 10)]             # 2nd, 5th, and 10th element #must have the c for vector,
                            #A vector here only has one dimension so 
                           #without the c there would be an error,
                            # [ ] would be for arrays,data,matrices 
                   #dataframes can have diff modes, matrices can't

y[1:6]                     # or: y[seq(1, 6)], y[c(1, 2, 3, 4, 5, 6)]
y[-1]                      # exclude the first element
y[seq(0, 10, 2)]           # 2nd, 4th, 6th, 8th, and 10th element
y[length(y)]               # last element

# logical indexing
x
x[c(TRUE, FALSE, TRUE, TRUE, FALSE, TRUE)] #Keep all the true, take away the false
x[x < 16]       #Highlight x < 16 you can find out the true/false             
# if index is TRUE, the corresponding element of x is selected

# indexing by name
names(x)
names(x) <- c("1st", "2nd", "3rd", "4th", "5th", "6th")
x
x["2nd"]                   

#h<-subset(soep, seval == "seval")

#h    

names(soep)

## Manipulating vectors

# changing content 
countries
countries[3] <- "Italy"            # replaces Netherlands with Italy
countries
y
y[c(1, 3, 5)] <- c(100, 110, 120)  # replaces 1st, 3rd, and 5th element
y
y[y <= 10] <- 0                    # set values lower or equal to 10 to zero 
y
y[y > 10] <- 1                     # set values greater than 10 to one
y

# Dealing with missing values: NAs

# detecting NAs
is.na(y)                           # returns a logical vector
sum(is.na(y))                      # counts the number of NAs in y
which(is.na(y))                    # positions of NAs in y 

# Functions operating on vectors (mean, sd, etc.) usually have an argument 
# stating how to deal with NAs. If not, NAs can be easily removed from the 
# vector in one of three ways:

y2 <- y[is.na(y) == FALSE]
y3 <- y[!is.na(y)]
y4 <- y[complete.cases(y)]

y2

### Factors

# Factors are vectors whose values denote different categories (called 
# "levels"). Technically, factors are a cross between a numeric and a 
# character vector: they store information numerically while attaching 
# characters to each numeric value. 

# generating a factor
f <- factor(c(3, 2, 2, 2, 2, 3), labels = c("day", "night"))
f

mode(f)                   # the mode of a factor is numeric, not character
as.numeric(f)             # the numeric values underlying the categories
as.character(f)           

# generating a factor from a character vector
fc <- factor(countries)
fc

# generating a factor from a numeric vector
y
fy <- factor(y)           # conversion to factor
fy
# Note: The displayed values are only labels. The underlying numeric 
# values are always 1, 2, ... 
as.numeric(fy)
as.character(fy)

# changing levels
levels(fy)                      # the categories of fy
levels(fy) <- c("red", "blue")  # new categories
fy
levels(fc)
levels(fc)[2] <- "Italy"        # code Germany as Italy
fc


#Levels are like catergories

### Exercise 2

# Run the following two lines to create vectors x and y:
x <- c(0, 4, 8, 12, 16, 20) 
y <- c(3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5)

# 1. Combine the first 5 elements of vector x with the 2nd to 12th element  
#    of vector y to the new vector z.
# 2. Set every third value of y to zero.

# Run the following line:
polint <- sample(rep(c(1:3, NA), c(55, 89, 78, 45)), 267)

# Suppose polint is a variable measuring political interest on a sample of 
# 267 survey respondents using a scale from 1 (not interested) to 3 (very 
# interested).

# 3. Delete all missing values from polint.
# 4. Convert polint to a factor with levels: low, mid, high

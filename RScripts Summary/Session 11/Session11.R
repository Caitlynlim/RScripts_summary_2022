############################
# Data Analysis with R
# Michael Herrmann
# University of Konstanz
# Session 11
############################


### Combining/joining data sets

#Matching by columns --> append: adding observations
#Do not have the same observations but some observations are the same
#matching by rows adding variables --> merging 

# There are two ways to combine data sets:

# 1. Adding new observations/rows (by matching variables)    =>  rbind()
# 2. Adding new variables/columns (by matching observations) =>  merge()


rm(list = ls())

setwd("/Users/CaitlynLim/Documents/RScripts/Session 11")



# 1. Creating a data set with all candidates from the 2009 and 2013
#    Bundestag elections 

# reading the candidate data from both elections into R
library(rio)
cnd09 <- import("Bewerber0913.xlsx", sheet = "Bewerber2009")
cnd13 <- import("Bewerber0913.xlsx", sheet = "Bewerber2013")

#List of all the candidates that stood for election

# To join the two data sets, the variables must be identically named! 

# inspecting the names of variables in both data sets
sort(names(cnd09))
sort(names(cnd13))    # variables are identically named but cnd09 lacks mdb    

# creating a variable to distinguish elections in the target data set
cnd09$btw <- 2009
cnd13$btw <- 2013

#rbind is append
# combining both data frames at the exclusion of mdb
cnd <- rbind(cnd09, cnd13[, -14]) #combine, with the exception of variable 14 
View(cnd[3500:4000, ])

#to check for the underlying number: names(cnd2013)
#you can select a variable by name but not deselect by name

#which(names(cnd13) == "mdb")

# combining both data frames, including mdb
cnd09$mdb <- NA #Make it NA for the one without mdb
cnd <- rbind(cnd09, cnd13)
View(cnd[3500:4000, ])




# 2. Merging the 2013 election results with candidate data from 2013

# Our aim is to create a data set in which each row represents a candidate 
# in the 2013 election. Variables should contain attributes of the candidates 
# from the Bewerber2013 data set as well as the number of first votes 
# (Erststimmen) and second votes (Zweitstimmen) each candidate and his or her 
# party won in the constituency. The election results are contained in the 
# data set Ergebnis2013 (see below).

# First, we read the 2013 election results into R

er13 <- import("Ergebnis2013.xlsx")
View(er13)

# Which variables could be used to match candidates with their election 
# results?


# Second, we reshape the data set so that each unit corresponds to
# a party within a constituency

# Our goal is to stack the voting results party-wise, such that all
# Erststimmen vote counts go in one variable and all Zweitstimmen 
# vote counts go in another. To achieve this, we must rename the 
# vote count variables like this: "ballot_party"

names(er13)[seq(10, 78, 2)] <- paste("erst", #paste glues two text string tgt
                                     unlist(
                                       strsplit(
                                         names(er13)[seq(10, 78, 2)], "1" #split the string then remove the 1
                                         )
                                       ), 
                                     sep = "_" #this is the glue
                                     )

names(er13)[seq(11, 79, 2)] <- paste("zweit", 
                                     unlist(
                                       strsplit(
                                         names(er13)[seq(11, 79, 2)], "2"
                                         )
                                       ), 
                                     sep = "_"
                                     )

# reshaping the data
vars <- names(er13)[10:79]
er13 <- reshape(er13, 
                direction = "long", 
                varying = vars, 
                sep = "_", 
                timevar = "partei", 
                idvar = "wkr"
                )
View(er13)


# Third, we join election results and candidate information

cnd13 <- import("Bewerber0913.xlsx", sheet = "Bewerber2013")

# One difficulty here is that we want to merge the two data sets based on 
# constituency number and party name, but spelling of party names differs
# between the data sets:

table(cnd13$partei)
table(er13$partei)

# create a clone of partei (for later comparison)
cnd13$p <- cnd13$partei     

# now we switch all letters to lower case and turn umlauts into vowels
cnd13$partei <- tolower(cnd13$partei)
cnd13$partei <- chartr("?", "u", cnd13$partei)
cnd13$partei <- chartr("?", "o", cnd13$partei)

# this resolves differences for about half of the party names:
table(cnd13$partei)
table(er13$partei)

# For the remaining 17 differences, we manually create one vector containing   
# the names to be changed and another containing the desired replacements  
# (in the same order). We then loop over the elements of both vectors to
# replace each instance of an old name in cnd13$parties with its 
# corresponding new name: 

oldnames <- c("bundnis 21/rrp", 
              "die frauen", 
              "die linke", 
              "die partei", 
              "die rechte", 
              "die violetten", 
              "familie", 
              "freie w?hler", 
              "grune", 
              "nein!", 
              "partei der nichtw?hler", 
              "partei der vernunft", 
              "piraten", 
              "pro deutschland", 
              "rentner", 
              "tierschutzpartei", 
              "volksabstimmung"
              )

newnames <- c("rrp", 
              "frau", 
              "link", 
              "partei", 
              "recht", 
              "viol", 
              "fam", 
              "fw", 
              "grun", 
              "nein", 
              "nicht", 
              "pdv", 
              "pira", 
              "deut", 
              "rent", 
              "tier", 
              "volk"
              )

for (i in 1:length(oldnames)) {
  cnd13$partei[cnd13$partei == oldnames[i]] <- newnames[i]
}

cnd13[1:100, c("partei", "p")]  # compare to original values...
cnd13$p <- NULL                 # ...and delete variable p


# Finally, we merge both data sets

# natural join
ercnd13 <- merge(cnd13, er13, by = c("wkr", "partei"))
# retains only matched cases

# left outer join
ercnd13 <- merge(cnd13, er13, by = c("wkr", "partei"), all.x = TRUE)
# retains all unmatched candidates in addition to matched candidates

# right outer join
ercnd13 <- merge(cnd13, er13, by = c("wkr", "partei"), all.y = TRUE)
# retains all unmatched election results in addition to matched results

# full outer join
ercnd13 <- merge(cnd13, er13, by = c("wkr", "partei"), all = TRUE)
# retains all unmatched candidates and election results in addition to matched ones



summary(soep$age)
View(soep$age)
levels(soep$age)

# 1. Create a factor called minor identifying respondents under age 18. The factor 
#    should have two levels: "adult" and "under age 18". Check whether age contains 
#    missing values and make sure they are retained. Verify that you've created the 
#    variable correctly.

soep$adultXminor <- "Adult"  #creating 2 levels
soep$adultXminor <- ifelse(soep$age == 17, "Minor", soep$adultXminor)
View(soep$adultXminor)

soep[1:100, c("age", "adultXminor")]   #check
soep[is.na(soep$age), c("age", "adultXminor")]  # check missings, no missing

# 2. Take a look at the variable lsat. Create a factor from lsat with three levels:  
#    "dissatisfied" (from "completely dissatisfied" to "4"), "neither/nor" (equal to
#    "intermediate"), "satisfied" (from "6" to "completely satisfied"). Make sure all 
#    missing values on lsat are retained. Verify that you've created the variable 
#    correctly.


soep$conlsat <- NA
soep$conlsat <- ifelse(soep$lsat %in% c("Completely dissatisfied",1,2,3,4), 
                              "Dissatisfied", soep$conlsat)

soep$conlsat <- ifelse(soep$lsat %in% c("Intermediate"), 
                       "Neither/nor", soep$conlsat)

soep$conlsat <- ifelse(soep$lsat %in% c(6,7,8,9,"Completely satisfied"), 
                       "Satisfied", soep$conlsat)


soep[1:100, c("conlsat", "lsat")]                # check 
soep[is.na(soep$married), c("conlsat", "lsat")]  # check missings

View(soep$conlsat)

#soep$lsat <- levels(c("dissatisfied", "neither/nor", "satisfied"))




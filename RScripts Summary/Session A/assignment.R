rm(list = ls())

#1a

setwd("/Users/CaitlynLim/Documents/RScripts/Session A")

library(rio)
library(haven)

gles <- data.frame(as_factor(read_dta("gles2013extract.dta", encoding = "latin1")))
gles <- spread_attrs(droplevels(gather_attrs(gles))) 

#1b

View(gles, c("vn1, vn2c, vn35, v39, n39, v41, n41, 
             vn42a, vn42b, vn43a, vn43b, vn62, vn119a, ostwest"))

reducedVar <- c("vn1", "vn2c", "vn35", "v39", "n39", "v41", "n41", 
              "vn42a", "vn42b", "vn43a", "vn43b", "vn62", "vn119a", "ostwest")

gles1 <- gles[reducedVar]

View(gles1)

#1c

#v41 contains the answers of respondents surveyed before the election 
#n41 contains those from respondents surveyed after the election
#didn???t take part in a given survey wave are coded as ???nicht in Auswahlgesamtheit???
#on the respective variable.) Code all answers other than Merkel and Steinbr??ck as NA.
#Label the levels of prefmerkel ???Merkel??? and ???Steinbrueck??? and define ???Steinbrueck???
#as the base level.


#For before the electiion preference 
class(gles1$v41) 
table(gles1$v41, useNA = "always")
gles1$pmb <- gles1$v41
gles1$pmb <- ifelse(gles1$pmb == "Angela Merkel" | gles1$pmb == "Peer Steinbrueck",
                    gles1$pmb,
                    NA)
gles1[1:100, c("v41", "pmb")]

gles1$prefMerkelB <- factor(c(gles1$pmb), labels = c("Merkel", "Steinbrueck"))

gles1[1:100, c("v41", "prefMerkelB")]  #check

#For after the election preference
class(gles1$n41) 
table(gles1$n41, useNA = "always")
gles1$pma <- gles1$n41
gles1$pma <- ifelse(gles1$pma == "Angela Merkel" | gles1$pma == "Peer Steinbrueck",
                    gles1$pma,
                    NA)
gles1[1:100, c("n41", "pma")]

gles1$prefMerkelA <- factor(c(gles1$pma), labels = c("Merkel", "Steinbrueck"))

gles1[2004:3000, c("n41", "prefMerkelA")]  #check

#1d 

class(gles1$vn35)                              
table(gles1$vn35, useNA = "always")            # inspect values

gles1$angst <- gles1$vn35                      # create angst var
gles1$angst <- ifelse(gles1$angst == "ueberhaupt keine Angst"
                      | gles1$angst == "wenig Angst"
                      | gles1$angst == "mittelmaessige Angst"
                       | gles1$angst == "grosse Angst"
                       | gles1$angst == "sehr grosse Angst",
                    gles1$angst,
                    NA)
gles1[1:100, c("vn35", "angst")]

gles1$angst <- as.numeric(as.character(gles1$angst)) #convert to numeric

gles1$angst <- factor(c(gles1$angst), labels = c("4", "3", "2","1","0")) #lable respectively to numbers


gles1[1:100, c("vn35", "angst")] #check

#1e

#For before the electiion preference 
class(gles1$v39) 
table(gles1$v39, useNA = "always")
gles1$crisisB <- gles1$v39
gles1$crisisB <- ifelse(gles1$v39 == "sehr zufrieden" 
                        | gles1$v39 == "zufrieden"
                        | gles1$v39 == "teils/teils"
                        | gles1$v39 == "unzufrieden"
                        | gles1$v39 == "sehr unzufrieden",
                    gles1$crisisB,
                    NA)
gles1[1:100, c("v39", "crisisB")]

gles1$crisisB <- factor(c(gles1$crisisB), labels = c("4", "3", "2", "1", "0"))

gles1[1:100, c("v39", "crisisB")]  #check

#For after the electiion preference 
class(gles1$n39) 
table(gles1$n39, useNA = "always")
gles1$crisisA <- gles1$n39
gles1$crisisA <- ifelse(gles1$n39 == "sehr zufrieden" 
                        | gles1$n39 == "zufrieden"
                        | gles1$n39 == "teils/teils"
                        | gles1$n39 == "unzufrieden"
                        | gles1$n39 == "sehr unzufrieden",
                        gles1$crisisA,
                        NA)
gles1[2004:3000, c("n39", "crisisA")]

gles1$crisisA <- factor(c(gles1$crisisA), labels = c("4", "3", "2", "1", "0"))

gles1[2004:3000, c("n39", "crisisA")]  #check

#1f

#42-43 == merkel - steinbrueck
class(gles1$vn42a)                              
table(gles1$vn42a, useNA = "always")            # inspect values

gles1$mAssert <- gles1$vn42a                      # create angst var
gles1$mAssert <- ifelse(gles1$mAssert == "trifft ueberhaupt nicht zu"
                      | gles1$mAssert == "trifft eher nicht zu"
                      | gles1$mAssert == "teils/teils"
                      | gles1$mAssert == "trifft eher zu"
                      | gles1$mAssert == "trifft voll und ganz zu",
                      gles1$mAssert,
                      NA)    #4-8 completely doesn't believe to completely believe
gles1[1:100, c("vn42a", "mAssert")]

gles1$mAssert <- factor(c(gles1$mAssert), labels = c(0, 1, 2, 3, 4))

gles1$mAssert <- as.numeric(as.character(gles1$mAssert)) #convert to numeric

gles1[1:100, c("vn42a", "mAssert")]


sum(gles1$mAssert,na.rm=TRUE)








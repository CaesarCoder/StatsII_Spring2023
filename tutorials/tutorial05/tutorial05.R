#####################
# load libraries
# set wd
# clear global .envir
#####################

install.packages("nnet")
install.packages("MASS")
library(nnet)
library(MASS)
library(ggplot2)


# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

lapply(c(),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## Ordered multinomial logits:
  
  # This data set is analyzed by Long (1997).  The response variable has four ordered categories:
  # Strongly Disagree, Disagree, Agree, and Strongly Agree in relation to the statement
  # “A working mother can establishjust as warm and secure a relationship with her children as a mother who does not work."
  
  # The explanatory variables are:
  # the year of the survey (1977 or 1989),
  # the gender of the respondent, 
  # the race of the respondent (white or non-white), 
  # the respondent’s age, and 
  # the prestige of the respondent’s occupation (a quantitative variable)

workingMoms <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/WorkingMoms.txt", header=T)

# (a) Perform an ordered (proportional odds) logistic regression of attitude toward working mothers on the other variables.
# What conclusions do you draw?
workingMoms$gender <- as.factor(workingMoms$gender)
workingMoms$attitude <- factor(workingMoms$attitude,
                               levels = c("SD", "D", "A", "SA"),
                               labels = c("Strongly Disagree", 
                                          "Disagree",
                                          "Agree",
                                          "Strongly Agree"))
workingMoms$gender <- as.factor(workingMoms$gender)
workingMoms$race <- factor(workingMoms$race,
                           levels = c(0, 1),
                           labels = c("Non-white", "white"))
workingMoms$year<- factor(workingMoms$year,
                          levels = c("Year1977", "Year1989"),
                          labels = c("1977","1989"))
ordered_att <- polr(attitude ~ age + education + prestige + gender + race, data = workingMoms)
summary(ordered_att)

# being male, are being less likely to agree; the more prestiage, the more likeliy to agree 


# calculate a p value
ctable <- coef(summary(ordered_att))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable <- cbind(ctable, "p value" = p))

# calculate confidence intervals
(ci <- confint(ordered_att))

# covert to odds ratio 
exp(cbind(OR = coef(ordered_att), ci))



# b) fit a multinomial logit model 
workingMoms$attitude <- relevel(workingMoms$attitude, ref = "Strongly Disagree")


mult.log <- multinom(attitude ~., data = workingMoms)
exp(coef(mult.log))









exp(cbind(OR = coef(ordered_att), confint(ordered_att)))

(exp())

    
basepredict.polr(ordered_att, values = c (rep(0, 2), 1, rep(0, 7), 1))

# (b) Assess whether the proportional-odds assumption appears to hold for this regression. 
# Fit a multinomial logit model to the data, and compare and contrast the results with those from the proportional odds model.

# (c) Consider that possibility that gender interacts with the other explanatory variables in influencing the response variable. 
# What do you find?
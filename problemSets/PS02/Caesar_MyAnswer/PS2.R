#####################
# load libraries
# set wd
# clear global .envir
#####################

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

#####################
# Problem 1
#####################

# load data
load(url("https://github.com/ASDS-TCD/StatsII_Spring2023/blob/main/datasets/climateSupport.RData?raw=true"))



# Question 1: Additive Model 
library(stargazer)

mod1 <- glm(choice ~ countries  + sanctions, data = climateSupport,
            family = "binomial")
summary(mod1)
stargazer(mod1, title="Addictive Model")


# transfer log odds ratios to odds ratios 
itc <- exp(-0.006)
itc

cL <- exp(0.458)
cL

cQ <- exp(-0.010)  # p > 0.05
cQ

sL <- exp(-0.276)
sL

sQ <- exp(-0.181)
sQ

sC <- exp(0.150)
sC


## Get exp() here: https://www.educative.io/answers/how-to-calculate-the-natural-exponential-in-r-using-exp


#H0: All the coefficients are 0. 
# Ha: at least one coefficient is not 0. 

mod_null <- glm(choice ~ 1, family = binomial(link = "logit") , data = climateSupport)

CST <- anova(mod1, mod_null, test = "LRT")
stargazer(CST, title = "Chi-Square Test")

# we can deny the null hypothesis that all coefficients equal to 0, and we can conclude that at least one predictor is reliable. 





# Question 2: 

# a) # odds change 

odds_change <- exp(-0.181) - exp(-0.276)
odds_change # 0.07562243

# b) # probability 
logit_ns_80 <- -0.181 - 0.006

prob_perc <- exp(logit_ns_80)/(1+exp(logit_ns_80))*100
prob_perc  # 45.34% 

# c) Interaction Model or not?

mod2 <- glm(choice ~ countries  + sanctions + countries:sanctions, data = climateSupport,
            family =  binomial(link = "logit"))
summary(mod2)
stargazer(mod2, title="Interactive Model")



test2 <- anova(mod1, mod2, test="LRT")
stargazer(test2, title="Test: Interactive Model or not")
# p-value is 0.391
# no significant difference between the two models 

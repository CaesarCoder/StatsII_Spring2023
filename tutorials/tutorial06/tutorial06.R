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

## Poisson

# Data: Research Productivity of Doctoral Students in Bio-chemistry (Long 1990) 
# Productivity of doctoral students in biochemistry during the last three yearsof their PhD programmes. 
# The response variables the number of articles published during this period (art)
# Explanatory variables include:
# - gender of the student (fem=1 for women, 0=men)
# - student’s marital status (mar= 1 if married, 0 otherwise)
# - student’s number of children five years old or younger (kid5); 
# - a rating of the prestige of the student’sPhD department (phd); 
# - number of articles published by the student’s mentor during the three-yearperiod (ment)

# (a) Examine the distribution of the response variable. 
# Does least-squares linear regression appear a promising strategy for these data?

# (b) Perform a Poisson regression of number of articles published on the explanatory variables. 
# What conclusions would you draw from this analysis?

# (c) Consider the possibility of over-dispersion, either by fitting an over-dispersed Poisson model. 
# Is there evidence for over-dispersion? How, if at all, do the results change when over-dispersion is taken into account

long_data <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Long.txt", header=T)

# mean and variances are the most concerned for poisson distribution 

# over-dispersion: we have too much 0, inflated the data (for instance, ask people in the park whether they caught fish;
# but many 0 is generated because many people did not go fishing)
# over-dispersion test: compare mean and variances

# install.packages("AER")
library(AER)
library(pscl)

mean(long_data$art)
sd(long_data$art)^2



long_data <- within(long_data , {
  fem <- as.logical(fem)
  mar <- as.logical(mar)
})

summary(long_data)

with(long_data, 
     list(mean(art), var(art)))

hist(long_data$art)  # a lot of 0, so data-generation, is that becasue many of them may be 1st year with no publications? 


# OLS?
mod.lm <-lm(art ~ .,  data = long_data)

mod.lm2 <- lm(art ~ fem *.,  data = long_data) # interaction 

plot(predict(mod.lm2), abs(resid(mod.lm2)), xlab = "Predicted", ylab = "Absolute", data = long_data)

sresid <- rstandard(mod.lm2)



## The data is pretty skewed, and it's not normally distributed 






poisson_model <- glm(art ~., data = long_data, family = "poisson")
summary(poisson_model)
# our median is before our mean, so again, data is skewed 

# we get the coefficients of log odds 

# if you are female, married, with kids over 5, you are less likely to pubish

cfs <- coef(possion_model)
cfs

# predicted no. of articles for a married male PHd researcher with 1 child at 2-
# instittue whose phD supervisor pubished 4 articles 
# we need to intepret the coefficits by prediction becasue log odds of the coefficient 
# itself is not so meaningful cuz our outcome is a count, not probability 
# these coefficients do not make sense alone, they can only make sense when all terms are
# put together 

# when we put all coefficients toghether, we got the expected/predicted number of publications

pred <- data.frame(fem = FALSE,
                   ment = 5,
                   phd = 2,
                   mar = TRUE,
                   kid5 = 1)
predict(possion_model, newdata= pred, type = "response")

exp(cfs[1]+ cfs[2]*0 + cfs[3] + ....) # about to finish )

    
mod2.ps <- glm(art ~ fem *., data = long_data, family = "poisson")

## calculate pseudo R squared 
1- (possion_model$deviance/possion_model$null.deviance)
        

# RMSE
sqrt(mean(mod2.ps$model$art - mod2.ps$fitted.values)^2)
    
zero_inf <- dispersiontest(poisson_model)
zero_inf

# so it is zero-inflated, and we should use zero_inflated model, cuz we 
# should not use poisson on it, cuz it zero-inflated 
# our data violated our assumption of poisson regression


zero_infl_poisson <- zeroinfl(art ~ ., data= long_data, dist = "poisson")
summary(zero_infl_poisson)


## ment coef: -0.13 in zero-inflat
# the more publication your mentor have, you are less likely to be the part of generating zero-inflation 
# which means, the more publication your mentor have, you are less likely to publish 0, so you are more 
# likely to publish 


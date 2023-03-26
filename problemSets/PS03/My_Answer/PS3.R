library(MASS)
library(nnet)
library(ggplot2)
library(stargazer)
library(AER)
library(pscl)


data <- read.csv("C:/Users/Caesar/Documents/GitHub/StatsII_Spring2023/datasets/gdpChange.csv", header = TRUE)



# set the level for outcome variable 

data$GDPWdiff_reo <- ifelse(data$GDPWdiff == 0, "no change", ifelse(data$GDPWdiff < 0, "negative",  "positive")) 

data$GDPWdiff_reo <- as.factor(data$GDPWdiff_reo)

data$GDPWdiff_reo <- relevel(data$GDPWdiff_reo, ref = "no change")


# run unordered multinomial regression 

mult.log <- multinom(GDPWdiff_reo ~ REG + OIL, data = data)
summary(mult.log)
stargazer(mult.log, title="mult.log Results")


# run ordered multinomial regression 
data$GDPWdiff_reo <- factor(data$GDPWdiff_reo , levels = c("negative", "no change", "positive"))
ord.log <- polr(GDPWdiff_reo ~ REG + OIL, data = data, Hess = TRUE)
summary(ord.log)
stargazer(ord.log, title="ord.log Results")




## Q2:

# a)

data2 <- read.csv("C:/Users/Caesar/Documents/GitHub/StatsII_Spring2023/datasets/MexicoMuniData.csv", header = TRUE)


mod.ps <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, data = data2, family = "poisson")
result <- summary(mod.ps)
stargazer(mod.ps, title="mod.ps Results")

cfs <- coef(mod.ps)
cfs[2]   # coefficient for competitive.dsitrict 

se_cd <- result$coefficients[2, 2]
se_cd  # standard error of competitve.district

## I will conduct a Z test to test individual coefficient for the variable Competitive.district
## I set 95% as our confidence interval
## H0: B1 = 0 
## Ha: B0 not equals to 0 
## Z test score: 
z = (cfs[2]-0)/se_cd
z # -0.4766106

# two-tail, p-value
p = (1-pnorm(abs(z)))*2
p
# p value: 0.6336394, which is larger than 5%. Therefore, we cannot rejct the null hypothesis that
# the coefficient for the competitive.district equals to 0.



# b) 


# format: 
# for a one unit change in the predictor variable, the difference in the logs of
# expected counts is expected to change by the respective regression coefficient,
# given the other predictor variables in the model are held constant.



# interpretation of the coefficient for marginality.06: 
# Holding all other covariates constant, for a one unit change in marginality.06, 
# from the category of not-marginal to the category of marginal, the logs of 
# expected counts of visit is expected to decrease by 2.080. 



# interpretation of the coefficient for PAN.governor.06:  
# Holding all other covariates constant,for a one unit change in PAN.governor.06,
# from the category of not PAN.governor to the category of PAN.governor, the logs 
# of expected counts of visit is expected to decrease by 0.312. 


# c)


pred <- data.frame(competitive.district = 1,
                   marginality.06 = 0,
                   PAN.governor.06 = 1)
predict(mod.ps, newdata= pred, type = "response")

# 0.01494818 



exp(cfs[1] + cfs[2] * 1 + cfs[3] * 0 + cfs[4] *1)

##  0.01494818 

## In conclusion, the estimated mean number of visit is 0. 


data <- read.csv("C:/Users/Caesar/Documents/GitHub/StatsII_Spring2023/datasets/gdpChange.csv", header = TRUE)


library(MASS)
library(nnet)
library(ggplot2)
library(stargazer)
library(AER)
library(pscl)

# set the level for outcome variable 

data$GDPWdiff_reo <- ifelse(data$GDPWdiff == 0, "no change", ifelse(data$GDPWdiff < 0, "negative",  "positive")) 

data$GDPWdiff_reo <- as.factor(data$GDPWdiff_reo)

data$GDPWdiff_reo <- relevel(data$GDPWdiff_reo, ref = "no change")


# run multinomial regression 

mult.log <- multinom(GDPWdiff_reo ~ REG + OIL, data = data)
summary(mult.log)
stargazer(mult.log, title="mult.log Results")

ord.log <- polr(GDPWdiff_reo ~ REG + OIL, data = data, Hess = TRUE)
summary(ord.log)
stargazer(ord.log, title="ord.log Results")




## Q2:

# a)

data2 <- read.csv("C:/Users/Caesar/Documents/GitHub/StatsII_Spring2023/datasets/MexicoMuniData.csv", header = TRUE)


mod.ps <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, data = data2, family = "poisson")
summary(mod.ps)
stargazer(mod.ps, title="mod.ps Results")


# b) 



# c)


pred <- data.frame(competitive.district = 1,
                   marginality.06 = 0,
                   PAN.governor.06 = 1)
predict(mod.ps, newdata= pred, type = "response")

# 0.01494818 



cfs <- coef(mod.ps)
cfs

exp(cfs[1] * 1 + cfs[2] * 0 + cfs[3] *1)

## 0.002765929 

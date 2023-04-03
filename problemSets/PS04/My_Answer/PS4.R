
library(eha)
library(survival)
library(stargazer)

## Import Data 
data(infants)

## Coz-Harzard 
add_surv <- coxph(Surv(enter, exit, event) ~ age + sex, data = infants)
summary(add_surv)
stargazer(add_surv, title = "Infants Survivial")


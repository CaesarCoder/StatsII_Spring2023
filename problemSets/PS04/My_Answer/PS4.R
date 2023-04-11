
library(eha)
library(survival)
library(stargazer)

## Import Data 
data(child)

## Coz-Harzard 
add_surv <- coxph(Surv(enter, exit, event) ~ m.age + sex, data = child)
summary(add_surv)
stargazer(add_surv, title = "Child Survivial")


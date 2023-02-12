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

lapply(c("stringr"),  pkgTest)

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################
library("stargazer")
set.seed(123)

# create empirical distribution of observed data

data1 <- rcauchy(1000, location = 0, scale = 1)

ECDF <- ecdf(data1)
empiricalCDF <- ECDF(data1)
# generate test statistic
D <- max(abs(empiricalCDF - pnorm(data1)))
D
# test statistic D =  0.1347281

# p-value calculation: method 1: 
ks_test1 <- ks.test(data1, "pnorm")
ks_test1$p.value
# p vluae is very close to 0

# p-value calculation: method 2:
# create a randomly distributed data (size: 1000): 
data_norm <- rnorm(1000, mean=500, sd=5)

ks_test2 <- ks.test(data1, data_norm)
ks_test2$p.value
# p-value is 0 

# Get the ks.test() here: https://www.statology.org/kolmogorov-smirnov-test-r/ 

# p-value is roughly 0 by using both methods. Therefore, we can reject the hypothesis 
# that the empirical distribution does not match the queried theoretical distribution.

#####################
# Problem 2
#####################

set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

lm_test <- lm(y ~ x, data = data)
lm_test$coefficients  # roughly 2.73
stargazer(lm_test, title="linear regression Results, method 1")


linear.lik <- function(theta, y, X){
  n <- nrow(X) 
  k <- ncol(X)  
  beta <- theta[1 : k]  
  sigma2 <- theta[k + 1]^2  
  e <- y - X%*%beta
  logl <- -.5*n*log(2*pi) -.5*n*log(sigma2) - ((t(e) %*%                                                  e ) / ( 2 * sigma2 ) )
  return(-logl)}

linear.MLE <- optim(fn=linear.lik, par=c(1, 1, 1), hessian =TRUE, y =data$y, X= cbind (1 ,data$x), method = "BFGS")
linear.MLE$par # coefficient is roughly equal to 2.73

# the coefficients for X in both models are roughly equal to 2.73

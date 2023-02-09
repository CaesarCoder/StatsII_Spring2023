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

set.seed(123)


# create empirical distribution of observed data

data1 <- rcauchy(1000, location = 0, scale = 1)

ECDF <- ecdf(data1)
empiricalCDF <- ECDF(data1)
# generate test statistic
D <- max(abs(empiricalCDF - pnorm(data1)))
D
# test statistic D =  0.1347281

# create a randomly distributed data (size: 1000): 
data_norm <- rnorm(1000, mean=500, sd=5)

# p-value 
ks_test1 <- ks.test(data1, "pnorm")
PV1 <- ks_test1$p.value
PV1

ks_test2 <- ks.test(data1, data_norm)
PV2 <- ks_test2$p.value
PV2

# Get the ks.test() here: https://www.statology.org/kolmogorov-smirnov-test-r/ 

# p-value is 0.554, which is larger than 0.05. we cannot reject the hypothesis 
# that the empirical distribution matches the queried theoretical distribution

#####################
# Problem 2
#####################

set.seed (123)
data <- data.frame(x = runif(200, 1, 10))
data$y <- 0 + 2.75*data$x + rnorm(200, 0, 1.5)

lm_test <- lm(y ~ x, data = data)
lm_test$coefficients

linear.lik <- function(theta, y, X){
  n <- nrow(X) 
  k <- ncol(X)  
  beta <- theta[1 : k]  
  sigma2 <- theta[k + 1]^2  
  e <- y - X%*%beta
  logl <- -.5*n*log(2*pi) -.5*n*log(sigma2) - ((t(e) %*%                                                  e ) / ( 2 * sigma2 ) )
  return(-logl)}

linear.MLE <- optim(fn=linear.lik, par=c(1, 1, 1), hessian =TRUE, y =data$y, X= cbind (1 ,data$x), method = "BFGS")
linear.MLE$par

# the coefficients for X in both models are roughly equal to 2.727

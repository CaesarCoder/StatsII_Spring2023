#######################
# Stats 2: tutorial 4 #
#######################

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

## More on logits: visualising and goodness of fit

graduation <- read.table("http://statmath.wu.ac.at/courses/StatsWithR/Powers.txt",
                         stringsAsFactors = TRUE)
graduation <- graduation[-which(graduation$nsibs<0),]

# 1. This time, let's analyse the data in more detail. Run some checks to see if 
#    the data are well distributed. Try some simple plots to get an idea of the 
#    relationship between variables. Drop those errors too.
library(ggplot2)

graduation$hsgrad <- ifelse(graduation$hsgrad == "yes", 1, 0) 

##ggplot(aes(income, hsgrad, group = nonwhite), data = graduation)## check the solution,
## not a good plot cuz it did not give much information

plot(graduation$hsgrad ~ graduation$income)

plot(graduation$nonwhite ~ graduation$income)

plot(graduation$hsgrad ~ graduation$income)

plot(graduation$hsgrad ~ graduation$nonwhite)

plot(graduation$income~graduation$nonwhite)

# 2. Last week we created a kitchen sink model, with nsibs as a binned factor. 
#    Here was the code:
graduation$nsibs_cut <- cut(graduation$nsibs, 
                            breaks = c(0, 0.9, 1, 3, Inf), 
                            include.lowest = TRUE,
                            labels = c("None", "One", "Two_Three", "FourPlus"))

mod_1 <- glm(hsgrad ~., 
             data = graduation[,!names(graduation) %in% c("nsibs")], 
             family = "binomial")
summary(mod_1)

# kitchen sink, throw everything in model  

# Create a more parsimonious model of your own choice. Select three predictor 
# variables, run the regression, and check with summary.

mod_2 <- glm(hsgrad ~ nonwhite*income, family = "binomial", data = graduation)
summary(mod_2)
  #your model

# 3. a) Create a new data frame comprising the outcome variable and two columns 
#       of fitted values, one from mod_1 and another from mod_2. 
hsgrad <- graduation$hsgrad
fv1 <- mod_1$fitted.values
fv2 <-mod_2$fitted.values

predicted_data <- data.frame(hsgrad, fv1, fv2)

# 3. b) Create a pipe (without reassigning) whereby you reorder your new 
#       dataframe according to the fitted values of mod_1, create a new rank 
#       variable, then create a scatterplot of rank by fitted value, 
#       colored by the outcome variable.
library(tidyverse)



# null model: for linear model, how better is our line(slope) compared to a parallel line of mean
# null model: similar 

predicted_data %>%
  arrange(fv1) %>%
  mutate(rank = row_number()) %>%
  ggplot(aes(rank, fv1)) + geom_point(aes(colour = hsgrad), alpha = 0.5) + scale_y_continuous(limits = c(0,1))


predicted_data %>%
  arrange(fv2) %>%
  mutate(rank = row_number()) %>%
  ggplot(aes(rank, fv2)) + geom_point(aes(colour = hsgrad), alpha = 0.5) + scale_y_continuous(limits = c(0,1))

# 3. c) Do the same for mod_2. Compare the results.

predicted_data %>%
  arrange(fv2) %>%
  mutate(rank = row_number()) %>%
  ggplot(aes(rank, fv2)) + geom_point(aes(colour = hsgrad), alpha = 0.5) + scale_y_continuous(limits = c(0,1))



# 4. Calculate McFadden's Pseudo R squared for both models. 
#    Which model explains more variance?
#    What are the p values?

# McFadden's Pseudo R squared 
# raito of two likelihood, a ratio of likelihood bewteen null model and your own model
# fomula: 1- (likelihood of our model/likelihood of null model) 
# the larger the value is, the more our model expalin 

with(summary(mod_1), 1 - deviance/null.deviance) 

with(summary(mod_2), 1 - deviance/null.deviance) 



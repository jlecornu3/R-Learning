## HT, Wk3, Exercise Sheet 2 - Poisson Regression Question 5, Applied Wooldridge Example

library(lmtest)
library(wooldridge)
library(sandwich)

names(bwght)

## Part a.) Binary Variable, estimate probit regression ####
View(bwght)
bwght$smokes <- ifelse(bwght$male == 0 & bwght$cigs > 0, 1, 0)

## Now fit a Probit Model
smokes_model = smokes ~ motheduc + white + lfaminc

probit <- glm(smokes_model, 
              family = binomial(link = 'logit'), 
              data = bwght)

# Summarize Results
coeftest(probit)
coeftest(probit, vcoc. = vcovHC)

# Part b.) Estimate some sample differences ####

## Part c.) Calculate some Average Partial effects ####

## Part d.) Calculate Pseudo R^2 ####

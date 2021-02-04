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
summary(probit)
confint(probit)
coefs <- summary(probit)$coefficients

# Part b.) Estimate some sample differences ####
coefs[1]
inc_mean = mean(bwght$lfaminc)

alice <- dnorm(coefs[1] + coefs[2]*12 + coefs[3]*1 + coefs[4]*inc_mean)
beth <- dnorm(coefs[1] + coefs[2]*16 + coefs[3]*1 + coefs[4]*inc_mean)

## Part c.) Calculate some Average Partial effects ####
ape_lfaminc <- 

## Part d.) Calculate Pseudo R^2 ####
PseudoR2(probit, which = NULL)
blr_rsq_mcfadden(probit)
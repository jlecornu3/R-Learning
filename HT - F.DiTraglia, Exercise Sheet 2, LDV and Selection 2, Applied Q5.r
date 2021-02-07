## HT, Wk3, Exercise Sheet 2 - Poisson Regression Question 5, Applied Wooldridge Example

library(lmtest)
library(wooldridge)
library(sandwich)

names(bwght)

## Part a.) Binary Variable, estimate probit regression ####
View(bwght)
bwght$smokes <- ifelse(bwght$male == 0 & bwght$cigs > 0, 1, 0)

# Check vs Solution Method
bwght$smokes_2 <- ifelse(bwght$cigs > 0, yes = 1, no = 0)
all(bwght$smokes == bwght$smokes_2)

## Now fit a Probit Model
smokes_model = smokes ~ motheduc + white + lfaminc

probit <- glm(smokes_model, 
              family = binomial(link = 'probit'), 
              data = bwght)

# Summarize Results
coeftest(probit)
coeftest(probit, vcoc. = vcovHC)
summary(probit)
confint(probit)
coefs <- summary(probit)$coefficients

# Part b.) Estimate some sample differences ####
coefs[1]
linc_mean = mean(bwght$lfaminc)
inc_mean = mean(bwght$faminc)

alice <- dnorm(coefs[1] + coefs[2]*12 + coefs[3]*1 + coefs[4]*inc_mean)
beth <- dnorm(coefs[1] + coefs[2]*16 + coefs[3]*1 + coefs[4]*inc_mean)

Alice <-  c(motheduc = 12, white = 1, lfaminc = log(inc_mean))
Beth <-  c(motheduc = 16, white = 1, lfaminc = log(inc_mean))
predict_me <- data.frame(rbind(Alice,Beth))
predictions <-  predict(probit, newdata = predict_me, type = 'response')
predictions
diff(predictions)

## Part c.) Calculate some Average Partial effects ####
ape_lfaminc <- mean(dnorm(predict(probit)))*coef(probit)[4]
ape_lfaminc

## Part d.) Calculate Pseudo R^2 ####
# Fit null model
model_null <- smokes ~ 1
probit_null <- glm(model_null, family = binomial(link = 'probit'), data = bwght)

PseudoR2 <- 1- logLik(probit)/logLik(probit_null)
PseudoR2

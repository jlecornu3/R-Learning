## HT, Wk 2, L3 - Econometrics Binary Choice Models
#   Applied Lecture on R Implementation of Binary Choice Models


# install.packages("wooldridge")
# install.packages("lmtest")
# install.packages("sandwich")

library(wooldridge)
library(lmtest)
library(sandwich)

# Build the model with regressors
names(fertil2)
fertility_model <- children ~ educ + age + agesq + evermarr + urban + electric + tv

# Three versions of models
pois_reg <- glm(fertility_model, 
                family = poisson(link = 'log'), 
                data = fertil2)

quasipois <- glm(fertility_model, 
                family = quasipoisson(link = 'log'), 
                data = fertil2)


ols <- lm(fertility_model, 
          data = fertil2)

# Now do the coefficient tests
coeftest(pois_reg)
coeftest(quasipois)
coeftest(ols)
coeftest(ols, vcov. = vcovHC, type = 'HC0')

# Extract estimate of sigma squared
summary(quasipois)$dispersion
# Displays underdispersion < 1

# Do this By Hand: --> Ensure residuals on the scale of y
yhat <- predict(pois_reg, type = 'response')
uhat <- residuals(pois_reg, type = 'response')
mean(uhat^2 / yhat)

# Small Difference comes from sample mean of dividing by n, R doing work adjusts for DoF

# Lets do the robust sandwich Ses for poisson regression
coeftest(pois_reg, vcov. = vcovHC, type = 'HC0')

# Compare OLS and Poisson - square bracket removes the intercept,first element which we don't want to show
ybar <- mean(fertil2$children)
OLS_est <- coefficients(ols)[-1]
pois_est <- coefficients(pois_reg)[-1]

# Bind some columns together into a table
cbind(OLS = OLS_est, Poisson_APE = ybar*pois_est, Poisson = pois_est)
# Notice how APE Poisson a lot closer to OLS (some sort of average effect as its linear)
# Poisson is not an average effect, but is when we scale by y_bar

# Note how age and agesquared are not partial effects...


###Binary Outcome model Example 15.1 from Woolridge 
# Robust SE packages
library(lmtest)
library(sandwich)

# Load data from wooldridge
library(wooldridge)

names(mroz)

# Female 1975 Labuor market model
labour_model <- inlf ~ nwifeinc + educ + exper + expersq + age + kidslt6 + kidsge6

# Fit 3 Models
lpm <- lm(labour_model, data = mroz)
logit <- glm(labour_model, 
             family = binomial(link = 'logit'), 
             data = mroz)

probit <- glm(labour_model, 
             family = binomial(link = 'probit'), 
             data = mroz)

# Test Results
coeftest(lpm, vcov. = vcovHC, type = 'HC0')
coeftest(logit)
coeftest(probit)

# Calculating Pseudo R2

# Fit Models with Intercept Only
model0 <- inlf ~ 1
logit0 <- glm(model0, 
              family = binomial(link='logit'), 
              data = mroz)

probit0 <- glm(model0, 
              family = binomial(link='probit'), 
              data = mroz)

# Calculate Pseduo as 1 - ratio of log likelihoods
1 - logLik(logit)/logLik(logit0)
1 - logLik(probit)/logLik(probit0)

# Calculating Average Partial Effects scaling factors
# Average of g(x' beta_hat) where g is the desired density function

logit_APE_factor <- mean(dlogis(predict(logit)))
logit_APE_factor

probit_APE_factor <- mean(dnorm(predict(probit)))
probit_APE_factor

# Now we collated APEs across models
lpm_est <-  coefficients(lpm)[-1]
logit_est <-  coefficients(logit)[-1]
probit_est <-  coefficients(probit)[-1]

cbind(lpm = lpm_est, logit_APE = logit_APE_factor * logit_est, probit_APE = probit_APE_factor * probit_est)








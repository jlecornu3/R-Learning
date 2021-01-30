## HT, Wk2, Exercise Sheet 1 - Poisson Regression Question 4, Applied Wooldridge Example

library(lmtest)
library(wooldridge)
library(sandwich)

## Part a.) Linear Regression
names(smoke)
cig_model <- cigs ~ lcigpric + lincome + restaurn + white + educ + age + agesq 
ols <- lm(cig_model,
          data = smoke)

# Interpretations
coeftest(ols)

# Cigarette prices - B^ = -0.85, T = -0.14 --> Insignificant
# Income - B^ = 0.87, T = 1.19 --> Insignificant

# Impact of robust Ses - 
coeftest(ols, vcov. = vcovHC, type = 'HC0')

# Cigarette prices - B^ = -0.85, T = -0.14 --> Insignificant
# Income - B^ = 0.87, T = 1.46 --> Insignificant


## Part b.) Repeat with a Poisson regression, exponential conditional mean rather than lm
pois_reg <- glm(cig_model, 
                family = poisson(link = 'log'),
                data = smoke)

coeftest(pois_reg)
# Cigarette prices - B^ = -0.11, P-val = 0.46
# Income - B^ = -0.12, P-val = 0 --> Significant now! 


# Calculate APE and Compare to OLS
ybar <- mean(smoke$cigs)
OLS_est <- coefficients(ols)[-1]
pois_reg_est <- coefficients(pois_reg)[-1]

# Bind some columns together into a table
cbind(OLS = OLS_est, Poisson_APE = ybar*pois_reg_est, Poisson = pois_reg_est)

## Part c.) Interpret again with sandwich form and compare

coeftest(pois_reg, vcov. = vcovHC, type = 'HC0')
# Cigarette prices - B^ = -0.11, P-val = 0.87
# Income - B^ = -0.10, P-val = 0.2 --> Back to Insignificant now! 
pois_se <- coef(summary(pois_reg))[, 2][-1]
ols_se <- coef(summary(ols))[, 2][-1]

# Bind some columns together into a table
cbind(OLS = OLS_est, OLS_SE = ols_se, Poisson = pois_reg_est, pois_reg_se = pois_se, Poisson_APE = ybar*pois_reg_est)


## Part d.) Calculate sigma_hat_2
summary(pois_reg)$dispersion
# Does not display underdispersion = 1!

# Impact of Quasi poisson variance assumption?
quais_pois_reg <- glm(cig_model, 
                family = quasipoisson(link = 'log'),
                data = smoke)

summary(quasi_pois_reg)$dispersion
# Displays overdispersion > 1!

# Compare Results:
quasi_pois_reg_est <- coefficients(quasi_pois_reg)[-1]

# Bind some columns together into a table
cbind(Poisson_APE = ybar*pois_reg_est, Quasi_Poisson_APE = ybar*quasi_pois_reg_est)

coeftest(quais_pois_reg)
coeftest(quasi_pois_reg, vcov. = vcovHC, type = 'HC0')

coeftest(quais_pois_reg)
coeftest(quasi_pois_reg, vcov. = vcovHC, type = 'HC0')


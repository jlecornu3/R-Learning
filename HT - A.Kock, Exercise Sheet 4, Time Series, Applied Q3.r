### HT, Wk5, Exercise Sheet 4 - Time Series Question 3

# Load Packages
require(stargazer)
require("readxl")
require("tidyverse")
require(readr)
require(Hmisc)
require(ggplot2)
require("gridExtra")
require("dplyr")
require(broom)
require(readxl)
require(lubridate)
require(gridExtra)
require(scales)

# Change Directory
setwd("C:/Users/jleco/OneDrive - Nexus365/Oxford MPhil/Metrics/02. Exercise Sheets")
wd <- "C:/Users/jleco/OneDrive - Nexus365/Oxford MPhil/Metrics/02. Exercise Sheets"

# Read ONS Quarterly Accounts Data
nqa <- read_excel("Data/ONS_NationalQuarterlyAccounts.xlsx")
View(nqa)

# Keep Columns IT and YU from Excel

# Question 3a.) - what is variable AMBI2014_3 measuring? Units? Real? Nominal?
# Quarterly National Account
# Release 	23 Dec 2014=
# Downloaded	16 Jan 2015
# ABMI
# Gross Domestic Product: chained volume measures: Seasonally adjusted £m						
# seasonal_adjustment='SA'						
# base_period='2011'						
# price='CONS'						
# index_period

# Question 3b.) Let y = log(Y) where Y is real gdp
# Form the real growth rate (first difference)
nqa$row_num <- seq.int(nrow(nqa)) 
quarters <- ts(nqa$row_num, start = c(1955, 1), frequency = 4)
nqa$period <- ts(nqa$row_num, start = c(1955, 1), frequency = 4)

#Drop Missings
nqa_clean = nqa[!nqa$period >= 244,]
nqa_clean$gdp <- nqa_clean$ABMI2015_3
nqa_clean$log_gdp <- log(as.numeric(nqa_clean$ABMI2015_3))
nqa_clean$lag_log_gdp <- Lag(nqa_clean$log_gdp)
nqa_clean$real_growth_rate <- (nqa_clean$log_gdp - nqa_clean$lag_log_gdp)
nqa_clean$lag_real_growth_rate <- Lag(nqa_clean$real_growth_rate)

view(nqa_clean)

# Question 3c.) Plot the first difference and discuss observations.
first_diff_plot <- ggplot(nqa_clean, aes(x=period, y=real_growth_rate)) +
  geom_line(colour = "black", size = 0.5) +
  geom_hline(yintercept = 0, color = "red") +
    labs(title = "First difference of quarterly log GDP (Real growth rate)",
       subtitle = "ONS National Quarterly Accounts, 1955Q1 - 2015Q3",
       tag = "Question 1c.",
       x= "Time",
       y="Real Growth Rate (First different of log price)"
  )

first_diff_plot
ggsave("Plots/First Difference Plot.png")

# Question 3d.) Fit an AR(1) model to first difference for 
# i.) Full Sample
# ii.) 1995 Q3 - 2005 Q3

# Create Sample Splits
nqa_clean_sub = nqa_clean[nqa_clean$period >= 163 & nqa_clean$period <= 203,]

# Fit Models
ar1_realgrowth <- real_growth_rate ~ lag_real_growth_rate + 1

full_sample <- lm(ar1_realgrowth, data = nqa_clean)
sub_sample <- lm(ar1_realgrowth, data = nqa_clean_sub)

summary(full_sample)
summary(sub_sample)

stargazer(full_sample,sub_sample, align=TRUE)

# Compute Long run mean
# Full Sample
mu_full <- summary(full_sample)$coefficients[1,1]
alpha_full <- summary(full_sample)$coefficients[2,1]
long_run_full <- (mu_full)/(1-alpha_full)
long_run_full

# Sub Sample
mu_sub <- summary(sub_sample)$coefficients[1,1]
alpha_sub <- summary(sub_sample)$coefficients[2,1]
long_run_sub <- (mu_sub)/(1-alpha_sub)
long_run_sub

# Compute long run standard deviation
sigma_full <- summary(full_sample)$sigma
LR_Var_full <- (sigma_full^2)/(1-(alpha_full)^2)
LR_StdV_full <- LR_Var_full^0.5
LR_StdV_full

sigma_sub <- summary(sub_sample)$sigma
LR_Var_sub <- (sigma_sub^2)/(1-(alpha_sub)^2)
LR_StdV_sub <- LR_Var_sub^0.5
LR_StdV_sub

# Is it a good model?

# Question 3e.) Fit an AR(1) model to yt including a liner trend for
# a.) Full Sample
# b.) 1995 Q3 - 2005 Q3

# i.) Compute long run growth rate and interpret. Link this to part d.
# ii.) Is it a good model?



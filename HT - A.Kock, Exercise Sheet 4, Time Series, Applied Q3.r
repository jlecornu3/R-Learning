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
install.packages("cowplot")
install.packages('forecast', dependencies = TRUE)
require(forecast)

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

# Run the Regression Specifically
nqa_clean <- augment(full_sample)
nqa_clean_sub <- augment(sub_sample)

# Create Residual Plot
ggplot(nqa_clean, aes(x = .rownames, y = .resid)) + 
  geom_point() +
  geom_segment(aes(xend = .rownames, yend = 0))
  geom_hline(yintercept = 0, color = "blue")
)

ggsave("Plots/Residiual Plot for Real Growth Model.png")

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
# b.) 1995 Q3 - 2005 Q3
ar1_loggdp <- log_gdp ~ lag_log_gdp + period + 1
ar1_loggdp_m <- lm(ar1_loggdp, data = nqa_clean_sub)
summary(ar1_loggdp_m)
loggdp_res = resid(ar1_loggdp_m)

stargazer(ar1_loggdp_m, align=TRUE)


# i.) Compute long run growth rate and interpret. Link this to part d.
lag <- summary(ar1_loggdp_m)$coefficients[2,1]
trend <- summary(ar1_loggdp_m)$coefficients[3,1]
lr_growth <- trend/(1-lag)
lr_growth


# ii.) Is it a good model?
Acf(nqa_clean$log_gdp)

conf.level <- 0.95
ciline <- qnorm((1 - conf.level)/2)/sqrt(length(x))
bacf <- acf(nqa_clean$log_gdp, plot = FALSE)
bacfdf <- with(bacf, data.frame(lag, acf))

acf <- ggplot(data=bacfdf, mapping=aes(x=lag, y=acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))

## ACF Template from Online
ggplot.corr <- function(data, lag.max = 24, ci = 0.95, large.sample.size = TRUE, horizontal = TRUE,...) {
  
  require(ggplot2)
  require(dplyr)
  require(cowplot)
  
  if(horizontal == TRUE) {numofrow <- 1} else {numofrow <- 2}
  
  list.acf <- acf(data, lag.max = lag.max, type = "correlation", plot = FALSE)
  N <- as.numeric(list.acf$n.used)
  df1 <- data.frame(lag = list.acf$lag, acf = list.acf$acf)
  df1$lag.acf <- dplyr::lag(df1$acf, default = 0)
  df1$lag.acf[2] <- 0
  df1$lag.acf.cumsum <- cumsum((df1$lag.acf)^2)
  df1$acfstd <- sqrt(1/N * (1 + 2 * df1$lag.acf.cumsum))
  df1$acfstd[1] <- 0
  df1 <- select(df1, lag, acf, acfstd)
  
  list.pacf <- acf(data, lag.max = lag.max, type = "partial", plot = FALSE)
  df2 <- data.frame(lag = list.pacf$lag,pacf = list.pacf$acf)
  df2$pacfstd <- sqrt(1/N)
  
  if(large.sample.size == TRUE) {
    plot.acf <- ggplot(data = df1, aes( x = lag, y = acf)) +
      geom_area(aes(x = lag, y = qnorm((1+ci)/2)*acfstd), fill = "#B9CFE7") +
      geom_area(aes(x = lag, y = -qnorm((1+ci)/2)*acfstd), fill = "#B9CFE7") +
      geom_col(fill = "#4373B6", width = 0.7) +
      scale_x_continuous(breaks = seq(0,max(df1$lag),6)) +
      scale_y_continuous(name = element_blank(), 
                         limits = c(min(df1$acf,df2$pacf),1)) +
      ggtitle("ACF") +
      theme_bw()
    
    plot.pacf <- ggplot(data = df2, aes(x = lag, y = pacf)) +
      geom_area(aes(x = lag, y = qnorm((1+ci)/2)*pacfstd), fill = "#B9CFE7") +
      geom_area(aes(x = lag, y = -qnorm((1+ci)/2)*pacfstd), fill = "#B9CFE7") +
      geom_col(fill = "#4373B6", width = 0.7) +
      scale_x_continuous(breaks = seq(0,max(df2$lag, na.rm = TRUE),6)) +
      scale_y_continuous(name = element_blank(),
                         limits = c(min(df1$acf,df2$pacf),1)) +
      ggtitle("PACF") +
      theme_bw()
  }
  else {
    plot.acf <- ggplot(data = df1, aes( x = lag, y = acf)) +
      geom_col(fill = "#4373B6", width = 0.7) +
      geom_hline(yintercept = qnorm((1+ci)/2)/sqrt(N), 
                 colour = "sandybrown",
                 linetype = "dashed") + 
      geom_hline(yintercept = - qnorm((1+ci)/2)/sqrt(N), 
                 colour = "sandybrown",
                 linetype = "dashed") + 
      scale_x_continuous(breaks = seq(0,max(df1$lag),6)) +
      scale_y_continuous(name = element_blank(), 
                         limits = c(min(df1$acf,df2$pacf),1)) +
      ggtitle("ACF") +
      theme_bw()
    
    plot.pacf <- ggplot(data = df2, aes(x = lag, y = pacf)) +
      geom_col(fill = "#4373B6", width = 0.7) +
      geom_hline(yintercept = qnorm((1+ci)/2)/sqrt(N), 
                 colour = "sandybrown",
                 linetype = "dashed") + 
      geom_hline(yintercept = - qnorm((1+ci)/2)/sqrt(N), 
                 colour = "sandybrown",
                 linetype = "dashed") + 
      scale_x_continuous(breaks = seq(0,max(df2$lag, na.rm = TRUE),6)) +
      scale_y_continuous(name = element_blank(),
                         limits = c(min(df1$acf,df2$pacf),1)) +
      ggtitle("PACF") +
      theme_bw()
  }
  cowplot::plot_grid(plot.acf, plot.pacf, nrow = numofrow)
}

ggplot.corr(data = nqa_clean$log_gdp, lag.max = 24, ci= 0.95, large.sample.size = FALSE, horizontal = TRUE) + 
  ggsave("Plots/GDP ACF and PACF.png")

ggplot.corr(data = loggdp_res, lag.max = 24, ci= 0.95, large.sample.size = FALSE, horizontal = TRUE) + 
  ggsave("Plots/GDP Residuals ACF and PACF.png")



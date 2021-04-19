# ## HT, Wk9, Exercise Sheet 5 - Time Series Question 1
# Load Excel reading package

# Load Data
install.packages("readxl")
install.packages("tidyverse")
install.packages("Hmisc")
install.packages("stargazer")
require(stargazer)
require("readxl")
require("tidyverse")
require(readr)
require(Hmisc)
require(ggplot2)
require("gridExtra")
require("dplyr")
require(broom)

# Change Directory
setwd("C:/Users/jleco/OneDrive - Nexus365/Oxford MPhil/Metrics/02. Exercise Sheets")

# Read Fulton Fish Data
fulton <- read_table2("Data/Fulton.txt")
View(fulton)

# Construct a lagged price variable and differenced price and quantity variables
names(fulton)
inds <-  seq(as.Date("1991-12-02"), as.Date("1992-03-21"),by="day")
fulton$time_period <- ts(inds, 
                         start=as.numeric(format(inds[1],"%j")),
                         frequency=365)

fulton$lag_price <- Lag(fulton$LogPrice, +1)
fulton$lag_quantity <- Lag(fulton$LogQuantity, +1)

# Question 1c.)
fulton$diff_price <- fulton$LogPrice - fulton$lag_price
fulton$diff_quantity <- fulton$LogQuantity - fulton$lag_quantity

# Question 1b.)
levels_m <- LogQuantity ~ LogPrice + lag_quantity + lag_price + 1
levels_lm <- lm(levels_m, data=fulton)

# Question 1c.)
diffs_m <- diff_quantity ~ diff_price + lag_quantity + lag_price + 1
diffs_lm <- lm(diffs_m, data=fulton)


# Export Table with StarGazer
stargazer(levels_lm,diffs_lm, align=TRUE)
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

# Change Directory
setwd("C:/Users/jleco/OneDrive - Nexus365/Oxford MPhil/Metrics/02. Exercise Sheets")

# Read ONS Quarterly Accounts Data
nqa <- read_table2("ONS.txt")
View(nqa)

# Question 3a.) - what is variable AMBI2014_3 measuring? Units? Real? Nominal?

# Question 3b.) Let y = log(Y) where Y is real gdp
# Form the real growth rate (first difference)

# Question 3c.) Plot the first difference and discuss observations.

# Question 3d.) Fit an AR(1) model to first difference for 
# i.) Full Sample
# ii.) 1995 Q3 - 2005 Q3

# Compute Long run mean

# Compute long run standard deviation

# Is it a good model?

# Question 3e.) Fit an AR(1) model to yt including a liner trend for
# a.) Full Sample
# b.) 1995 Q3 - 2005 Q3

# i.) Compute long run growth rate and interpret. Link this to part d.
# ii.) Is it a good model?



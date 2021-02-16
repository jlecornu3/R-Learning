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

# Change Directory
setwd("C:/Users/jleco/OneDrive - Nexus365/Oxford MPhil/Metrics/02. Exercise Sheets")

# Read ONS Quarterly Accounts Data
nqa <- read_excel("ONS_NationalQuarterlyAccounts.xlsx")
View(nqa)

# Keep Columns IT and YU from Excel

# Question 3a.) - what is variable AMBI2014_3 measuring? Units? Real? Nominal?
# Quarterly National Account
# Release 	23 Dec 2014
# Downloaded	16 Jan 2015
# ABMI
# Gross Domestic Product: chained volume measures: Seasonally adjusted £m						
# seasonal_adjustment='SA'						
# base_period='2011'						
# price='CONS'						
# index_period

# Question 3a.) - what is variable AMBI2014_3 measuring? Units? Real? Nominal?
describe(nqa$AMBI2014_3)
summary(nqa$AMBI2014_3)

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



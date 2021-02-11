# ## HT, Wk3, Exercise Sheet 3 - Time Series Question 3

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
fulton <- read_table2("Fulton.txt")
View(fulton)

# Question 1a.) - construct a lagged price variable
names(fulton)
inds <-  seq(as.Date("1991-12-02"), as.Date("1992-03-21"),by="day")
fulton$time_period <- ts(inds, 
                         start=as.numeric(format(inds[1],"%j")),
                         frequency=365)

fulton$lag_price <- Lag(fulton$LogPrice, +1)

# Plot it
colors <- c("Lagged Price" = "red", "Log Price" = "black")
price_and_lag <- ggplot(fulton, aes(x=time_period)) +
  geom_line(aes(y = lag_price, color = "Lagged Price"), size = 1.25) + 
  geom_line(aes(y = LogPrice, color = "Log Price"),  size = 1, linetype = "dashed") + 
  labs( x= "Time",
        y = "Price (Logs)",
        color = "Legend",
        tag = "Question 1a."
          ) +
  scale_color_manual(values = colors) +
  theme(axis.text.x=element_blank())

  price_and_lag

  ggsave("Price and Lag Time Series.png")
  
  
# Question 1b.) Scatter plot of LogPrice and Lagged Log Price
scatter <-  ggplot(fulton, aes(x=lag_price, y=LogPrice)) +
  geom_point(colour = "black", size = 2.5) + 
  labs(title = "Scatter plot of LogPrice and lagged LogPrice",
       subtitle = "Fulton fish data Dec 1991 - Mar 1992",
       tag = "Question 1b.",
       x= "One period lagged log price",
       y="Log Price"
       )

scatter
ggsave("Price and Lag Scatter.png")

# Question 1c.) Scatter plot of LogPrice and Lagged Log Price with regression line
scatter_fit <-  ggplot(fulton, aes(x=lag_price, y=LogPrice)) +
  geom_point(colour = "black", size = 2.5) + 
  geom_smooth(method=lm) +
  labs(title = "Scatter plot of LogPrice and lagged LogPrice - With linear fit",
       subtitle = "Fulton fish data Dec 1991 - Mar 1992",
       tag = "Question 1c.",
       x= "One period lagged log price",
       y="Log Price"
  )

scatter_fit
ggsave("Price and Lag Scatter with Fit.png")

# Run the Regression Specifically
AR_1 <- LogPrice ~ lag_price
lm <- lm(AR_1, data=fulton)
fulton <- augment(lm)

# Create Residual Plot
ggplot(fulton, aes(x = .fitted, y = .resid)) + 
  geom_point() +
  geom_point(aes(y=0), shape = 1) +
  geom_segment(aes(xend = .fitted, yend = 0)) +
  geom_hline(yintercept = 0, color = "blue") +
  labs(title = "Residual Plot") +
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        )

  
  
ggsave("Residuals plot.png")


## Now lets look at quantity and price together
price_lm <- LogQuantity ~ LogPrice + 1
quantity_lm <- LogPrice ~ LogQuantity + 1

price_model <- lm(price_lm, data = fulton)
quantity_model <- lm(quantity_lm, data = fulton)

# Export Table with StarGazer
stargazer(quantity_model,price_model, align=TRUE)
summary(price_model)


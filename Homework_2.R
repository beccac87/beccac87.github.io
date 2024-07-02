#Homework 2

#Making sure my packages are installed!
library(tidyverse)
library(tidyquant)
library(ggplot2)

#First, Use the tq_get function from the tidyquant package to download stock prices data 
#for the Disney (DIS) from January 1, 2020 to January 1, 2022.
DIS_prices <- tq_get("DIS",
                     get = "stock.prices",
                     from = "2020-01-01",
                     to = "2022-01-01")

#Check and make sure it is loaded
DIS_prices

#Next, you create a simple dot plot of Disney’s adjusted close prices over the period
DIS_price_points <- ggplot(data = DIS_prices,
                      aes(x= date, y = close)) +
  geom_point() +
  labs(x = "time", y = "adjusted close price", title = "Disney adjusted close prices 2020-2021") +
  theme_bw()

DIS_price_points

#Create a histogram of the distribution of Disney’s adjusted close prices
DIS_price_hist <- ggplot(data = DIS_prices, aes(x= adjusted)) +
  geom_histogram(binwidth = 2) +
  labs(x = "adjusted close price", y = "count", title = "Histogram of Disney adjusted close prices, 2020-2021") +
  theme_bw()

DIS_price_hist

#You decide to create a boxplot of Disney’s adjusted close prices, with one boxplot for each month, like we did in class.
#However, you think that 24 months is too long of a period for one figure like this.
#So, you limit your data to January to September 2020. Additionally, you realize you must create a month variable.
DIS_prices_short <- DIS_prices %>%
  filter(date < "2020-10-01") %>%
  mutate(DIS_month = month(date))

DIS_prices_short

#Boxplot
DIS_boxplot_month <- ggplot(data = DIS_prices_short,
                      aes(x=close, y = as.character(DIS_month))) +
  geom_boxplot() +
  labs(x = "daily adjusted close price", y = "month", title = "Monthly boxplots of daily adjusted close prices for Dinsey", subtitle="January to September 2020") +
  scale_y_discrete(labels = c("1" = "Jan", "2" = "Feb", "3" = "March", "4" = "April", "5" = "May", "6" = "June", "7" = "July", "8" = "Aug", "9" = "Sept")) +
  theme_bw() +
  coord_flip()

DIS_boxplot_month

#Bonus! Save your boxplot from question 5 as a pdf with a height of 3” and a width of 6”
ggsave(filename = "Desktop/DIS_boxplot_month.pdf",
       plot = DIS_boxplot_month,
       height = 3,
       width = 6,
       units = c("in"))

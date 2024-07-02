#Homework 1

#Making sure my packages are installed!
library(tidyverse)
library(tidyquant)

#Use the tq_get function from the tidyquant package to download stock prices data 
#for the Technology Select Sector SPDR Fund (XLK) from January 1, 2015 to January 1, 2022.
XLK_prices <- tq_get("XLK",
                     get = "stock.prices",
                     from = "2015-01-01",
                     to = "2022-01-01")

#Check and make sure it is loaded
XLK_prices

#Create a lag of the adjusted price variable and use this variable to calculate 
#holding period returns (HPRs) for XLK.  Name the variable HPR.
#Then, drop any rows missing HPR observations and all columns except date and HPR.
XLK_prices <- XLK_prices %>%
  mutate(adjusted_lag = lag(XLK_prices$adjusted)) %>%
  mutate(HPR = (adjusted - adjusted_lag)/adjusted_lag) %>%
  filter(!is.na(HPR)) %>%
  select(date, HPR)

XLK_prices

#Calculate the mean (i.e., average) and median HPR and round the solutions to the 5th decimal place.
round(mean(XLK_prices$HPR), digits = 5)
round(median(XLK_prices$HPR), digits = 5)

#Use the year and quarter functions from the lubridate package to calculate year and quarter variables.
XLK_prices <- XLK_prices %>%
  mutate(XLK_quarter = quarter (date),
         XLK_year = year (date))

XLK_prices

#Use the group_by and summarize functions to calculate XLK’s average HPR per quarter. 
XLK_quarter_avg <- XLK_prices %>%
  group_by(XLK_year, XLK_quarter) %>%
  summarize(quarter_avg_HPR = mean(HPR)) %>%
  ungroup()

XLK_quarter_avg

#Use the summary function to summarize XLK’s average quarterly HPR, 
#rounding the solutions to the 5th decimal place
summary(XLK_quarter_avg, digits = 5)

#You would like to know in which quarters XLK had the highest returns,
#so you sort XLK_quarter_avg by quarter_avg_HPR as seen here:
XLK_quarter_avg <- XLK_quarter_avg %>%
  arrange(quarter_avg_HPR)

XLK_quarter_avg

#Should you use the head or tail function to show you the quarters in which XLK had the highest returns?
head(XLK_quarter_avg)
tail(XLK_quarter_avg)

#What is the maximum daily return of XLK during the period?  Round your answer to four decimal places.
round(max(XLK_quarter_avg$quarter_avg_HPR), digits = 4)

#You see that XLK’s largest average quarterly returns come from later in the sample period,
#so you would like to know what XLK’s maximum quarterly return was before 2019.
#First, you create a new data set called XLK_quarter_avg_early by limiting your data to observations before 2019.    
XLK_quarter_avg_early <- XLK_quarter_avg %>%
  filter(XLK_year < '2019')

XLK_quarter_avg_early

#Use the max function to calculate the maximum quarterly return for XLK before 2019.
#Round your answer to the 4th decimal place.
round(max(XLK_quarter_avg_early$quarter_avg_HPR), digits = 4)




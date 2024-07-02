#Homework 3

#Clears environment and helps you start fresh
rm(list = ls())

#Making sure my packages are installed
library(tidyverse)
library(tidyquant)

#use the tq_get function from the tidyquant package to download stock price data from May 5, 2020 to January 1, 2023
beer_tickers <- c("BUD", "TAP", "ABEV", "SAM", "HEINY", "STZ", "KNBWY", "SPY")


beer_prices <- tq_get(beer_tickers,
                      get = "stock.prices",
                      from = "2020-05-05",
                      to = "2023-01-01")

#You decide that for your CAPM estimations, you would like monthly HPR data.
#1) Keep only the columns with the date, ticker, and adjusted close prices.
#2) Create a variable that indicates the month of the observation using the lubridate month function
#3) Create a variable that leads the month variable by one row
beer_prices <- beer_prices %>%
  select(symbol, date, adjusted) %>%
  mutate(beer_month = month(date)) %>%
  mutate(lead_beer_month = lead(beer_month)) %>%
  filter(lead_beer_month != beer_month)

#Next, you calculate holding period returns.
#1) Use the group_by function to group your data by the variable that identifies firms (the ticker)
#2) Create a variable that lags the adjusted price variable by one row
#3)Create an HPR return variable calculated as the difference between the adjusted
#close price and the lagged adjusted close price, divided by the lagged adjusted close price. Name the variable HPR
#4) Ungroup the data
beer_prices <- beer_prices  %>%
  group_by(symbol)  %>%
  mutate(lag_adjusted = lag(adjusted))  %>%
  mutate(HPR = (adjusted - lag_adjusted)/lag_adjusted)  %>%
  ungroup()

#1) Use the select function to only keep the symbol, date, and HPR columns in your data
#2) Eliminate any rows with missing HPR from the data
beer_prices <- beer_prices  %>%
  select(symbol, date, HPR)  %>%
  filter(!is.na(HPR))

#Calculate the average HPR for each firm using the group_by function
#1) Use the group_by function to group your data by the variable that identifies the firms (the ticker)
#2) Use the summarize function to create mean_HPR and sd_HPR
#3) Ungroup the data
beer_HPR_summary_stats <- beer_prices  %>%
  group_by(symbol) %>%
  summarize(mean_HPR = mean(HPR), sd_HPR = sd(HPR)) %>%
  ungroup()

#Add a Sharpe variable to beer_HPR_summary_stats and sort by Sharpe.
#1) Create a Sharpe variable equal to the difference between mean_HPR and 0.003, our assumed monthly risk free rate, divided by sd_HPR
#2) Sort the data in descending Sharpe, such that the highest Sharpe ratio is the first row.
beer_HPR_summary_stats <- beer_HPR_summary_stats  %>%
  mutate(Sharpe = (mean_HPR - 0.003)/sd_HPR) %>%
  arrange(-Sharpe)

head(beer_HPR_summary_stats)

#1) Use the tq_get function to download tbill data(TB4WK) from May 5, 2020 to January 1,2023 from FRED
#2) Create a price_decimal variable that is the price variable divided by 100
#3) Create a rf variable that converts the annual price_decimal to a monthly risk-free rate
#4) Use the select function to remove price, price_decimal, and symbol from the data
tbills <- tq_get("TB4WK",
                 get = "economic.data",
                 from = "2020-05-01",
                 to = "2023-01-01")
head(tbills)

tbills <- tbills %>%
  mutate(price_decimal = price / 100) %>%
  mutate(rf = (1+ price_decimal)^(1/12) - 1) %>%
  select(-price, -price_decimal, -symbol)

#You merge the tbill and beer_prices data
#1) Use the floor_date function to create a new data variable in the beer_prices data such that the date
#column in beer_prices matches the date column in tbills
#2) Create a new data set called index_model_data by suing full_join to merge beer_prices and tbills by date
#3) Create an excess return variable in index_model_data as the difference between HPR and rf
#4) Remove any rows with missing excess return observations
beer_prices <- beer_prices %>%
  mutate(date = floor_date(date, "month"))

index_model_data <- full_join(beer_prices, tbills, by = "date")

index_model_data <- index_model_data %>%
  mutate(excess_return = HPR - rf) %>%
  filter(!is.na(excess_return))

#Use a loop to calculate the CAPM betas for each beet company using the loop below
SML_data <- tibble(beta = rep(0,7))
SML_data

for (i in 1:7){
  loop_ticker <- beer_tickers[i]
  loop_y <- index_model_data %>%
    filter(symbol == loop_ticker) %>%
    mutate(excess_return_y = excess_return) %>%
    select(date, excess_return_y)
  
loop_x <- index_model_data %>%
  filter(symbol == "SPY") %>%
  mutate(excess_return_x = excess_return) %>%
  select(date, excess_return_x)
  
loop_data <- full_join(loop_x, loop_y, by = "date")
  
loop_index_model <- lm(excess_return_y ~ excess_return_x, data = loop_data)
  
SML_data[i,1] = loop_index_model$coefficients[[2]]
}


SML_data <- SML_data %>%
  mutate(symbol = beer_tickers[1:7]) %>%
  arrange(-beta)

SML_data

#Add a CPI to your regression
#1) Use the tq_get function to downlaod CPI data (CPIAUCSL) from May 5, 2020 to January 1, 2023 from FRED
#2) Use the select function to drop the symbol column from the data
#3) Create a variable called CPI that is equal to the price
#4) Use the select function to drop the price column from the data
CPI_data <- tq_get("CPIAUCSL",
                 get = "economic.data",
                 from = "2020-05-01",
                 to = "2023-01-01")

CPI_data <- CPI_data %>%
  select(-symbol) %>%
  mutate(CPI = price) %>%
  select(-price)

#Limit analysis to only BUD and use "wide" version of data
#1) Create a new data set called BUD_data by limiting your index data only to observations where symbol is equal to BUD or SPY
#2) Create a new data set called wide_BUD_data from your BUD_data by:
  #Keep only the columns with the data, ticker, and excess_return
  #Use pivot_wider to create a wide version of your data
#3) Create a new data set called BUD_model_data by using full_join to merge CPI_data and wide_BUD data by date
BUD_data <- index_model_data %>%
  filter(symbol == "BUD" | symbol == "SPY")

wide_BUD_data <- BUD_data %>%
  select(symbol, date, excess_return) %>%
  pivot_wider(names_from = symbol,
              values_from = excess_return)

BUD_model_data <- full_join(CPI_data, wide_BUD_data, by = "date")

#Use BUD_model_data to estimate a regression with BUD's excess returns as the y-variable and two x-variables, the market excess returns and CPI
BUD_model <- lm(BUD ~ SPY + CPI, data = BUD_model_data)

summary(BUD_model)




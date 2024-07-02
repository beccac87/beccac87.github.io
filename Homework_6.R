#Clears environment and helps you start fresh
rm(list = ls())

#Library Statements
library(tidyverse)
library(tidyquant)
library(rpart)
library(rpart.plot)

#Question 1
#First, use the tq_get function from the tidyquant package to download price data from January 1, 2000 to January 1, 2023 for three ETFs:
stock_tickers <- c("XLK", "XLY", "XLP")

stock_prices <- tq_get(stock_tickers,
                      get = "stock.prices",
                      from = "2000-01-01",
                      to = "2023-01-01")

#Then, limit your data only to the symbol, date, and adjusted close price columns.
stock_prices <- stock_prices %>%
  select(symbol, date, adjusted)

dim(stock_prices)
head(stock_prices)

#Question 2
#For your analysis, you require weekly HPR data.  Your code should do the follow:
#1) Create a variable that indicates the week of the observation using the lubridate week function.
#2) Create a variable that leads the week variable by one row.
#3) Filter the data to only include observations where the week and leading week are not equal to each other.  This filter should eliminate all observations that are not the last observation in the week.
stock_prices <- stock_prices  %>%
  mutate(week = week(date))  %>%
  mutate(lead_week = lead(week))
  
stock_prices <- stock_prices  %>%
  filter(lead_week != week)

head(stock_prices)
dim(stock_prices)

#Question 3
#Next, you calculate holding period returns.  Your code should do the following:
#1) Use the group_by function to group your data by symbol.
#2) Create a variable that lags the adjusted price variable by one row.
#3) Create a HPR return variable calculated as the difference between the adjusted close price and the lagged adjusted close price, divided by the lagged adjusted close price.  Name the variable HPR. 
#4) Ungroup the data.
#5) Eliminate any observations with missing HPR observations.
#6) Limit the data only to the symbol, date, and HPR columns.
stock_prices <- stock_prices  %>%
  group_by(symbol)  %>%
  mutate(lag_adjusted = lag(adjusted))  %>%
  ungroup()

stock_prices <- stock_prices  %>%
  group_by(symbol)  %>%
  mutate(HPR = (adjusted - lag_adjusted)/lag_adjusted)  %>%
  ungroup()

stock_prices <- stock_prices %>%
  select(symbol, date, HPR) %>%
  filter(!is.na(HPR))

head(stock_prices)
dim(stock_prices)

#Question 4
#Next, you create four variables: lag_HPR_1, lag_HPR_2, lag_HPR_3, and lag_HPR_4.
#These variables lag this week’s returns back four weeks to use in a classification tree.  (Hint:  If you don’t remember the lag function, here is how you would create a variable for a two-period lag:  mutate(lag_HPR_2 = lag(HPR, 2).  The goal is to use the last four weeks’ returns to predict the current week’s returns.)
#Then, filter the data to remove any observations where lag_HPR_4 is missing. 
stock_prices <- stock_prices %>%
  group_by(symbol) %>%
  mutate(lag_HPR_1 = lag(HPR, 1),
         lag_HPR_2 = lag(HPR, 2),
         lag_HPR_3 = lag(HPR, 3),
         lag_HPR_4 = lag(HPR, 4)) %>%
  ungroup() %>%
  filter(!is.na(lag_HPR_4))

head(stock_prices)
dim(stock_prices)

#Question 5
#Use the function pivot_wider to create a wide version of your data.
#The “names_from” column is symbol, and “values_from” should be equal to the following list:  c("HPR", "lag_HPR_1", "lag_HPR_2", "lag_HPR_3", "lag_HPR_4").
wide_stock_prices <- stock_prices %>%
  pivot_wider(names_from = symbol,
              values_from = c("HPR", "lag_HPR_1", "lag_HPR_2", "lag_HPR_3", "lag_HPR_4"))

head(wide_stock_prices)
dim(wide_stock_prices)

#Question 6
#Create a binary variable called “direction” equal to one if HPR_XLY, or the contemporaneous weekly return to the XLY ETF, is greater than 0. 
wide_stock_prices <- wide_stock_prices %>%
  mutate(direction = ifelse(HPR_XLY > 0, 1, 0))

dim(wide_stock_prices)

#Question 7
#Our goal is to use lags of weekly returns to XLY, XLP, and XLK to predict whether XLY increases or decreases, so direction is our outcome variable.
#If, for all observations in the sample, you guessed that XLY increases, what proportion of times would you be correct?  Round your answer to two decimal places.  
round(mean(wide_stock_prices$direction), digits = 2)

#Question 8
#Next, in preparation for your classification tree estimation, you create training and testing data sets.  Your code should do the following:
#1) Set a seed equal to 315.
#2) Draw a random sample of rows equal in length to 70% of the number of rows in your data.
#3) Create a training data set with the rows in your random sample.
#4) Create a testing data set with the rows not in your random sample.
set.seed(315, "Mersenne-Twister", sample.kind="Rejection")

rows <- sample(nrow(wide_stock_prices), 0.7*nrow(wide_stock_prices))

rows[1:5]

training_data <- wide_stock_prices[rows,]
test_data <- wide_stock_prices[-rows,]

dim(training_data)
dim(test_data)

#Question 9
#Use your training data to estimate a classification tree.  Your outcome variable is direction.  Include the four lags of weekly XLY returns as explanatory variables.  Do not specify any hyperparameters in your tree estimation (just rely on the defaults).
#Then, use your testing data to predict outcomes.  Create a confusion matrix using your actual outcomes and predicted outcomes.  Fill in the blanks:
tree_1 <- rpart(direction ~ lag_HPR_1_XLY + lag_HPR_2_XLY + lag_HPR_3_XLY + lag_HPR_4_XLY, 
                    data = training_data, method = "class")

print(tree_1)
rpart.plot(tree_1)

#predictions from tree:
test_data <- test_data %>%
  mutate(predict_1 =  predict(tree_1, test_data, type = "class"))

confusion_matrix_1 <- table(test_data$predict_1, test_data$direction)
confusion_matrix_1

accuracy_1 <- sum(diag(confusion_matrix_1))  / sum(confusion_matrix_1)
accuracy_1

   #We would have been better just guessing "1" all the time...
mean(test_data$direction)

#Question 11
#Now, use your training data to estimate a second tree.  Again, your outcome variable is direction.  Include the four lags of weekly XLY returns as explanatory variables.  For this tree, specify the following hyperparameters:  cp = 0, minbucket = 1, and minsplit = 2.
#Then, prune your tree, requiring cp to be greater than 0.02.  (Set cp = 0.02 in the pruning statement).  Use your pruned tree and testing data to predict outcomes.  Create a confusion matrix using your actual outcomes and predicted outcomes.  Fill in the blanks:
tree_2 <- rpart(direction ~ lag_HPR_1_XLY + lag_HPR_2_XLY + lag_HPR_3_XLY + lag_HPR_4_XLY, 
                    data = training_data, method = "class", cp = 0, minbucket = 1, minsplit = 2) 

#print(tree_2)
#rpart.plot(tree_2)

tree_2_pruned <- prune(tree_2, cp = 0.02)

test_data <- test_data %>%
  mutate(predict_2 = predict(tree_2_pruned, test_data, type = "class"))

confusion_matrix_2 <- table(test_data$predict_2, test_data$direction)
confusion_matrix_2

accuracy_2 <- sum(diag(confusion_matrix_2))  / sum(confusion_matrix_2)
round(accuracy_2, digits = 3)

#Precision = TP / (TP + FP) 
#Recall = TP / (TP + FN)

precision <- 112 / (112 + 111)
round(precision, digits = 3)

recall <- 112 / (112 + 76)
round(recall, digits = 3)

#Question 13
#Use your training data to estimate a third and final tree.  Again, your outcome variable is direction.  Include the four lags of weekly XLY returns, four lags of weekly XLP returns, and four lags of weekly XLK returns as explanatory variables.  For this tree, specify the following hyperparameters:  cp = 0, minbucket = 1, and minsplit = 2.
#Then, prune your tree, requiring cp to be greater than 0.02.  (Set cp = 0.02 in the pruning statement).  Use your pruned tree and testing data to predict outcomes.  Create a confusion matrix using your actual outcomes and predicted outcomes.  Fill in the blanks:
tree_3 <- rpart(direction ~ lag_HPR_1_XLY + lag_HPR_2_XLY + lag_HPR_3_XLY + lag_HPR_4_XLY + 
                  lag_HPR_1_XLP + lag_HPR_2_XLP + lag_HPR_3_XLP + lag_HPR_4_XLP + 
                  lag_HPR_1_XLK + lag_HPR_2_XLK + lag_HPR_3_XLK + lag_HPR_4_XLK,
                data = training_data, method = "class", cp = 0, minbucket = 1, minsplit = 2)

#print(tree_3)
#rpart.plot(tree_3)

tree_3_pruned <- prune(tree_3, cp = 0.02)

test_data <- test_data %>%
  mutate(predict_3 =  predict(tree_3_pruned, test_data, type = "class"))

confusion_matrix_3 <- table(test_data$predict_3, test_data$direction)
confusion_matrix_3

accuracy_3 <- sum(diag(confusion_matrix_3))  / sum(confusion_matrix_3)
round(accuracy_3, digits = 3)

#Precision = TP / (TP + FP) 
#Recall = TP / (TP + FN)

precision <- 133 / (133 + 123)
round(precision, digits = 3)

recall <- 133 / (133 + 65)
round(recall, digits = 3)

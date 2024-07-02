#Clears environment and helps you start fresh
rm(list = ls())

#Library Statements!
library(tidyverse)
library(rpart)
library(rpart.plot)
library(randomForest)

##Question 1:
#First, save the data to your computer and use an appropriate file path to read the data into RStudio.  Fill in the blanks below:
bnb_data <-read.csv("~/Desktop/FIN454/AirBnBlistings.csv")
dim(bnb_data)
head(bnb_data)

##Question 2:
#If the host of the AirBnB listing is a superhost, host_is_superhost equals “t”.
#Create a binary variable equal to one if host_is_superhost equals “t”. 
#Name this variable superhost_binary.
#What proportion of listings are for hosts that are designated as superhosts?  Round your solution to three decimal places. 
bnb_data <- bnb_data %>%
  mutate(superhost_binary = ifelse(host_is_superhost == "t", 1, 0))

round(mean(bnb_data$superhost_binary), digits = 3)


##Question 3:
#The data includes a variable, instant_bookable_binary, that is equal to one if the property is instantly bookable and zero if you must request the host’s permission to book the property.  Calculate the following answers, do not round intermediate solutions, and round all final solutions to three decimal places.
prob_instant_bookable <- mean(bnb_data$instant_bookable_binary)
prob_not_instant_bookable <- 1 - prob_instant_bookable

entropy_whole_sample <- -1* (prob_instant_bookable*log(prob_instant_bookable,base = 2) + 
                               prob_not_instant_bookable*log(prob_not_instant_bookable, base = 2))

round(prob_instant_bookable, digits = 3)
round(entropy_whole_sample, digits = 3)

##Question 4:
#You are considering estimating a classification tree to predict an outcome variable of whether the listing is instantly bookable (instant_bookable_binary equals 1).  You wonder whether superhost_binary is an appropriate explanatory variable to include in a tree estimation.  So, you calculate by hand the information gain that would result from a tree splitting the sample on superhost_binary.  Calculate the following answers, do not round intermediate solutions, and round all final solutions to three decimal places.
#What is the probability that a listing with a superhost is instantly bookable?  
#What is the entropy in a sample of listings with superhosts? 
#What is the probability that a listing that does not have a superhost is instantly bookable?
#What is the entropy in a sample of listings without superhosts? 
#What is the weighted average of entropy for this possible split? 
#What is the information gain for this possible split? 
##data superhost
data_super <- bnb_data %>%
  filter(superhost_binary == 1)

prob_instant_bookable_super <- mean(data_super$instant_bookable_binary)
prob_not_instant_bookable_super <- 1 - prob_instant_bookable_super

entropy_super <- -1* (prob_instant_bookable_super*log(prob_instant_bookable_super,base = 2) + 
                        prob_not_instant_bookable_super*log(prob_not_instant_bookable_super, base = 2))

round(prob_instant_bookable_super, digits = 3)
round(entropy_super, digits = 3)

##data not superhost:
data_notsuper <- bnb_data %>%
  filter(superhost_binary != 1)

prob_instant_bookable_notsuper <- mean(data_notsuper$instant_bookable_binary)
prob_not_instant_bookable_notsuper <- 1 - prob_instant_bookable_notsuper

entropy_notsuper <- -1* (prob_instant_bookable_notsuper*log(prob_instant_bookable_notsuper,base = 2) + 
                           prob_not_instant_bookable_notsuper*log(prob_not_instant_bookable_notsuper, base = 2))

round(prob_instant_bookable_notsuper, digits = 3)
round(entropy_notsuper, digits = 3)

##weighted average:
super_length <- nrow(data_super)
notsuper_length <- nrow(data_notsuper)


entropy_weighted <- entropy_super * (super_length / (super_length + notsuper_length)) +
  entropy_notsuper * (notsuper_length / (super_length + notsuper_length))


round(entropy_weighted, digits = 3)

##info gain:
info_gain <- entropy_whole_sample - entropy_weighted

round(info_gain , digits = 3)

#Question 5:  S/A 

##Question 6:
#You remember from class that calculating the gini index is another way of considering whether an explanatory variable provides valuable information to a forest.  So, you continue with your calculations and calculate the gini index that would result from a tree splitting the sample on superhost_binary.  Calculate the following answers, do not round intermediate solutions, and round all final solutions to three decimal places.
#What is the gini index for instant_bookable_binary for the whole sample?
#What is the gini index for instant_bookable_binary in a sample of listings with superhosts? 
#What is the gini index for instant_bookable_binary in a sample of listings without superhosts? 
#What is the difference between the weighted average gini index for this possible split and the gini index for the whole sample?
gini_whole_sample <- 1 - (prob_instant_bookable^2 + prob_not_instant_bookable^2)

round(gini_whole_sample, digits = 3)

gini_super <- 1 - (prob_instant_bookable_super^2 + prob_not_instant_bookable_super^2)

gini_notsuper <- 1 - (prob_instant_bookable_notsuper^2 + prob_not_instant_bookable_notsuper^2)

round(gini_super, digits = 3)
round(gini_notsuper, digits = 3)

gini_weighted <- gini_super * (super_length / (super_length + notsuper_length)) +
  gini_notsuper * (notsuper_length / (super_length + notsuper_length))

round(gini_weighted, digits = 3)

##Question 7:  S/A


##Question 8:
#Next, in preparation for estimating a tree and forest, you create training and testing data sets.  Your code should do the following:
#1) Set a seed equal to 90001 exactly like we did in class, using “Mersenne-Twister” as the algorithm and “Rejection” as the sample.kind.
#2) Draw a random sample of rows equal in length to 60% of the number of rows in your data.  (Note:  we usually use 70%.  We are purposely only going to use 60% for this exercise because we have so many observations.)
#3) Create a training data set with the rows in your random sample.
#4) Create a testing data set with the rows not in your random sample.
set.seed(90001, "Mersenne-Twister", sample.kind = "Rejection")

rows <- sample(nrow(bnb_data), 0.6*nrow(bnb_data))

training_data <- bnb_data[rows,]
test_data <- bnb_data[-rows,]

head(rows, 5)
dim(training_data)
dim(test_data)

##Question 9:
#Use your training data to estimate a regression tree explaining price (y-variable), with four explanatory variables:  accommodates, bedrooms, private_stay, and minimum_nights.  Remember that the method for a regression tree must be set to “anova”. 
#Use the rpart.plot function to examine your tree.  Answer the following questions.
#How many terminal nodes does your tree have?
#In the farthest left terminal node, what is the predicted price?
#The farthest right terminal node contains what proportion of the observations?
#You are considering a listing with 2 bedrooms.  Based on your tree, what is the predicted price for this listing?
reg_tree_1 <- rpart(price ~ accommodates + bedrooms  +
                      private_stay + 
                      minimum_nights, 
                    data = training_data, method = "anova")

print(reg_tree_1)

rpart.plot(reg_tree_1)

##Question 10:
#Use your tree to predict prices for the listings in your test data.  What is the average predicted price?  Round your solution to three decimal places. 
test_data <- test_data %>%
  mutate(predicted_price = predict(reg_tree_1, test_data, type = "matrix"))

round(mean(test_data$predicted_price), digits = 3)


##Question 11:
#Use ggplot to plot your predicted prices (y-variable) against your actual prices (x-variable) in your test data.  Label your axes appropriately.  Save your figure as a pdf, 6 inches by 6 inches in dimension.  Upload your graph below.
ggplot(data = test_data, aes(x = price, y = predicted_price)) +
  geom_point() + 
  theme_bw() +
  labs(x = "price",
       y = "predicted price", 
       title = "Predicted prices using a regression tree",
       subtitle = "Los Angeles, CA AirBnB listings, Q1 2023")

##Question 12:
#Calculate the root mean squared error (RMSE) for your regression tree and round to three decimal places. 
test_data <- test_data %>%
  mutate(error = price - predicted_price)

##RMSE:
reg_tree_RMSE <- sqrt(mean((test_data$error)^2))

round(reg_tree_RMSE, digits = 3)


##Question 13:
#You are concerned that your RMSE is too high.  So, you decide to investigate your error more.  Use the group_by and summarize functions to calculate average error and average predicted price per number of bedrooms, then sort by absolute value of average error.  Fill in the table below and round to the nearest whole number.
error_bedrooms <- test_data %>%
  group_by(bedrooms) %>%
  summarize(mean_error = mean(error), mean_pred_price = mean(predicted_price)) %>%
  ungroup() %>%
  arrange(-abs(mean_error))

error_bedrooms

##Question 14: S/A
table(test_data$bedrooms)


##Question 15:
#Disheartened by your regression tree’s performance, you decide to estimate a linear regression, to see if a linear regression results in lower error.  Estimate a linear model in your training data with an outcome variable of price and four explanatory variables:  accommodates, bedrooms, private_stay, and minimum_nights.  Report the intercept and coefficients on your explanatory variables.  Round each to three decimal places.
reg_1 <- lm(price ~ accommodates + bedrooms  +
              private_stay + 
              minimum_nights,  data = training_data)

round(reg_1$coeff, digits = 3)

##Question 16:
#Use your linear model to predict prices for the listings in your test data.  Then, calculate the root mean squared error (RMSE) for your linear model and round to three decimal places.  
test_data <- test_data %>%
  mutate(predicted_price_lm = predict(reg_1, test_data))

test_data <- test_data %>%
  mutate(error_lm = price - predicted_price_lm) %>%
  filter(!is.na(error_lm))

##RMSE:
lm_RMSE <- sqrt(mean((test_data$error_lm)^2))


round(reg_tree_RMSE, digits = 3)
round(lm_RMSE, digits = 3)

##Question 17:
#Continue to analyze your linear model error.  Use the group_by and summarize functions to calculate average error and average predicted price per number of bedrooms, then sort by absolute value of average error.  Fill in the table below and round to the nearest whole number.
error_bedrooms_lm <- test_data %>%
  group_by(bedrooms) %>%
  summarize(mean_error_lm = mean(error_lm), mean_pred_price = mean(predicted_price_lm)) %>%
  ungroup() %>%
  arrange(-abs(mean_error_lm))

error_bedrooms_lm

##Question 18:  S/A

##Question 19:
#Now, estimate a second regression tree explaining price (y-variable), with four explanatory variables:  accommodates, bedrooms, private_stay, and minimum_nights.  Do not use the default complexity parameter (cp).  Set cp equal to 0.001.  Remember that the method for a regression tree must be set to “anova”. 
#What is the RMSE for this tree? Round to three decimal places. 
reg_tree_2 <- rpart(price ~ accommodates + bedrooms  +
                      private_stay + 
                      minimum_nights, 
                    data = training_data, method = "anova", cp = 0.001)

print(reg_tree_2)

rpart.plot(reg_tree_2)

test_data <- test_data %>%
  mutate(predicted_price_tree2 = predict(reg_tree_2, test_data, type = "matrix"))

test_data <- test_data %>%
  mutate(error_tree2 = price - predicted_price_tree2)

##RMSE:
reg_tree_RMSE_tree2 <- sqrt(mean((test_data$error_tree2)^2))
round(reg_tree_RMSE_tree2, digits = 3)

##Question 20:
#Which of the three models you have estimated so far, the regression tree, the linear model, or the regression tree with a low cp, predicts prices most accurately?  Explain.
round(reg_tree_RMSE, digits = 3)
round(lm_RMSE, digits = 3)


############
####Forest:
rf <- randomForest(price ~ accommodates + bedrooms  +
                     private_stay + 
                     minimum_nights, data = training_data, proximity=TRUE,
                   ntree = 200)

print(rf)

#Question 21
#Now, estimate a random forest (rf) explaining price (y-variable), with four explanatory variables:  accommodates, bedrooms, private_stay, and minimum_nights.  Set ntree equal to 200.  (Note:  the default is 500, which will take several minutes to run on your computer.  Estimating a forest with 200 trees should only take a minute or two.)
#Use either the importance() function or varImpPlot() function to identify which of the four explanatory variables is the most important to explaining price. (answer should be accommodates, bedrooms, private_stay, or minimum_nights) 
print(importance(rf,type = 2)) 

varImpPlot(rf)

##Question 22:
#Use your random forest (rf) to calculate predicted prices.  Then, calculate RMSE. 
#What is the RMSE? Round to three decimal places.
test_data <- test_data %>%
  mutate(predicted_price_forest = predict(rf, test_data, type = "response"))

test_data <- test_data %>%
  mutate(error_forest = price - predicted_price_forest)

##RMSE:
reg_tree_RMSE_forest <- sqrt(mean((test_data$error_forest)^2))

round(reg_tree_RMSE_forest, digits = 3)

##Question 23: S/A
round(reg_tree_RMSE_tree2, digits = 3)

round(reg_tree_RMSE, digits = 3)
round(lm_RMSE, digits = 3)

print(rf)




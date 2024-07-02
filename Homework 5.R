#Clears environment and helps you start fresh
rm(list = ls())

#Library Statements
library(tidyverse)
library(ggplot2)

#Download credit_card_data from the “data for class” folder on Blackboard and save it to your computer.  Read the data into R using a file path.  If you would like to refer to details about the data, see the list of variables and descriptions in Blackboard under the data. 
cc_data <-read.csv("~/Desktop/FIN454/credit_card_data.csv", header = TRUE)
head(cc_data)

#QUESTION 1
#Use the dimension (dim) function to find the number of rows and columns in the data.  Fill in the blanks below.  Round to the nearest whole number.
dim(cc_data)

#QUESTION 2
#We are interested in explaining variation in the LIMIT_BAL variable, which is the credit limit of each borrower.
#Create a new version of this variable that is the natural log of the variable using the log function, and save this variable using a new name because we will still use LIMIT_BAL for some calculations.
cc_data <- cc_data %>%
  mutate(limit_balance = log(LIMIT_BAL))

#Use the summary and standard deviation functions to calculate summary statistics for this new variable.  Fill in the blanks with your solutions.  Round all answers to two decimal places. 
summary(cc_data$limit_balance)
sd(cc_data$limit_balance)

#QUESTION 3
#Create a histogram to display the distribution of your logged credit limit variable.
#Select an appropriate binwidth, label your axes, and title your figure.
limit_bal_histogram <- ggplot(data = cc_data, aes(x = log(LIMIT_BAL))) +
  geom_histogram(color = "black", fill = "lightblue", binwidth = 0.25) +
  labs(x = "limit of credit card balance", y = "count", 
       title = "Histogram of limits on credit card balances",
       subtitle = "Taiwan, April 2005 to September 2005") + 
  theme_bw() 

limit_bal_histogram

#Save your figure as a jpg, with a width of 6 inches and a height of 6 inches.  Upload your figure below.
ggsave(filename = "Desktop/limit_bal_histogram.jpg",
       plot = limit_bal_histogram, 
       height = 6,
       width = 6,
       units = c("in"))

#QUESTION 4
#Estimate a linear regression with LIMIT_BAL divided by 1000 as the y-variable and AGE as the x-variable.
#Answer the following questions using your regression output.  Round all final numeric answers to two decimal places.  Please do not round intermediate calculations.
reg_1 <- lm(LIMIT_BAL/1000 ~ AGE, data = cc_data)
summary(reg_1)

#What is the predicted credit limit for a borrower who is 25 years old? 
reg_1$coefficients[[2]]  #For each additional year old, credit limits increase by $2.03 th.
reg_1$coefficients[[1]] + reg_1$coefficients[[2]]*25 #Predicted balance: Age = 25...

#QUESTION 5
#Interpret the coefficient on AGE for your linear regression from question 4.  (If x goes up by…, what happens to y?)

#ANSWER: The coefficient on AGE is 2.04.  This coefficient means that if a borrower’s age increases by one year, the expected (or average) credit limit increases by 2.04 thousand. 

#QUESTION 6
#Estimate a linear regression with your logged credit limit variable as the y-variable and AGE as the x-variable.
reg_2 <- lm(limit_balance ~ AGE, data = cc_data)
summary(reg_2)
#Answer the following questions using your regression output.  Round all final numeric answers to four decimal places.  Please do not round intermediate calculations.
round(reg_2$coefficients[[1]], digits = 4)
round(reg_2$coefficients[[2]], digits = 4)

##Interpreting coefficient:
exp(reg_2$coefficients[[2]])
(exp(reg_2$coefficients[[2]]) - 1)*100  #for each additional year old, percent increase in credit limit.

##Predicted balance: Age = 25:
round((exp(reg_2$coefficients[[1]] + reg_2$coefficients[[2]]*25))/1000, digits = 4)

#QUESTION 7
#Interpret the coefficient on AGE from your linear regression from question 6.  (If x goes up by…, what happens to y?)

#ANSWER: The coefficient on AGE estimated is 0.0138.
#Because the outcome (or y) variable is logged, the interpretation of the coefficient is this:  if borrower age increases by one year, expected credit limit increases by (exp(0.0138) – 1) * 100 = (1.0139 – 1) * 100 = 1.39%.  

#QUESTION 8
#Now, we will shift focus to analyzing a new outcome variable (or y-variable), default.
#Use the summary and standard deviation functions to calculate summary statistics for default.payment.next.month.
#Fill in the blanks with your solutions.  Round all answers to two decimal places. 
summary(cc_data$default.payment.next.month)
sd(cc_data$default.payment.next.month)

#QUESTION 9
#Calculate the probability of default, odds of default, and the logged odds of default using the binary variable default.payment.next.month.  Round all answers to three decimal places:
default_table <- table(cc_data$default.payment.next.month)
default_table

probability_of_default <- 6636 / (23364 + 6636)
probability_of_default

odds_of_default <- 0.2212 / (1 - 0.2212)
odds_of_default

log_odds_default <- log(28.40267)
log_odds_default

#QUESTION 10
#Use the glm function to estimate a logistic model with default.payment.next.month as the y-variable and AGE as the x-variable.
#Remember to use family = binomial(link = "logit") in your glm function.* Answer the following questions using your regression output.  Round all final numeric answers to four decimal places.  Please do not round intermediate calculations.
default_glm <- glm(default.payment.next.month ~ AGE, 
                   data = cc_data, 
                   family = binomial(link = "logit"))

summary(default_glm)

alpha_1 = default_glm$coefficients[[1]]
beta_1 = default_glm$coefficients[[2]]

#So, what is the predicted probability of default for a borrower who is 25 years old?
exp(alpha_1 + beta_1*25)/ (1 + exp(alpha_1 + beta_1*25))

#QUESTION 11
#Interpret your intercept estimate from your logistic regression from question 10.  What does the intercept mean?

#ANSWER: The intercept is the estimated log of odds of default if the x-variable is equal to zero.
#The x-variable here is AGE, so intercept is the estimated log of odds if AGE is zero.  (Obviously, this is hypothetical because we would not really have borrowers who are infants.)
#However, exp(-1.3870) = 0.25.  So, 0.25 is the odds of default for borrowers who are zero years old.
#The probability of default for these borrowers is then:  0.25 / (1 + 0.25) = 0.20, or 20%.  

#QUESTION 12
#Interpret the coefficient on AGE from your logistic regression from question 10.  (If x goes up by…, what happens to y?)

#ANSWER: The estimated coefficient on age is 0.0036.
#This coefficient means that, if a borrower’s age increases by one year, the log odds of default increases by 0.0036.
#Additionally, the odds of default increase multiplicatively by x*exp(beta), or x*1.0036.    

#QUESTION 13
#Create a binary variable (sex_binary) equal to 1 for male borrowers and 0 for female borrowers.
#Use the table function to create a crosstabs table for sex_binary and default.  Fill in the blanks in the following table.
cc_data <- cc_data %>%
  mutate(sex_binary = ifelse(SEX == 1 , 1, 0))

default_sex_xtab <- table(cc_data$sex_binary, 	cc_data$default.payment.next.month)
default_sex_xtab

margin.table(default_sex_xtab, 1)
margin.table(default_sex_xtab, 2)


#QUESTION 14
#Use your crosstabs table to calculate the following solutions. Round all final numeric answers to four decimal places.  Please do not round intermediate calculations.
#What is the predicted probability of default for a male borrower? 
male_probability <- 2873 / 11888
male_probability
#What is the predicted probability of default for a female borrower? 
female_probability <- 3763 / 18112
female_probability
#What are the odds of default for a male borrower?
male_odds <- 0.2416723 / (1 - 0.2416723)
male_odds
#What are the odds of default for a female borrower?
female_odds <- 0.2077628 / (1 - 0.2077628)
female_odds
#What is the odds ratio for male to female borrowers? 
male_to_female_odds <- 0.3186911 / 0.2622482
male_to_female_odds
#What is the logarithm of the odds of female borrowers?
log(female_odds)
#What is the logarithm of the odds of male borrowers? 
log(male_odds)
#What is the logarithm of odds ratio for male to female borrowers? 
log(male_to_female_odds)

#QUESTION 15
#Use the glm function to estimate a logistic model with default.payment.next.month as the y-variable and sex_binary as the x-variable.
#Remember to use family = binomial(link = "logit") in your glm function.  Answer the following questions using your regression output.  Round all final numeric answers to four decimal places.  Please do not round intermediate calculations.
default_glm_2 <- glm(default.payment.next.month ~ sex_binary, 
                   data = cc_data, 
                   family = binomial(link = "logit"))

summary(default_glm_2)

exp(default_glm_2$coefficients[[1]]) #The odds of a person who is NOT male defaulting
exp(default_glm_2$coefficients[[2]])

#Odds of a person who IS male and defaulting is 
exp(default_glm_2$coefficients[[2]]) * exp(default_glm_2$coefficients[[1]])

#QUESTION 16
#Interpret your intercept estimate from your logistic regression from question 15.  What does the intercept tell us about female borrowers and default?

#ANSWER:The estimated intercept is -1.3385.
#Because the only other variable in the regression, sex_binary, is equal to one for male borrowers, this coefficient captures the log odds that female borrowers (sex_binary equal to zero) default.
#So, -1.3385 is the log odds of female borrowers defaulting, and exp(-1.3385) is the odds of female borrowers defaulting, which is equal to 0.2622.  

#QUESTION 17
#Interpret the coefficient on sex_binary from your logistic regression from question 15.  What does the coefficient tell us about male borrowers and default?

#ANSWER: The estimated coefficient on sex_binary is 0.1949.
#Because sex_binary is equal to one for male borrowers, this coefficient is the log odds ratio of default for male to female borrowers.
#So, exp(0.1949), or 1.2152, is the odds ratio for male to female borrowers.
#This ratio tells us that the odds of male borrowers defaulting is 1.2152 times that of the odds ratio of female borrowers.  

#QUESTION 18
#Is it possible to solve for the odds of male borrowers using only the intercept and coefficient on sex_binary from your logistic regression from question 15?  Explain fully.

#ANSWER:Yes! The intercept coefficient, -1.3385, is the log odds of default for female borrowers.
#The coefficient on sex_binary, 0.1949, is the log odds ratio of default for male to female borrowers.
#So, we can calculate the odds of default for female borrowers as exp(-1.3385), or 0.2622.
#We can also calculate the odds ratio of default for male to female borrowers as exp(0.1949), or 1.2152.
#The odds of default for male borrowers is the odds of default for female borrowers times the odds of default for male borrowers (because female * (male / female) = male).
#So, the odds of default for male borrowers is 0.2622 * 1.2152, or 0.3187.

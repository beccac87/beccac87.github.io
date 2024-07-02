#Clears environment and helps you start fresh
rm(list = ls())

#Library Statements
library(tidyverse)
library(tidyquant)
library(ggplot2)
library(ggthemes)
library(tvthemes)
library(hrbrthemes)

#QUESTION 1
#Download credit_card_data from the “data for class” folder on Blackboard and save it to your computer.  Read the data into R using a file path
Credit_data <-read.csv("~/Desktop/FIN454/credit_card_data.csv", header = TRUE)
head(Credit_data)

#Use the dimension (dim) function to find the number of rows and columns in the data
dim(Credit_data)

#QUESTION 2
#We are interested in explaining variation in the LIMIT_BAL variable, which is the credit limit of each borrower.  Because the scale of the variable is large, create a new version of the credit limit variable that is equal to LIMIT_BAL divided by 1000.  Use the summary and standard deviation functions to calculate summary statistics for this new variable.  Fill in the blanks with your solutions.  Round all answers to one decimal place.  Going forward, use your newly-created credit limit variable for all calculations. 
Credit_data <- Credit_data %>%
  mutate(credit_limit = LIMIT_BAL / 1000)

summary(Credit_data)

sd(Credit_data$credit_limit)
mean(Credit_data$credit_limit)

#QUESTION 3
#Create a histogram to display the distribution of credit limit.  Select an appropriate binwidth, label your axes, and title your figure.  Save your figure as a jpg, with a width of 6 inches and a height of 6 inches.  Upload your figure below.
Credit_limit_hist <- ggplot(data = Credit_data, aes(x = credit_limit)) +
  geom_histogram(color = "black", fill = "lightblue",  binwidth = 10) +
  labs(x = "Credit Limit", y = "Count", title = "Histogram of Credit Limit Distribution") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

Credit_limit_hist

ggsave(filename = "Desktop/Credit_limit_hist.jpg",
       plot = Credit_limit_hist,
       height = 6,
       width = 6,
       units = c("in"))

#QUESTION 4
#Now that you understand the distribution of the credit limit variable, you turn your attention to possible explanatory variables.  First, you consider borrower age as a possible explanatory variable for credit limit.
#Create a scatterplot with credit limit as your y-variable and age as your x-variable.  Label your axes, and title your figure.  Save your figure as a jpg, with a width of 6 inches and a height of 6 inches.  Upload your figure below.
Credit_data_scatterplot <- ggplot(data = Credit_data, aes(x = AGE, y = credit_limit)) +
  geom_point() +
  labs(x = "Age", y = "Credit Limit", title = "Scatterplot of Credit Limit by Age using Credit Card Data") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_smooth(method = "lm", formula = y~x, se = FALSE) +
  geom_hline(yintercept = 0) 

Credit_data_scatterplot

ggsave(filename = "Desktop/Credit_data_scatterplot.jpg",
       plot = Credit_data_scatterplot,
       height = 6,
       width = 6,
       units = c("in"))

#QUESTION 5
#Use the correlation function (cor) to calculate the correlation between credit limit and firm age.  Round to two decimal places.  Fill in the blank with your solution.  
cor(Credit_data$credit_limit, Credit_data$AGE)

#QUESTION 6
#Based on your analysis of credit limit and age, do you believe that age is a relevant explanatory variable for credit limit?  Should we include age in our model?  Defend your answer.

#ANSWER: I do not believe that age is a relevant explanatory variable, so we should not include age in our model.
#Looking at the scatterplot, there is no clear relation between age and credit limit.
#The correlation of 0.14 between the variables, which is a relatively low correlation, confirms that age is not strongly correlated with credit limit, so age is not a relevant predictor.

#QUESTION 7
#Next, you consider borrower education level as a possible explanatory variable for credit limit.
#1) Use the unique function to determine how many different unique responses there are for the education variable.
#2) Create a new variable in your data called “ones” equal to 1.  (The variable should be equal to 1 for all observations.)  Use the summary function to examine the summary statistics for “ones” to ensure you have created the variable correctly.
#3) Next, you are going to use group_by and summarize to find the number of observations per each level of education.  Remember to save as a new object so that you do not over-write your credit card data.  Group by the education variable and use summarize to create a variable equal to the sum of “ones”.  This provides the number of observations for each level of education.  Fill in the blanks below with your answers.

unique(Credit_data$EDUCATION)

Credit_data <- Credit_data %>%
  mutate(ones = 1)

summary(Credit_data)

Credit_data_summary_stats <- Credit_data  %>%
  group_by(EDUCATION) %>%
  summarize(total_count = sum(ones)) %>%
  ungroup()

Credit_data_summary_stats

#QUESTION 8
#Next, use mutate to create four binary variables:  grad_school_binary, college_binary, high_school_binary, and schooling_unknown_binary.  The first (grad_school_binary) should equal one when EDUCATION equals one; this variable indicates which borrowers have graduate-level education. The second (college_binary) should equal one when EDUCATION equals two; this variable indicates which borrowers have college-level education.  The third (high_school_binary) should equal one when EDUCATION equals three; this variable indicates which borrowers have high-school-level education.  The fourth (schooling_unknown_binary) should equal one when EDUCATION equals four, five, or zero; this variable indicates which borrowers have unknown level of education.  Find the average of each of the four binary variables.  Round your answers to four decimal places and fill in the blanks below.
Credit_data <- Credit_data %>%
  mutate(grad_school_binary = ifelse(EDUCATION == 1, 1, 0),
         college_binary = ifelse(EDUCATION == 2, 1, 0),
         high_school_binary = ifelse(EDUCATION == 3, 1, 0),
         schooling_unknown_binary = ifelse(EDUCATION == 4 | 
                                             EDUCATION == 5 |
                                             EDUCATION == 0, 1, 0))

mean(Credit_data$grad_school_binary)
mean(Credit_data$college_binary)
mean(Credit_data$high_school_binary)
mean(Credit_data$schooling_unknown_binary)

#QUESTION 9
#What proportion of borrowers have a graduate-level education or a college-level of education?  Explain.

#ANSWER: If 35.3% of borrowers have a graduate-level education and 46.8% of borrowers have a college-level education, then the sum of these two, or 82.1% of borrowers have either a graduate or college-level of education.
#To calculate these proportions, I found averages of binary variables indicating level of education.
#Because an average is the sum of the variable, divided by the number of observation, the average of a binary variable is the sum of the “ones”, or the number of graduate-level educated individuals, divided by the total number of observations in the data.
#Effectively, this calculates the percent of graduate-level educated individuals in the dataset.  (The same statement is true for college-level educated individuals.)

#QUESTION 10
#Your data includes a variable, SEX, that is equal to 1 for male borrowers and 2 for female borrowers.
#1) What is the average of SEX? Round your answer to two decimal places. 
#2) Create a binary variable (sex_binary) equal to 1 for male borrowers and 0 for female borrowers.  What is the average of sex_binary?  Round your answer to two decimal places
mean(Credit_data$SEX)
unique(Credit_data$SEX)

Credit_data <- Credit_data %>%
  mutate(sex_binary = ifelse(SEX == 1 , 1, 0))
       
round(mean(Credit_data$sex_binary), digits = 2)

#QUESTION 11
#Create a boxplot to show the distributions of credit limit for male and female borrowers.  Label your axes, and title your figure Save your figure as a jpg, with a width of 8 inches and a height of 8 inches.  Upload your figure below.  (Hint:  for an example of a similar boxplot, see your class notes from January 26th.)
Credit_data_boxplot <- ggplot(data = Credit_data,
                      aes(x =LIMIT_BAL/1000, y = as.character(sex_binary))) +
  geom_boxplot(fill = "lightblue") +
  labs(x = "Sex", y = "Credit Limit", title = "Boxplot distribution of credit limit for male and female borrowers") +
  theme_avatar(title.font = "Herculanum") +
  theme(plot.title = element_text(hjust = 0.5)) +
  coord_flip()

Credit_data_boxplot

ggsave(filename = "Desktop/Credit_data_boxplot.jpg",
       plot = Credit_data_boxplot,
       height = 8,
       width = 8,
       units = c("in"))

#QUESTION 12
#Based on your analysis, you decide to focus your analysis only on observations for which EDUCATION is equal to one, two, or three.  Create a new data set, and use filter to eliminate observations for which EDUCATION equals zero, four, five, or six.  Use this data set for all regressions and calculations for the rest of the assignment.
#Use the dimension (dim) function to find the number of rows and columns in the data.  Fill in the blanks below.  Round to the nearest whole number.  (Note that you should have more columns than previously because we have created several new variables.)
New_credit_data <- Credit_data %>%
  filter(EDUCATION == 1 | EDUCATION == 2 | EDUCATION == 3)

unique(New_credit_data$EDUCATION)
dim(New_credit_data)

#QUESTION 13
#You now run your first regression model.  Regress credit limit on grad_school_binary and college_binary.  (Remember to use the data set you created in #12.)  Fill in the blanks below.  Round to two decimal places.
regression_1 <- lm(credit_limit ~ grad_school_binary + college_binary, data = New_credit_data)
summary(regression_1)

#QUESTION 14
#You now run your second regression model.  Regress credit limit on high_school_binary and college_binary.  Fill in the blanks below.  Round to two decimal places.
regression_2 <- lm(credit_limit ~ high_school_binary + college_binary, data = New_credit_data)
summary(regression_2)

#QUESTION 15
#In your first regression model, the coefficient estimate for college_binary is positive.  In your second regression model, the coefficient estimate for college_binary is negative.  Why?  Explain. 

#ANSWER: SEE HOMEWORK 4 KEY, very long

#QUESTION 16
#Calculate a predicted credit limit for a borrower with a college-level education using your first regression results.  Then, calculate a predicted credit limit for a borrower with a college-level education using your second regression results.  Are your predictions the same?  Why or why not? 

#ANSWER: Our predictions are the same because model 1 and model 2 are effectively the same model.
#In model 1, we estimate coefficients for grad school and college borrowers, and the intercept captures high school borrowers.
#In model 2, we estimate coefficients for high school and college borrowers, and the intercept captures grad school borrowers.
#Thus, all three binary variables are included in both models, albeit in different ways.

#QUESTION 17
#You now run your third regression model.  Regress credit limit on grad_school_binary, college_binary, and AGE.  Fill in the blanks below.  Round to two decimal places.
regression_3 <- lm(credit_limit ~ grad_school_binary + college_binary + AGE, data = New_credit_data)
summary(regression_3)

#QUESTION 18
#Use your third regression model results to answer this question.  If AGE is the x-variable and credit limit is the y-variable, what is the equation for the line of best fit for a borrower with a college-level education? Fill in the blanks below.  Round to two decimal places.  If possible, do not round any intermediate calculations.
#y = [2.72]x + [52.61]
#2.72 is the AGE estimate and 51.21 is college_binary (35.68) plus the intercept (16.93)

#QUESTION 19
#You now run your fourth regression model.  Regress credit limit on only sex_binary (the binary variable we created earlier).  Fill in the blanks below.  Round to two decimal places.
regression_4 <- lm(credit_limit ~ sex_binary, data = New_credit_data)
summary(regression_4)

#QUESTION 20
#Does your fourth regression model suggest that male or female borrowers have, on average, higher credit limits?  Explain.

#ANSWER: The model predicts that female borrows, on average, have higher credit limits.
#We include in our fourth model a binary variable equal to one if the borrower is male.
#Thus, the intercept captures the average credit limit for female borrowers (because sex_binary = 0 for female borrowers) and the coefficient on sex_binary captures the incremental credit limit for male borrowers.
#Because the coefficient is negative, male borrowers, on average, have lower credit limits than female borrowers.

#QUESTION 21
#You now run your fifth and final regression model.  Regress credit limit on AGE, sex_binary, and an interaction of AGE and sex_binary.  Fill in the blanks below.  Round to two decimal places.
regression_5 <- lm(credit_limit ~ AGE + sex_binary + AGE * sex_binary, data = New_credit_data)
summary(regression_5)

#QUESTION 22
#Use your fifth regression model results to find the equations for the lines of best fit for male and female borrowers, where AGE is the x-variable and credit limit is the y-variable.  Fill in the blanks below.  Round to two decimal places.  If possible, do not round any intermediate calculations.
#male borrowers:  y = [2.40]x + [75.83]
  #2.40 is AGE (1.8496) plus sex_binary(0.5491)  and 75.83 is the intercept (105.4467) plus sex_binary (-29.6164)
#female borrowers:  y = [1.85]x + [105.45]
  #1.85 is AGE and 105.45 is the intercept

#QUESTION 23
#Use your fifth regression model results to calculate a predicted credit limit for a male borrower who is 35 years old and a female borrower who is 35 years old.  Which predicted credit limit is higher? 
#Then, use your fifth regression model results to calculate a predicted credit limit for a male borrower who is 75 years old and a female borrower who is 75 years old.  Which predicted credit limit is higher? 

#ANSWER:Our fifth model predicts that a 35-year-old female borrower has a credit limit of 170.18 th and a 75-year-old female borrower has a credit limit of 244.17 th.  
#Our fifth model predicts that a 35-year-old male borrower has a credit limit of 159.78 th and a 75-year-old male borrower has a credit limit of 255.73 th.  
#So, at 35 years old, female borrowers have higher average credit limits than male borrowers.  But, at 75 years old, male borrowers have higher average credit limits than female borrowers.

#QUESTION 24
#Does your fifth regression model predict that male borrowers always have higher credit limits than female borrowers of the same age?  Why or why not?  Explain your answer fully.  Use your answer to #17 to support your discussion.

#ANSWER: No, male borrowers do not always have higher credit limits than female borrowers.
#The intercept for the line of best fit for male borrowers is less than the intercept of the line of best fit for female borrowers.
#This suggests that when borrowers are young, for a period of time, female borrowers have higher credit limits.
#However, the slope on age is greater for male borrowers than for female borrowers.
#Thus, as borrowers age, male borrowers have greater increases in credit limit than female borrowers.
#At some point, the lines of best fit for male and female borrowers intersect, with the male borrower line rising above the female borrower line.
#So, in sufficiently older borrowers, male borrowers have higher credit limits than female borrowers.

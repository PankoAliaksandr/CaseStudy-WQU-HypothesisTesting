# Case Study 5

# Description:
#===============================================================================================================
# Task 1: Download data for last 1 year for the S&P500 and any 10 of its component stocks.
# Task 2: Calculate daily returns of the S&P500 index and the downloaded stocks over the period under study
# Task 3: For each of the selected stocks and the index perform a Student’s T test,
#         calculate the p-value and t-value and test the Null Hypothesis that the mean daily stock return is zero

# Question 1: 1. For how many stocks was the null hypothesis accepted.

# Question 2: Given that you drew the stocks randomly from the index constituents,
#             is it possible to extrapolate the behavior of the index (in terms of the null hypothesis)
#             from the average results obtained from analyzing the stocks?
#===============================================================================================================

library(quantmod)
library(PerformanceAnalytics)


# Task 1: Download data
global_data_frame <- read.csv(file = "d:/^GSPC10.csv")

#Convert Date into "YYYY-MM-DD" format
global_data_frame$Date <- as.Date(global_data_frame$Date, "%m/%d/%Y")

#Create time series for all data frame
time_series_character <- xts(x = global_data_frame,order.by = global_data_frame$Date )

#Remove "Date" column
time_series_character$Date <- NULL

#Convert character time series into numeric
time_series_numeric <-as.numeric(time_series_character)
attributes(time_series_numeric) <- attributes(time_series_character)

# Task 2: Daily returns

daily_returns <- Return.calculate(prices = time_series_numeric, method = "discrete")
daily_returns <- daily_returns[-1,]

# Task 3: Student’s T test
p_value_array <- c()
t_value_array <- c()

number_of_t.tests <- ncol(daily_returns)

for(i in 1: number_of_t.tests){
  price_vector <- as.vector(daily_returns[,i])
  ttest_data <-t.test(price_vector,mu = 0)
  p_value <- ttest_data[['p.value']]
  t_value <- as.vector(ttest_data[['statistic']])
  p_value_array <- append(p_value_array,p_value)
  t_value_array <- append(t_value_array,t_value)
}

pt_value_matrix <- matrix(nrow = length(p_value_array), ncol = 2)
pt_value_matrix[,1] <- p_value_array
pt_value_matrix[,2] <- t_value_array 
stocks_names <- colnames(daily_returns)
colnames(pt_value_matrix) <- c('p-value', 't-value')
rownames(pt_value_matrix) <- stocks_names


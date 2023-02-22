#This step aims to check and organize the data set for the exploratory data analysis (EDA). 

#Libraries
library(tidyverse) #collection of data science packages
library(skimr)     #summary statistics
library(lubridate) #manipulation of dates

#Step 1 - Load the CSV file
df<- read.csv("data_gold_prices.csv")
##View
df |> view() #Important variables: Date, Close.Last.

#Step 2 - Data wrangling

##Check the data type
str(df) #Date is defined as char. It must be in date format.

##Data manipulation
#1 - Select the relevant variables
#2 - Rename the variable names
#3 - Casting for the variable associated with the date. It must be in date format.

df <- df |> select(Date, Close.Last) |> 
  rename(period = Date, price = Close.Last) |> 
  mutate(period = mdy(period))

#Note - the price variable selected was the last/closing price of gold.

#View
df |> view() #Date is defined as YYYY-MM-DD

#Step 3 - Set up the time frame - dates between 2020 and 2021

## First date
min(df$period) #2018-02-12
## Last date
max(df$period) #2023-02-10

##Filter between 2020 and 2021
df_final <- df |> filter(between(period, as.Date('2020-01-01'), as.Date('2021-12-31')))

#View
df_final |> view()

#Step 4 - Descriptive statistics
df_final |> skim()

#Total number of values: 505 observations 

##Central tendency:
#Mean - 1789
#Median - 1794

##Statistical dispersion:
#Date of the minimal price (Minimal price - 1478)
df_final |> slice_min(price) |> select(period) #2020-03-18
#Date of the maximum price (Maximum price - 2069)
df_final |> slice_max(price) |> select(period) #2020-08-06
#Range - 591
2069 - 1478
#Standard deviation - 107
#Coefficient of variation - 6%
round(100 * (107/1789),0)

#Comment: 
##In broad words, the data set does not present a high level of variability during 2020 and 2021. This fact ratifies by the coefficient of variation (below 10%), as well as mean and median are close. 

#Step 5 - Save the data frame
write.csv(df_final, "df_clean.csv", row.names = FALSE)

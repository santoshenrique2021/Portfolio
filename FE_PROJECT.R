#Feature engineering process. 

#Libraries
library(tidyverse)
library(lubridate)
library(RcppRoll)
library(correlationfunnel)

#Step 1 - Load the CSV file
df<- as_tibble( read.csv("df_clean.csv"))
##View
df 

#Step 2 - Casting 
##Type Casting - Set Date as a date-time variable
df<- df |> mutate(period = as.Date(period)) 

#Step 3 - Autocorrelation Function (ACF) 
##ACF values
acf(df$price, plot = FALSE)

#Step 4 - Modeling data frame
##Set up the data frame used for the modeling - Transform a time series into a supervised learning problem.
##Create the target.
##Create features - ACF values above 0.75.
##Create features - moving average. 
##Create features - moving median. 

df_fe <- df |> 
  mutate(target =   price,
         price_p1 = lead(target, n=1),
         price_p2 = lead(target, n=2),
         price_p3 = lead(target, n=3),
         price_p4 = lead(target, n=4),
         price_p5 = lead(target, n=5),
         price_p6 = lead(target, n=6),
         price_p7 = lead(target, n=7),
         price_p8 = lead(target, n=8),
         price_p9 = lead(target, n=9),
         price_p10 = lead(target, n=10),
         price_p11 = lead(target, n=11),
         price_p12 = lead(target, n=12),
         price_p13 = lead(target, n=13),
         mean_3 = roll_mean(price_p1, n=3, fill = NA, align = "left"),
         mean_5 = roll_mean(price_p1, n=5, fill = NA, align = "left"),
         mean_10 = roll_mean(price_p1, n=10, fill = NA, align = "left"),
         mean_15 = roll_mean(price_p1, n=15, fill = NA, align = "left"),
         median_5 = roll_median(price_p1, n=5, fill = NA, align = "left"),
         day = as.factor(wday(period)), 
         month = format(period, "%m")
         )

##Remove the price column
df_fe <- df_fe |> select(-price)

##Remove NA Lines
df_fe <- na.omit(df_fe)

#Step 5 - Correlation analysis -target vs numerical variables 
correlation_target_features <- df_fe %>% 
  select(!c(period, day, month)) %>% 
  correlate(target = target)

##Correlation Table
correlation_target_features %>% View()
##Correlation Plot
correlation_target_features %>% plot_correlation_funnel(interactive = TRUE)

#Step 6 - Final visualization
df_fe |> view()

#Step 7 - Save the data frame
write.csv(df_fe, "df_model.csv", row.names = FALSE)

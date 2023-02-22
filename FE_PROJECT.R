#This step represents the feature engineering process. The focus is on creating relevant features for the modeling.

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

#Step 3 - Create features
##Transform a time series into a supervised learning problem and create the features.

df_fe <- df |> 
  mutate(target =   lag(price,  n=1),
         price_p1 = lead(price, n=1),
         price_p2 = lead(price, n=2),
         price_p3 = lead(price, n=3),
         price_p4 = lead(price, n=4),
         price_p5 = lead(price, n=5),
         dif_p1 = price - price_p1,
         dif_p2 = price - price_p2,
         dif_p3 = price - price_p3,
         dif_p4 = price - price_p4,
         dif_p5 = price - price_p5,
         mean_5 = roll_mean(price, n=5, fill = NA, align = "left"),
         median_5 = roll_median(price, n=5, fill = NA, align = "left"),
         sd_5 = roll_sd(price, n=5, fill = NA, align = "left"),
         max_5 = roll_max(price, n=5, fill = NA, align = "left"),
         min_5 = roll_min(price, n=5, fill = NA, align = "left"),
         day = as.factor(wday(period)), 
         month = format(period, "%m")
         )

##Visualization
df_fe |> view()

#Step 4 - Remove NA Lines
df_fe_v2 <- na.omit(df_fe)

##Visualization
df_fe_v2 |> view()

#Step 5 - Correlation analysis -target vs numerical variables 
correlation_target_features <- df_fe_v2 %>% 
  select(!c(period, day, month)) %>% 
  correlate(target = target)

##Correlation Table
correlation_target_features %>% View()
##Correlation Plot
correlation_target_features %>% plot_correlation_funnel(interactive = TRUE)

#Note - These variables present low degree of correlation: dif_p5, dif_p4, dif_p3, dif_p2, dif_p1, and sd_5.

#Step 6 - Final data set - it includes the relevant features and the target
df_fe_v2 |> names()

##Select the relevant variables
df_final<- df_fe_v2 |> select(target, period, day, month, price,
                              price_p1, price_p2, price_p3,
                              price_p4, price_p5, mean_5, 
                              median_5, max_5, min_5)
##View
df_final |> view()

#Step 6 - Save the data frame
write.csv(df_final, "df_model.csv", row.names = FALSE)

#Student: Henrique Santos
##Project: A Case Study of Cielo's Stock Prices Prediction via Random Forest Regression

#Key points:
## 1 - Transform the time series into a supervised learning problem.
### 1.1 - Define the target.
### 1.2 - Define the independent variables via sliding window method.

##Clean up R work space 
rm(list=ls())

##Libraries
library(tidyverse)    #data science 
library(tidymodels)   #machine learning 
library(skimr)        #summary statistics
library(lubridate)    #dates and time
library(RcppRoll)     #rolling operations
library(plotly)       #interactive plot
library(timetk)       #time series analysis
library(modeltime)    #time series forecasting model
library(vip)          #identifies the most important variables 
library(changepoint)  #detect change points in a time series 
library(correlationfunnel) #Exploratory Data Analysis (EDA)
library(Metrics)      #calculate mae



#Step 1 - Import the CSV file
df<- read_csv("HISTORICAL_DATA.csv") 

#Step 2 - View the main information of the data set
glimpse(df)

###Number of observations: 3150
###Number of variables: 7

#Step 3 - Data wrangling: Select and rename the relevant columns

##Relevant Columns:
###Data - it is the date.    
###Ultimo - it is the stock closing price. 
 
df_v1<- df %>% select(Data, Último) %>% 
  rename(period = Data, price = Último)
#At this step, the column names were renamed.

#View data type
str(df_v1)

##Additional tasks:
#1 - period must be in the date format
#2 - price must be divided by 100.

df_v1 = df_v1 %>% mutate(period = as.Date(period, format = "%d.%m.%Y"),
price = price/100)

##Check
str(df_v1)

#Step 4 - Summary statistics of the data set
skim(df_v1)

##Period:
##First date - 2009-06-30
##Last date - 2022-03-24

##Prices:

###Measures of Central Tendency
#mean - 11.3
#median - 9.64

###Measures of Statistical Dispersion
#sd - 6.72
#Coefficient of Variation (CV)
cv = round((6.72/11.3)*100,2) #59.47%

## Comment - since the CV is almost 60%,
##along with the time, the data presented
##high level of volatility around the mean.

#Extreme values
##Min value - 2.01
###Date
df_v1 %>% slice_min(price) %>% select(period) #2022-01-14, 2022-01-10, 2021-12-01

##Max value - 25.2
###Date
df_v1 %>% slice_max(price) %>% select(period) #2015-07-22

#Step 5 - Visualization: Time Series Plot

##Static Plot
p<- ggplot()+
  theme(text = element_text(size=9), plot.title = element_text(face =  "bold"), axis.title.x = element_text(face = "bold"),  
        axis.title.y =   element_text(face = "bold"), panel.background = element_rect(fill = "lightblue", 
        colour = "lightblue", size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = .5, linetype = 'solid',
        colour = "white"),  panel.grid.minor = element_blank()) +
  geom_line(data = df_v1, aes  (x=period, y=price))+ 
  xlab('Period')+
  ylab('Closing Price')+
  geom_hline(yintercept = mean(df_v1$price), color = "red", size = 0.75)+
  scale_x_date(date_labels = "%b %Y")+
  labs(title = "Cielo Historical Stock Prices", caption = "Source: Bovespa", 
       subtitle = "Period: 2009 - 2022")
p #The red line is the mean

##Interactive Plot
p_2<- ggplotly(p) %>% 
layout(title = list(text = paste0('Cielo Historical Stock Prices',
'<br>', '<sup>','Period: 2009 - 2022','</sup>')))
p_2

#Comments
##At first glance, based on the plot it is possible to see that 
##the time series not stationary. Thus, the observation depends on 
##the time that the series is observed. 
##Along with the data, there are two different movements. 
##In the beginning, between 2011 and 2015, the stock prices grew up 
##continuously. On the other hand, after 2018, 
##it is possible to see an opposite trajectory. 

##It is worth mentioning that after October 2018, the prices are below average. 
##This movement goes on until the end of the time series.

#Step 6 - Feature engineering
##Create the target and the set of independent variables

#price         - the current closing stock price, in R$.
#period        - the date 
#target is the forecast for the next day. 

#Lag variables related to price: price_p1, price_p2, price_p3, price_p4, price_p5

#dif_1         - difference between the current price and price_p1
#dif_1_d       - dummy variable. It compares the difference of the current price and price_p1. 
##1 implies that the current value is larger than the previous one, and 0 is the otherwise. 

#day           - day that the price was quoted (categorical)
#month         - month that the price was quoted (categorical)
#year          - year that the price was quoted (categorical)

#mean_5        - rolling mean for five periods
#median_5      - rolling median for five periods

#price_mean_5   - dummy variable. It compares the current price with mean_5. 
##1 implies that the current value is larger than rolling mean for five periods, and 0 is the otherwise. 
#price_median_5 - dummy variable. It compares the current price with median_5. 
##1 implies that the current value is larger than rolling median for five periods, and 0 is the otherwise. 

#price_rate  - growing rate of the price given price_p1

df_v2<- df_v1 %>% mutate(target = lag(price, n = 1)) %>% 
mutate(price_p1 = lead(price, n =1), price_p2 = lead(price, n =2),
price_p3 = lead(price, n =3), price_p4 = lead(price, n =4), 
price_p5 = lead(price, n =5)) %>%
mutate(dif_1 = price - price_p1) %>%   
mutate(dif_1_d = if_else(price>price_p1, "1", "0")) %>%   
mutate(day = as.factor( wday(period)), month = format(period, "%m"),
year = format(period, "%y")) %>% 
mutate(mean_5 = roll_mean(price, n =5, fill = NA, align = "left")) %>% 
mutate(median_5 = roll_median(price, n =5, fill = NA, align = "left")) %>% 
mutate(price_mean_5 = if_else(price< mean_5, "0", "1")) %>% 
mutate(price_median_5 = if_else(price< median_5, "0", "1")) %>% 
mutate(price_rate = (price - price_p1)/price_p1 )
         
#data type
str(df_v2)

#Step 7: Bivariate Analysis
##target vs  numerical variables - Correlation Analysis

###Remove NA lines
df_v2_na<-na.omit(df_v2)
###Correlation
correlation_target_features <- df_v2_na %>% 
select(!c(period, day, month, year, dif_1_d, price_mean_5, price_median_5)) %>% 
correlate(target = target)
###View
#Correlation Table
correlation_target_features %>% View()
#Correlation Plot
correlation_target_features %>% plot_correlation_funnel(interactive = TRUE)

###Strong Variables
#price, price_p1, mean_5, median_5, median_5, price_p2, price_p3
###Weak Variables
#dif_1, price_rate

##target vs independent categorical variables - KS Test
###price_mean_5
kruskal.test(df_v2_na$target~df_v2_na$price_mean_5) #important
###price_median_5
kruskal.test(df_v2_na$target~df_v2_na$price_median_5) #important
###dif_1_d
kruskal.test(df_v2_na$target~df_v2_na$dif_1_d) #important
###year
kruskal.test(df_v2_na$target~df_v2_na$dif_1_d) #important
###month
kruskal.test(df_v2_na$target~df_v2_na$month) #important
###day
kruskal.test(df_v2_na$target~df_v2_na$day) #unimportant

#Step 8 - Machine Learning Modeling

#8.1 - Division of the data set

##Let's divide the data set into two: 

###Modeling - training and testing procedures

###OOT (Out of Time) - It simulates the situation when the model is working, 
#evaluating its performance and stability.
 
##Modeling - Jun (2009) - Fev (2022)
df_m<- df_v2 %>% filter(period < as.Date("2022-03-01"))

#OOT - Mar (2022) 
df_oot<- df_v2 %>% filter(period > as.Date("2022-02-28"))

#Strategy:
##The time series plot shows that the most recent data present minimum values. 
##Therefore, these values must be included in the modeling data frame.
##The model performance will evaluate the performance associated with March.


#8.2 - Train and test split
##Note - The re samples are not random and contain data points that are consecutive values.
set.seed(1935)
df_initial_split<- time_series_split(df_m, assess = "1 month",
lag =  "5 days", slice = 5, cumulative = TRUE)


##Visualize train/test split
df_initial_split %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(period, price, 
  .x_lab = "Period", .y_lab = "Stock Price")

#8.3 - Pre processing for modeling

df_recipe<- recipe(target ~ ., data = df_m) %>% 
  step_rm(dif_1, price_rate, day, period, year) %>% 
  step_normalize(all_numeric_predictors()) %>% 
  step_naomit(all_predictors()) %>% 
  step_dummy(all_nominal_predictors()) 

##view - training data under recipe
df_recipe %>% prep() %>% 
  bake(new_data = df_initial_split) %>% 
  glimpse()

#8.4 - Algorithm 

df_model_rf <- rand_forest(
  min_n = tune(), #Minimal Node Size
  mtry = tune(),  #Randomly Selected Predictors
  trees = tune()  #Trees
) %>% 
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression")

#8.5 - Workflow

df_wf<-
  workflow() %>% 
  add_recipe(df_recipe) %>% 
  add_model(df_model_rf)

#8.6 - Tune

grid_dt<- grid_random(
  min_n(range = c(10,20)),
  mtry(range = c(6,20)),
  trees(range = c(50,200))
)

#8.7 - Cross-Validation (k-fold)
##Cross validation sets for time series
###This function produces a sampling plan starting with the most recent time series observations, rolling backwards

#Date frame of training
df_train <- training(df_initial_split)
#Date frame of testing
df_test <- testing(df_initial_split)

df_resamples<- time_series_cv(
  data        = df_train,
  assess      = "2 years",
  initial     = "5 years",
  skip        = "1 year",
  slice_limit = 4
)

#Visualization of the cross validation sets
df_resamples %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(
    period, price,
    .facet_ncol  = 2,
    .interactive = FALSE
  )

#8.8 - Model tuning

df_tune_grid<- tune_grid(
  df_wf,
  resamples = df_resamples,
  grid = grid_dt,
  metrics = metric_set(mae),
  control = control_grid(verbose = TRUE, allow_par = FALSE))

##Plot the result
autoplot(df_tune_grid)

##Analyze the results for all experiments 
collect_metrics(df_tune_grid)

##Best model
show_best(df_tune_grid, n=1)
#mtry = 19
#trees = 166
#min_n = 17

##mae 1.63 - from training data

##Best model
best_model<-select_best(df_tune_grid, "mae")

##Close workflow
df_wf<- df_wf %>% finalize_workflow(best_model)

##Fit - evaluate the test set based on the training set parameters
df_fit<-last_fit(df_wf, df_initial_split)

##estimated values from the test data
df_test_preds<-
  collect_predictions(df_fit)

###Mae from the test set
observed<-c(df_test_preds$target)
predicted<-c(df_test_preds$.pred)

mae(observed, predicted) #Mae from the test set 0.08

#I did not understand this part
df_last_fit_model<-df_fit$.workflow[[1]]$fit$fit

##Importance of the variables
vip(df_last_fit_model)

df_fit<- df_wf %>% fit(df_train) #put any database


#8.9 Save a RDS file to estimate

saveRDS(df_fit, file = "ts_rfmodel.rds")

#9. Evaluate the model considering the OOT data set

##Run the RDS file
df_fit2<- readRDS("ts_rfmodel.rds")

predict(df_fit2, df_oot)

predict_oot<-predict(df_fit2, df_oot)

##Comparison between Real vs Forecast Values
###Month: March
df_comparison_oot<-data.frame(period = c(df_oot$period),
target = c(df_oot$target), forecast = c(predict_oot$.pred))

View(df_comparison_oot) #note - since the last observation is from 2022-03-24, 
##there is not target for it to be comparing.

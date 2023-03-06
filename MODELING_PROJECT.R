#Modeling

#Libraries
library(tidyverse)   #data manipulation
library(tidymodels)  #machine learning
library(modeltime)   #time series
library(timetk)      #time series  
library(vip)         #top variables

#Step 1 - Open the CSV file
df<- as_tibble(read_csv("df_model.csv"))
df |> nrow() #490 observations
##View
df |> view() 
##Data type
df |> str() #Note - the variable day must be defined as a char

#Step 2 - Casting
df<- df |> mutate(day = as.character(day))
##Check
df |> str()

#Step 3 - Data splitting 
##Training set - Jan 2020 - Nov 2021
##Testing set - Dec 2021

##Split 
gp_split<- time_series_split(df, assess = "33 days", cumulative = TRUE)
gp_split

###Train data set
df_train <- training(gp_split)
df_train |> nrow() #468 observations
df_train |> view()

####Save the data frame
write.csv(df_train, "df_train.csv", row.names = FALSE)

###Test data set
df_test <- testing(gp_split) 
df_test |> nrow() #22 observations
df_test |> view()

####Save the data frame
write.csv(df_test, "df_test.csv", row.names = FALSE)


##Visualization
gp_split %>% 
  tk_time_series_cv_plan() %>% 
  plot_time_series_cv_plan(period, target, 
                           .x_lab = "Period", .y_lab = "Gold price (US$)")

#Step 4 - Recipe
##Data pre-processing
df_recipe<- recipe(target~., data = df) |> 
  step_rm(period) |> 
  step_nzv(all_predictors()) |> 
  step_impute_median(all_numeric_predictors()) |> 
  step_range(all_numeric_predictors()) |> 
  step_impute_mode(all_nominal_predictors()) |> 
  step_dummy(all_nominal_predictors(), one_hot = TRUE)


#Remove the period variable
#Remove variables that are highly sparse and unbalanced
#Replace the numerical missing value for the median 
#Normalize numeric data considering a pre-defined range between 0 and 1 
#Applied one-hot encoding for the categorical variables 
#Replace the categorical missing data for the mode 

##View the recipe considering the training data 
df_recipe %>% prep() %>% 
  bake(new_data = df_train) %>% 
  glimpse()

#Step 5 - Random forest model
rf_model <- rand_forest(
  min_n = tune(), #Number of data points to split
  mtry = tune(),  #Number of sampled predictors
  trees = tune()  #Number of trees
) %>% 
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression")


#Feature Importance - importance = impurity

#Step 6 - Workflow - encapsulate the major pieces of the modeling process
##Recipe + Model
df_wf<-
  workflow() %>% 
  add_recipe(df_recipe) %>% 
  add_model(rf_model)

#Step 7 - Tune (Grid)
##Combination of estimators associated with the random forest - 80 combinations
set.seed(1956)

grid_df<- grid_random(
  min_n(range = c(5,50)),
  mtry(range = c(4,17)),
  trees(range = c(50,500)),
  size = 80
)

##View the combinations
grid_df %>% view()

#Step 8 - Cross Validation
cv_resamples_df<- time_series_cv(
  data        = df_train,
  assess      = "22 days",
  initial     = "1 year",
  skip        = "2 months",
  lag         = 2, 
  slice_limit = 4
)

##Plot: Cross Validation Plan 
cv_resamples_df %>%
  tk_time_series_cv_plan() %>%
  plot_time_series_cv_plan(
    period, target,
    .facet_ncol  = 2,
    .interactive = FALSE
  )

#Step 9 - Model Tuning (Optimization)
doParallel::registerDoParallel()
set.seed(1935)

df_tune_grid<- tune_grid(
  object =  df_wf,
  resamples = cv_resamples_df,
  grid = grid_df,
  metrics = metric_set(yardstick::mape),
  control = control_grid(verbose = TRUE, allow_par = FALSE))

## Results
###Plot
autoplot(df_tune_grid)

###Analyze the results for all experiments 
collect_metrics(df_tune_grid) %>% View()

###Best model parameters
show_best(df_tune_grid, n=1) #MAPE - 0.648
##mtry = 17
##trees = 142
##min_n = 48

#Step 10 - Best model fit
best_model<-select_best(df_tune_grid, "mape")
best_model

##Step 11 - Close workflow
df_wf<- df_wf %>% finalize_workflow(best_model)
df_wf

#Step 12 - Evaluate the model considering the test set 
df_fit<-last_fit(df_wf, gp_split)
##Target X Predict values
df_fit$.predictions |> view()
collect_predictions(df_fit) 
df_support<- collect_predictions(df_fit) 
## Data frame
df_test_pred<- tibble(
  period = c(df_test$period),
  predict_values = c(round(df_support$.pred,1)),
  target = c(df_test$target)
)
df_test_pred
##Save the data frame
write.csv(df_test_pred, "df_predict.csv", row.names = FALSE)

#Step 13- Importance of the variables
df_last_fit_model<-df_fit$.workflow[[1]]$fit$fit
vip(df_last_fit_model)

#Step 14 - Save the model considering the train data 
df_fit<- df_wf %>% fit(df_train)

#Step 15 - Save a RDS file
saveRDS(df_fit, file = "ts_rfmodel.rds")

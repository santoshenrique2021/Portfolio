# The gold price prediction via random forest regression

## Primary objective
This project presents a random forest regression applied to a time series problem. The model predicts the gold price - a daily time series quoted in Nasdaq - in Decemeber 2021.

## Secundary objective
1. Analyzing the characteristics of the time series; 
2. Data visualization of the data set - time series plot and boxplot;
3. Feature engineering to create explanatory variables. 

## Period
The time range of the data set is defined between 2020 and 2021, i.e., during the beginning of the COVID period. 

## Target 
The gold price for the next day.

## Accuracy metric
MAD

# Notes:

1. Gold is usually quoted by the ounce in U.S. dollars;
2. This project will be developed via **tidymodels** and **modeltime**, two R packages;
3. **Source**: https://www.nasdaq.com/market-activity/commodities/gc:cmx

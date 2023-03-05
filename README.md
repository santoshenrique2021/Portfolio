# Project
The gold price prediction via random forest regression.

# Target 
The gold price for the period 't'.

# Period
The time range is defined between 2020 and 2021. 

## Training period
January 2020 - November 2021.

## Testing period
December 2021.

# Objectives

## Primary objective
This project presents a random forest regression applied to a time series problem. The model predicts the gold price - a daily time series quoted in Nasdaq - for December 2021. Under this process, some variables are created to help the prediction.

## Secondary objective
1. Analyze the characteristics of the time series during 2020 and 2021; 
2. Data visualization - time series plot, boxplot, and correlogram;
3. Simulate a production environment for December 2021;
4. Feature engineering to create explanatory variables;
5. Detailing all steps used along with the modeling.

# Accuracy metric
MAPE - Mean absolute percentage error.

# Notes:
1. Gold is usually quoted by the ounce in U.S. dollars;
2. This project will be developed via **tidymodels** and **modeltime**, two R packages;
3. **Source**: https://www.nasdaq.com/market-activity/commodities/gc:cmx

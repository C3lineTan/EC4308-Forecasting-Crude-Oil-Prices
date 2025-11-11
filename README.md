# Comparative Analysis of Machine Learning Models in Forecasting Brent Crude Oil Price Returns

This project is made for the EC4308: Machine Learning and Economic Forecasting module. The main goal of this project is to forecast Brent crude oil prices at multiple horizons, at 1, 3, 6 and 12 months ahead, and to evaluate the relative performance of a wide range of forecasting models. 

We compare the forecasting performance of the following methods:
1. AR(1)
2. Lasso, Ridge and Elastic Net
3. PCR, PCA + Lasso, PLS
4. Decision Trees, Bagging, Random Forests and XGBoost
5. SVR

The code is stored in the following folders: 

| Folder | Description | 
| ---------- | ---------- |
|Data|This folder contains the raw data pulled from FRED-MD before data cleaning and preprocessing. |
|Code|This folder contains the codes used for data cleaning, workhorse R script to obtain results and perform forecasts for every general method, DM test, etc.|
|Model|Here we store the code for the different models. Lasso, Ridge and Elastic Net, PCR and SVR models for all forecast horizons are written in their respective files. The models for tree-based methods are split up into h-month forecasts. The tree methods code also contains the AR(1) model. Finally, the combined tree results and variable importances are saved in a seperate RMarkdown file.|
|Results|The results for model forecasts and evaluation results are stored in either csv or RData format. For tree-based methods, since running the code will take a long time, you may load trees_results.RData instead to obtain the saved variables.|
|1 Month, 3 Month, 6 Month, 12 Month|The full training and test dataset for h-step forecasts is stored in their respective folders. Each folder also contains the 'Results' folder, where we store the predicted vs actual level price forecasts for each method considered. |

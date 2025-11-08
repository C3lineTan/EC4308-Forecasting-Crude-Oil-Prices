source("results_function.R")
library(glmnet)
library(ranger)
library(dplyr)

# ============================================================================
# IMPORT DATA
# ============================================================================
library(readr)
train_1m <- read_csv("../1 Month/train_1m.csv") %>% as.data.frame() %>% # load data as df
  {rownames(.) <- .$date; .} %>% select(-date) # make date the rowname and remove the column 
test_1m <- read_csv("../1 Month/test_1m.csv") %>% as.data.frame() %>% 
  {rownames(.) <- .$date; .} %>% select(-date)
oilprices <- read_csv("../data/only_brent.csv") 
testdates <- read_csv("../1 Month/test_1m.csv") %>% as.data.frame() %>% select(date)

# ============================================================================
# 1. LASSO REGRESSION
# ============================================================================

fit_lasso <- function(train_x, train_y, params) {
  lambda <- if (!is.null(params$lambda)) params$lambda else 0.1
  glmnet(data.matrix(train_x), train_y, alpha = 1, lambda = lambda)
}

predict_lasso <- function(model, test_x) {
  as.vector(predict(model, newx = as.matrix(test_x)))
}


param_grid_lasso <- list(
  lambda = c(0.001, 0.01, 0.1, 0.5, 0.75, 1, 10, 50, 100)
)

results_lasso <- ts_cv_hyperparameter_tuning(
  train_1m, 
  fit_fn = fit_lasso,
  predict_fn = predict_lasso,
  param_grid = param_grid_lasso,
  n_folds = 5, cumulative=TRUE,
  model_name = "Lasso"
)

# ============================================================================
# 2. RIDGE REGRESSION
# ============================================================================

fit_ridge <- function(train_x, train_y, params) {
  lambda <- if (!is.null(params$lambda)) params$lambda else 0.1
  glmnet(data.matrix(train_x), train_y, alpha = 0, lambda = lambda)
}

predict_ridge <- function(model, test_x) {
  as.vector(predict(model, newx = as.matrix(test_x)))
}

param_grid_ridge <- list(
  lambda = c(0.001, 0.01, 0.1, 0.5, 0.75, 1, 10, 50, 100)
)

results_ridge <- ts_cv_hyperparameter_tuning(
  train_1m, 
  fit_fn = fit_ridge,
  predict_fn = predict_ridge,
  param_grid = param_grid_ridge,
  n_folds = 5, cumulative=TRUE,
  model_name = "Ridge"
)

# ============================================================================
# 3. ELASTIC NET
# ============================================================================

fit_elastic_net <- function(train_x, train_y, params) {
  lambda <- if (!is.null(params$lambda)) params$lambda else 0.1
  alpha <- if (!is.null(params$alpha)) params$alpha else 0.5
  glmnet(data.matrix(train_x), train_y, alpha = alpha, lambda = lambda)
}

predict_elastic_net <- function(model, test_x) {
  as.vector(predict(model, newx = as.matrix(test_x)))
}

param_grid_elastic <- list(
  lambda = c(0.01, 0.1, 1.0),
  alpha = c(0.25, 0.5, 0.75)
)

results_elastic <- ts_cv_hyperparameter_tuning(
  train_1m,
  fit_fn = fit_elastic_net,
  predict_fn = predict_elastic_net,
  param_grid = param_grid_elastic,
  n_folds = 5, cumulative=TRUE,
  model_name = "Elastic Net"
)


# ============================================================================
# Testing Rolling Window 
# ============================================================================
#fit the random forest on default settings, ignore ts data
fit_ranger_1m <- function(train_x, train_y, params) { #honestly params kinda useless here
  training_set <- as.data.frame(train_x)
  training_set$y = train_y
  ranger(y~., data = training_set, num.tree = 500, importance = "permutation")
}

predict_ranger_1m <- function(model,test_x){
  predict(model, data = test_x)$predictions
}

ranger_rolling_results_1m <- perform_rolling_forecast(train_1m, test_1m, fit_ranger_1m, predict_ranger_1m, "y_lr_1m")
level_prices_1m <- level_price_results(ranger_rolling_results_1m, oilprices, testdates)
mse_1m_ranger <- MSE(level_prices_1m$pred_level, level_prices_1m$actual_level)


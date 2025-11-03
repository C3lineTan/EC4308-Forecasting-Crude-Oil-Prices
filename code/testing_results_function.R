source("results_function.R")
library(glmnet)

# ============================================================================
# IMPORT DATA
# ============================================================================
library(readr)
train_1m <- read_csv("../1 Month/train_1m.csv")
test_1m <- read_csv("../1 Month/test_1m.csv")

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


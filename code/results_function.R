# load libraries
library(timetk)
library(rsample)
library(dplyr)

## MSE function 
MSE <- function(pred, truth){  
  return(mean((truth - pred)^2)) 
}

## RMSE function
RMSE <- function(pred, truth){  
  return(sqrt(MSE(pred, truth)))
}

###################################
## Directional accuracy function ##
###################################
# pred and truth values must be the price change from time t, not the level price 
directional_accuracy <- function(pred, truth){
  pred_dir = sign(pred)
  truth_dir = sign(truth)
  cm <- table(Predicted = pred_dir, Actual = truth_dir) #confusion matrix
  
  # not including 0 tho, not sure what to do if change/predicted change is 0 
  TP <- sum(pred_dir == 1 & truth_dir == 1)
  TN <- sum(pred_dir == -1 & truth_dir == -1)
  FP <- sum(pred_dir == 1 & truth_dir == -1)
  FN <- sum(pred_dir == -1 & truth_dir == 1)
  
  accuracy <- mean(pred_dir == truth_dir)
  precision <- ifelse((TP + FP) == 0, NA, TP / (TP + FP))
  recall <- ifelse((TP + FN) == 0, NA, TP / (TP + FN))
  specificity <- ifelse((TN + FP) == 0, NA, TN / (TN + FP))
  if (is.na(precision) | is.na(recall) | (precision + recall) == 0) {
    f1 <- NA
  } else {
    f1 <- 2 * precision * recall / (precision + recall)
  }
  
  # return the list of metrics
  list(
    confusion_matrix = cm,
    accuracy = accuracy,
    precision = precision,
    recall = recall,
    F1 = f1,
    counts = c(TP = TP, TN = TN, FP = FP, FN = FN)
  )
}

##################
## Recursive CV ##
##################

# fits the model and make predictions for one fold 
ts_cv_fold <- function(train_x, train_y, val_x, val_y, 
                       fit_fn, predict_fn, params) {
  model <- fit_fn(train_x, train_y, params)
  pred <- predict_fn(model, val_x)
  mse <- MSE(pred, val_y)
  return(mse)
}

# function that splits the training data into recursive rolling window/expanding window 
recursive_split_cv <- function(train_x, n=5, cumulative=TRUE){
  size = nrow(train_x)%/%n
  cv_splits <- time_series_cv(
    train_x,
    initial = size,   # training window size
    assess = 1,    # validation window size
    skip = 1,      # how far to move ahead each iteration
    cumulative = cumulative  # expand the window if TRUE, rolling window if FALSE 
  )
  return (cv_splits)
}

# function runs n folds of recursive cv splits and stores MSE in a vector
recursive_ts_cv <- function(train_x, train_y, fit_fn, predict_fn, params, 
                            n_folds = 5, cumulative=TRUE) {
  cv_splits <- recursive_split_cv(train_x, n = n_folds, cumulative=TRUE)
  mse_values <- c()
  for (i in seq_along(cv_splits$splits)) { # look through each split made
    
    split <- cv_splits$splits[[i]]
    train_idx <- analysis(split)
    val_idx   <- assessment(split)
    
    x_train <- train_idx[, -ncol(train_idx) , drop = FALSE]
    y_train <- train_idx[[ncol(train_idx)]]
    x_val   <- val_idx[, -ncol(val_idx) , drop = FALSE]
    y_val   <- val_idx[[ncol(val_idx)]]
    
    mse = ts_cv_fold(x_train, y_train, x_val, y_val, fit_fn, predict_fn, params)
    mse_values = c(mse_values, mse)
    
  }
  return(mse_values)
}

# main function combined. For each parameter combination, run recursive cv and obtain the MSE vector for the folds. 
# calculates mean MSE and outputs a df of the parameter combinations and mean MSE 
# cummulative = TRUE calls for expanding window, FALSE calls for rolling window
# n_folds specifies the amount of training data we start off with in the first window. 
# eg. n_folds = 5 means we train with 1/5 of the data and make forecasts for the rest 4/5 of the data  
ts_cv_hyperparameter_tuning <- function(train_x, train_y,
                                        fit_fn, predict_fn,
                                        param_grid = NULL,
                                        n_folds = 5, cumulative=TRUE,
                                        model_name = "model"){
  
  # make all parameter combinations based on param_grid
  if (is.null(param_grid) || length(param_grid) == 0) {
    param_combinations <- list(list())
  } else {
    param_combinations <- expand.grid(param_grid, stringsAsFactors = FALSE)
    param_combinations <- split(param_combinations, seq(nrow(param_combinations)))
    param_combinations <- lapply(param_combinations, as.list)
  }
  
  n_combinations <- length(param_combinations) #number of param combinations
  results <- vector("list", n_combinations) #initialize results vector
  
  cat("Testing", n_combinations, "hyperparameter combination(s) for", model_name, "\n")
  
  for (i in seq_along(param_combinations)) {
    params <- param_combinations[[i]]
    
    if (length(params) > 0) {
      cat("Combination", i, "/", n_combinations, ":", 
          paste(names(params), params, sep = "=", collapse = ", "), "\n")
    } else {
      cat("Running model with default parameters\n")
    }
    
    # do recursive time series cv
    fold_mses <- recursive_ts_cv(train_x, train_y, fit_fn, predict_fn, params,
                                  n_folds, cumulative=TRUE)
    
    # calculate mean MSE across the folds
    mean_mse <- mean(fold_mses, na.rm = TRUE)
    
    # store results
    results[[i]] <- c(params, 
                      list(mean_cv_mse = mean_mse
                           #,each_fold_mse = fold_mses
                           ))
    
    cat("Mean CV MSE:", round(mean_mse, 4), "\n")
  }
  
  results_df <- bind_rows(results) #results to data frame
  return(results_df)
}

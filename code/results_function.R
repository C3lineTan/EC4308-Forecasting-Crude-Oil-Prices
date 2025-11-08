# load libraries
library(timetk)
library(rsample)
library(dplyr)
library(parallel)


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

# function that splits the training data
recursive_split_cv <- function(train_data, n = 5, cumulative = TRUE){
  size = nrow(train_data) %/% n
  cv_splits <- rolling_origin(
    train_data,
    initial = size,
    assess = 1,
    skip = 1,
    cumulative = cumulative 
  )
  return (cv_splits)
}

# function runs n folds of recursive cv splits
recursive_ts_cv <- function(train_data, fit_fn, predict_fn, params, 
                            n_folds = 5, cumulative = TRUE) {
  cv_splits <- recursive_split_cv(train_data, 
                                  n = n_folds, 
                                  cumulative = cumulative)
  
  mse_values <- c()
  for (i in seq_along(cv_splits$splits)) { 
    
    split <- cv_splits$splits[[i]]
    
    train_fold_data <- analysis(split)
    val_fold_data   <- assessment(split)
    
    y_col_name <- names(train_fold_data)[ncol(train_fold_data)]
    x_col_names <- setdiff(names(train_fold_data), c(y_col_name, "date"))
    
    x_train <- train_fold_data[, x_col_names, drop = FALSE]
    y_train <- train_fold_data[[y_col_name]]
    x_val   <- val_fold_data[, x_col_names, drop = FALSE]
    y_val   <- val_fold_data[[y_col_name]]
    
    mse = ts_cv_fold(x_train, y_train, x_val, y_val, fit_fn, predict_fn, params)
    mse_values = c(mse_values, mse)
    
  }
  return(mse_values)
}

# main function combined
ts_cv_hyperparameter_tuning <- function(train_data,
                                        fit_fn, predict_fn,
                                        param_grid = NULL,
                                        n_folds, cumulative = TRUE,
                                        model_name = "model"){
  
  if (is.null(param_grid) || length(param_grid) == 0) {
    param_combinations <- list(list())
  } else {
    param_combinations <- expand.grid(param_grid, stringsAsFactors = FALSE)
    param_combinations <- split(param_combinations, seq(nrow(param_combinations)))
    param_combinations <- lapply(param_combinations, as.list)
  }
  
  n_combinations <- length(param_combinations)
  results <- vector("list", n_combinations) 
  
  cat("Testing", n_combinations, "hyperparameter combination(s) for", model_name, "\n")
  
  for (i in seq_along(param_combinations)) {
    params <- param_combinations[[i]]
    set.seed(1 + i)
    
    if (length(params) > 0) {
      cat("Combination", i, "/", n_combinations, ":", 
          paste(names(params), params, sep = "=", collapse = ", "), "\n")
    } else {
      cat("Running model with default parameters\n")
    }
    
    # do recursive time series cv
    fold_mses <- recursive_ts_cv(train_data, fit_fn, predict_fn, params,
                                 n_folds = n_folds, 
                                 cumulative = cumulative)
    
    mean_mse <- mean(fold_mses, na.rm = TRUE)
    
    results[[i]] <- c(params, list(mean_cv_mse = mean_mse))
    
    cat("Mean CV MSE:", round(mean_mse, 6), "\n")
  }
  
  results_df <- bind_rows(results)
  return(results_df)
}


###############################
##  Rolling window forecast  ##
###############################
# rolling forecast function 
# cols_to_remove includes the target column and date column (if exist)
perform_rolling_forecast <- function(train_data, test_data, fit_fn, predict_fn, target_col, params=NULL, cols_to_remove=NULL) {
  
  all_data <- rbind(train_data, test_data)
  n_train <- nrow(train_data)
  n_test <- nrow(test_data)
  
  # empty vector to store predictions
  predictions <- numeric(n_test)
  all_cols_to_remove <- c(cols_to_remove, target_col)
  
  # Loop through each observation in the test set
  for (i in 1:n_test) {
    
    # end index of the current training window
    window_end_index <- n_train + i - 1
    
    # Training data is all data from the start up to the current point
    current_train_data <- all_data[i:window_end_index, ]
    # Test data is just the next single observation
    current_test_data <- all_data[window_end_index + 1, ]
    
    # data split for current train x and y 
    current_train_x <- current_train_data %>% select(-all_of(all_cols_to_remove))
    current_train_y <- current_train_data[[target_col]]
    
    current_test_x <- current_test_data %>% select(-all_of(all_cols_to_remove))
    
    # Re-fit the model on the current training data
    rolling_model <- fit_fn(
      current_train_x,
      current_train_y,
      params = params
    )
    
    # Predict the single next observation
    prediction <- predict_fn(rolling_model, current_test_x)
    predictions[i] <- prediction[1] # Use [1] to ensure it's a single value
    
    if (i %% 10 == 0 || i == n_test) {
      print(paste("Completed rolling forecast for step:", i, "/", n_test))
    }
  }
  
  # Return a dataframe of actuals vs. predicted
  results_df <- tibble(
    actual = test_data[[target_col]],
    predicted = predictions
  )
  
  return(results_df)
}


################################
##  level price MSE function  ##
################################

level_price_results <- function(rolling_results, prices, test_dates) {

  rolling_results <- cbind(rolling_results,test_dates) %>% 
    inner_join(prices, by = "date") %>%
    mutate(pred_level = exp(predicted + log_brent), 
           actual_level = exp(actual + log_brent))

  return (rolling_results)
}
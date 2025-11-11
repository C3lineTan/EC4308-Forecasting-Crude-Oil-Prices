# load libraries
library(tidyverse)
library(glmnet)
library(rsample)
library(dplyr)
library(hdm)
library(purrr)
library(stringr)

# Load helpers (MSE, RMSE, directional_accuracy)
source("code/results_function.R")

# set seed for reproducibility
set.seed(42)

# -----------------------------
# SIS (Sure Independence Screening)
# -----------------------------
select_sis <- function(x_train, y_train){
  n <- nrow(x_train)
  p <- ncol(x_train)
  k <- 10
  #k <- min(p, max(1, round(n / log(n))))   # standard SIS rule
  cors <- apply(x_train, 2, function(col) abs(cor(col, y_train, use = "pairwise.complete.obs")))
  top_idx <- order(cors, decreasing = TRUE)[1:k]
  return(top_idx)
}

# -----------------------------
# Selection helpers using output from ts_cv_hyperparameter_tune()
# -----------------------------
# Select lambda with minimum mean CV MSE
choose_lambda_min <- function(results_df){
  idx <- which.min(results_df$mean_cv_mse)
  as.numeric(results_df$lambda[idx])
}

# Select lambda using 1-SE rule
choose_lambda_1se <- function(results_df, n_folds){
  mse_vals <- results_df$mean_cv_mse
  mse_min <- min(mse_vals, na.rm = TRUE)
  mse_sd_proxy <- sd(results_df$mean_cv_mse, na.rm = TRUE)
  se_proxy <- mse_sd_proxy / sqrt(n_folds)
  threshold <- mse_min + se_proxy
  candidates <- results_df %>% filter(mean_cv_mse <= threshold)
  if(nrow(candidates) == 0) {
    return(choose_lambda_min(results_df))
  }
  chosen <- candidates %>% arrange(desc(lambda)) %>% slice(1)
  as.numeric(chosen$lambda)
}

# Select alpha and lambda for elastic net with minimum mean CV MSE
choose_elastic_min <- function(results_df){
  idx <- which.min(results_df$mean_cv_mse)
  list(alpha = as.numeric(results_df$alpha[idx]), lambda = as.numeric(results_df$lambda[idx]))
}

# Select alpha and lambda for elastic net using 1-SE rule
choose_elastic_1se <- function(results_df, n_folds){
  mse_vals <- results_df$mean_cv_mse
  mse_min <- min(mse_vals, na.rm = TRUE)
  mse_sd_proxy <- sd(results_df$mean_cv_mse, na.rm = TRUE)
  se_proxy <- mse_sd_proxy / sqrt(n_folds)
  threshold <- mse_min + se_proxy
  candidates <- results_df %>% filter(mean_cv_mse <= threshold)
  if(nrow(candidates) == 0){
    return(choose_elastic_min(results_df))
  }
  chosen <- candidates %>% arrange(desc(lambda), alpha) %>% slice(1)
  list(alpha = as.numeric(chosen$alpha), lambda = as.numeric(chosen$lambda))
}

# -----------------------------
# Fit/predict wrappers for glmnet
# -----------------------------
# Fit glmnet model
fit_glmnet <- function(train_x, train_y, params){
  a <- ifelse(is.null(params$alpha), 1, params$alpha)
  lam <- ifelse(is.null(params$lambda), NULL, params$lambda)
  if(is.null(lam)){
    glmnet(as.matrix(train_x), train_y, alpha = a, standardize = FALSE)
  } else {
    glmnet(as.matrix(train_x), train_y, alpha = a, lambda = lam, standardize = FALSE)
  }
}

# Predict at specific lambda
predict_at_s <- function(model, test_x, s){
  as.vector(predict(model, newx = as.matrix(test_x), s = s))
}

# -----------------------------
# ts_cv_hyperparameter_tune
# -----------------------------
# Time series cross-validation for hyperparameter tuning
# 5 folds expanding window
ts_cv_hyperparameter_tune <- function(train_data, fit_fn, predict_fn, param_grid,
                                        n_folds = 5, cumulative = TRUE, model_name = "model"){
  target_col <- tail(names(train_data), 1)
  X_all <- as.matrix(train_data %>% select(-date, -all_of(target_col)))
  y_all <- train_data[[target_col]]
  n <- nrow(X_all)
  if(n_folds < 2) stop("n_folds must be >= 2")
  
  param_names <- names(param_grid)
  grid_df <- expand.grid(param_grid, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  block_size <- floor(n / (n_folds + 1))
  fold_ends <- (1:n_folds) * block_size
  results_list <- vector("list", nrow(grid_df))
  
  for(g in seq_len(nrow(grid_df))){
    params <- as.list(grid_df[g, , drop = FALSE])
    fold_mse <- numeric(length(fold_ends))
    for(k in seq_along(fold_ends)){
      train_end <- fold_ends[k]
      val_start <- train_end + 1
      val_end <- min(train_end + block_size, n)
      if(val_start > val_end){
        fold_mse[k] <- NA_real_
        next
      }
      x_train <- X_all[1:train_end, , drop = FALSE]
      y_train <- y_all[1:train_end]
      x_val <- X_all[val_start:val_end, , drop = FALSE]
      y_val <- y_all[val_start:val_end]
      
      model <- tryCatch(fit_fn(x_train, y_train, params), error = function(e) NULL)
      if(is.null(model)){
        fold_mse[k] <- NA_real_
        next
      }
      y_pred <- tryCatch(predict_fn(model, x_val), error = function(e) rep(NA_real_, length(y_val)))
      fold_mse[k] <- mean((y_val - y_pred)^2, na.rm = TRUE)
    }
    mean_mse <- mean(fold_mse, na.rm = TRUE)
    results_list[[g]] <- c(as.list(params), list(mean_cv_mse = mean_mse))
  }
  
  results_df <- bind_rows(results_list)
  if(!"lambda" %in% names(results_df)) results_df$lambda <- NA_real_
  if(!"alpha" %in% names(results_df)) results_df$alpha <- NA_real_
  
  return(results_df)
}

# -----------------------------
# Rolling forecast with feature tracking
# -----------------------------
# Perform rolling forecast (fixed window) on test data
rolling_forecast <- function(train_data, test_data, fit_fn, predict_fn, target_col, params=NULL, cols_to_remove=NULL){
  all_data <- rbind(train_data, test_data)
  n_train <- nrow(train_data)
  n_test <- nrow(test_data)
  predictions <- numeric(n_test)
  selected_features_list <- vector("list", n_test)
  all_cols_to_remove <- c(cols_to_remove, target_col)
  
  for(i in 1:n_test){
    window_end_index <- n_train + i - 1
    current_train_data <- all_data[i:window_end_index, ]
    current_test_data <- all_data[window_end_index + 1, ]
    current_train_x <- current_train_data %>% select(-all_of(all_cols_to_remove))
    current_train_y <- current_train_data[[target_col]]
    current_test_x <- current_test_data %>% select(-all_of(all_cols_to_remove))
    
    rolling_model <- fit_fn(current_train_x, current_train_y, params=params)
    prediction <- predict_fn(rolling_model, current_test_x)
    predictions[i] <- prediction[1]
    
    coefs <- coef(rolling_model)
    selected <- rownames(coefs)[coefs[,1] != 0]
    selected <- setdiff(selected, "(Intercept)")
    selected_features_list[[i]] <- selected
    
    if(i %% 10 == 0 || i == n_test) message("Completed rolling forecast for step: ", i, "/", n_test)
  }
  
  results_df <- tibble(
    date = test_data$date,  
    actual = test_data[[target_col]],
    predicted = predictions,
    selected_features = selected_features_list
  )
  return(results_df)
}

# -----------------------------
# Metrics computation
# -----------------------------
# Get test dates based on horizon label
get_testdates <- function(horizon_label) {
  folder <- if (horizon_label == "1 Month") {
    "1 Month"
  } else if (horizon_label == "3 Month") {
    "3 Month"
  } else if (horizon_label == "6 Month") {
    "6 Month"
  } else if (horizon_label == "12 Month") {
    "12 Month"
  } else {
    stop(paste("Unknown horizon label:", horizon_label))
  }

  file_name <- paste0("test_", gsub(" Month","", horizon_label), "m.csv")
  read_csv(file.path(folder, file_name), show_col_types = FALSE) %>%
    as.data.frame() %>%
    dplyr::select(date)
}
VAR <- function(rolling_results, confidence_level = 0.95) {
  strategy_returns <- sign(rolling_results$predicted)*rolling_results$actual
  probability <- 1-confidence_level
  var_metric <- quantile(strategy_returns, prob = probability, na.rm = TRUE)
  tail_losses <- strategy_returns[strategy_returns <= var_metric]
  cvar_metric <- mean(tail_losses, na.rm = TRUE)
  tibble(
    VaR_95 = var_metric,
    CVaR_95 = cvar_metric,
    Mean_Strategy_Return = mean(strategy_returns, na.rm = TRUE))
}
# Compute metrics from rolling forecast results
compute_metrics <- function(rolling_results, target_col) {
    # Convert "y_lr_1m" → "1 Month"
    horizon_label <- if (str_detect(target_col, "1m$")) {
    "1 Month"
  } else if (str_detect(target_col, "3m$")) {
    "3 Month"
  } else if (str_detect(target_col, "6m$")) {
    "6 Month"
  } else if (str_detect(target_col, "12m$")) {
    "12 Month"
  } else {
    stop(paste("Unknown target horizon:", target_col))
  }

  oilprices <- read_csv("data/only_brent.csv", show_col_types = FALSE) 
  testdates <- get_testdates(horizon_label)
  pred <- rolling_results$predicted
  actual <- rolling_results$actual
  VAR <- VAR(rolling_results)
  VaR_95 <- VAR$VaR_95
  CVaR_95 <- VAR$CVaR_95
  Mean_Strategy_Return <- VAR$Mean_Strategy_Return
  MSE_val <- MSE(pred, actual)
  RMSE_val <- RMSE(pred, actual)
  level_prices <- level_price_results(rolling_results, oilprices, testdates)
  level_mse <- MSE(level_prices$pred_level, level_prices$actual_level)
  da <- directional_accuracy(pred, actual)
  accuracy <- da$accuracy
  precision  <- da$precision
  recall <- da$recall  
  F1 <- da$F1
  TP <- da$counts["TP"] 
  TN <- da$counts["TN"]
  FP <- da$counts["FP"]
  FN <- da$counts["FN"]
  accuracy <- da$accuracy
  Num_Selected <- round(median(sapply(rolling_results$selected_features, length)),1)
  tibble(VaR_95 = VaR_95, CVaR_95 = CVaR_95, Mean_Strategy_Return = Mean_Strategy_Return,MSE = MSE_val, RMSE = RMSE_val, Level_MSE = level_mse, Accuracy=accuracy, Precision=precision, Recall=recall, F1=F1, TP=TP, TN=TN, FP=FP, FN=FN, # collapse list
         Num_Selected = Num_Selected)
}

# -----------------------------
# Run models for one horizon
# -----------------------------
# Run LASSO, RIDGE, ELASTIC NET for one horizon using time series CV
run_models_for_horizon_using_ts_cv <- function(train_path, test_path, target_col,
                                               use_sis = FALSE, selection = c("min","1se"),
                                               lambda_grid_lasso = 10^seq(-6, -2, length.out = 30),
                                               lambda_grid_ridge = 10^seq(-3, 0, length.out = 30),
                                               lambda_grid_elastic = 10^seq(-5, -1, length.out = 30),
                                               alpha_grid = c(0.5),
                                               n_folds = 5){
  selection <- match.arg(selection)
  train_df <- read.csv(train_path, stringsAsFactors = FALSE)
  test_df  <- read.csv(test_path, stringsAsFactors = FALSE)
  y_train <- train_df[[target_col]]
  x_train_full <- train_df %>% select(-all_of(target_col), -date)
  if(use_sis){
    sis_idx <- select_sis(as.matrix(x_train_full), y_train)
    sis_colnames <- colnames(x_train_full)[sis_idx]
    x_train_full <- x_train_full[, sis_colnames, drop=FALSE]
    train_df <- train_df %>% select(all_of(sis_colnames), target_col, date)
    test_df <- test_df %>% select(all_of(sis_colnames), target_col, date)
  }
  
  train_for_cv <- bind_cols(date = train_df$date, as.data.frame(x_train_full), !!target_col := y_train)
  # -----------------------------
  # LASSO
  # -----------------------------
  # Time series CV hyperparameter tuning for LASSO
  results_lasso <- ts_cv_hyperparameter_tune(
    train_data = train_for_cv,
    fit_fn = fit_glmnet,
    predict_fn = function(model, newx) predict_at_s(model, newx, s=model$lambda),
    param_grid = list(lambda = lambda_grid_lasso),
    n_folds = n_folds, cumulative = TRUE
  )
  best_lambda_lasso <- if(selection=="min") choose_lambda_min(results_lasso) else choose_lambda_1se(results_lasso, n_folds)
  lasso_full_model <- fit_glmnet(as.matrix(x_train_full), y_train, list(alpha=1, lambda=best_lambda_lasso))
  num_selected_lasso <- sum(coef(lasso_full_model, s=best_lambda_lasso) != 0) - 1
  
  # -----------------------------
  # RIDGE
  # -----------------------------
  # Time series CV hyperparameter tuning for RIDGE
  results_ridge <- ts_cv_hyperparameter_tune(
    train_data = train_for_cv,
    fit_fn = function(x,y,params) fit_glmnet(x,y,list(alpha=0, lambda=params$lambda)),
    predict_fn = function(model, newx) predict_at_s(model, newx, s=model$lambda),
    param_grid = list(lambda = lambda_grid_ridge),
    n_folds = n_folds, cumulative = TRUE
  )
  best_lambda_ridge <- if(selection=="min") choose_lambda_min(results_ridge) else choose_lambda_1se(results_ridge, n_folds)
  ridge_full_model <- fit_glmnet(as.matrix(x_train_full), y_train, list(alpha=0, lambda=best_lambda_ridge))
  num_selected_ridge <- sum(coef(ridge_full_model, s=best_lambda_ridge) != 0) - 1
  
  # -----------------------------
  # ELASTIC NET
  # -----------------------------
  # Time series CV hyperparameter tuning for ELASTIC NET
  results_elastic <- ts_cv_hyperparameter_tune(
    train_data = train_for_cv,
    fit_fn = fit_glmnet,
    predict_fn = function(model, newx) predict_at_s(model, newx, s=model$lambda),
    param_grid = list(alpha=alpha_grid, lambda=lambda_grid_elastic),
    n_folds = n_folds, cumulative = TRUE
  )
  best_elastic <- if(selection=="min") choose_elastic_min(results_elastic) else choose_elastic_1se(results_elastic, n_folds)
  elastic_full_model <- fit_glmnet(as.matrix(x_train_full), y_train, list(alpha=best_elastic$alpha, lambda=best_elastic$lambda))
  num_selected_elastic <- sum(coef(elastic_full_model, s=best_elastic$lambda) != 0) - 1
  
  tuned_params <- list(
    lasso = list(lambda=best_lambda_lasso),
    ridge = list(lambda=best_lambda_ridge),
    elastic = list(alpha=best_elastic$alpha, lambda=best_elastic$lambda)
  )
  
  all_cols_to_remove <- c(if(use_sis) setdiff(colnames(x_train_full), sis_colnames) else NULL, target_col, "date")
  # -----------------------------
  # Rolling forecast
  # -----------------------------
  lasso <- rolling_forecast(train_df, test_df,
                                            fit_fn=function(x,y,params) glmnet(as.matrix(x), y, alpha=1, lambda=params$lambda, standardize=FALSE),
                                            predict_fn=function(model,newx) predict_at_s(model,newx,s=model$lambda),
                                            target_col=target_col, params=list(lambda=tuned_params$lasso$lambda),
                                            cols_to_remove=all_cols_to_remove) 
  # Save predictions
  write_csv(lasso %>% select(date, actual, predicted),
          paste0("results/", target_col, "_LASSO_", selection, "_", ifelse(use_sis,"SIS","NOSIS"), ".csv"))
  # Compute performance metrics
  lasso_results <- lasso  %>% select(-date) %>% compute_metrics(target_col=target_col)

  ridge <- rolling_forecast(train_df, test_df,
                                            fit_fn=function(x,y,params) glmnet(as.matrix(x), y, alpha=0, lambda=params$lambda, standardize=FALSE),
                                            predict_fn=function(model,newx) predict_at_s(model,newx,s=model$lambda),
                                            target_col=target_col, params=list(lambda=tuned_params$ridge$lambda),
                                            cols_to_remove=all_cols_to_remove) 
  # Save predictions
  write_csv(ridge %>% select(date, actual, predicted),
          paste0("results/", target_col, "_RIDGE_", selection, "_", ifelse(use_sis,"SIS","NOSIS"), ".csv"))
  # Compute performance metrics
  ridge_results <- ridge %>% select(-date) %>% compute_metrics(target_col=target_col)

  elastic <- rolling_forecast(train_df, test_df,
                                              fit_fn=function(x,y,params) glmnet(as.matrix(x), y, alpha=params$alpha, lambda=params$lambda, standardize=FALSE),
                                              predict_fn=function(model,newx) predict_at_s(model,newx,s=model$lambda),
                                              target_col=target_col, params=list(alpha=tuned_params$elastic$alpha, lambda=tuned_params$elastic$lambda),
                                              cols_to_remove=all_cols_to_remove)
  # Save predictions
  write_csv(elastic %>% select(date, actual, predicted),
          paste0("results/", target_col, "_ELASTIC_", selection, "_", ifelse(use_sis,"SIS","NOSIS"), ".csv"))
  # Compute performance metrics
  elastic_results <- elastic %>% select(-date) %>% compute_metrics(target_col=target_col)

  list(
    LASSO = lasso_results,
    RIDGE = ridge_results,
    ELASTIC_NET = elastic_results,
    tuned_params = tuned_params
  )
}



# -----------------------------
# Run all horizons with/without SIS and min/1se selection
# -----------------------------
run_all_horizons <- function(horizons, targets, base_dir = ".",
                             use_sis_options = c(FALSE, TRUE), selection_options = c("min","1se"),
                             lambda_grid_lasso = 10^seq(-6, -2, length.out = 30),
                             lambda_grid_ridge = 10^seq(-3, 0, length.out = 30),
                             lambda_grid_elastic = 10^seq(-5, -1, length.out = 30),
                             alpha_grid = c(0.1, 0.3, 0.5, 0.7, 0.9), n_folds = 5
                             ){
  results_all <- list()
  for(i in seq_along(horizons)){
    cat("\n============================\nRunning:", horizons[i], "\n============================\n")
    train_path <- file.path(base_dir, horizons[i], paste0("train_", substr(targets[i], 6, nchar(targets[i])), ".csv"))
    test_path  <- file.path(base_dir, horizons[i], paste0("test_",  substr(targets[i], 6, nchar(targets[i])), ".csv"))

    results_all[[horizons[i]]] <- list()
    for(sis_flag in use_sis_options){
      for(sel in selection_options){
        cfg_name <- paste0(ifelse(sis_flag, "SIS_", "NO_SIS_"), toupper(sel))
        results_all[[horizons[i]]][[cfg_name]] <- run_models_for_horizon_using_ts_cv(
          train_path, test_path, targets[i],
          use_sis = sis_flag, selection = sel,
          lambda_grid_lasso = lambda_grid_lasso,
          lambda_grid_ridge = lambda_grid_ridge,
          lambda_grid_elastic = lambda_grid_elastic,
          alpha_grid = alpha_grid,
          n_folds = n_folds
        )
      }
    }
  }
  return(results_all)
}

# -----------------------------
# Example call (adjust grids/h_steps/test_window as needed)
# -----------------------------
horizons <- c("1 Month", "3 Month", "6 Month", "12 Month")
targets  <- c("y_lr_1m", "y_lr_3m", "y_lr_6m", "y_lr_12m")

lambda_grid_lasso   <- 10^seq(-6, -2, length.out = 50)
lambda_grid_ridge   <- 10^seq(-3, 0, length.out = 50)
lambda_grid_elastic <- 10^seq(-5, -1, length.out = 50)
alpha_grid <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9)

# For fixed rolling training window over combined train+prior-test rows set test_window to integer (e.g. 1000).
# If test_window = NULL -> expanding (include all previous data).
results_all <- run_all_horizons(
  horizons, targets, base_dir = ".",
  use_sis_options = c(FALSE, TRUE), selection_options = c("min","1se"),
  lambda_grid_lasso = lambda_grid_lasso,
  lambda_grid_ridge = lambda_grid_ridge,
  lambda_grid_elastic = lambda_grid_elastic,
  alpha_grid = alpha_grid, n_folds = 5
)

#saveRDS(results_all, "results/lasso_ridge_en_results.rds")

# -----------------------
# Build performance table
# -----------------------
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

extract_results <- function(results) {
  bind_rows(lapply(names(results), function(horizon_name) {
    horizon_data <- results[[horizon_name]]
    bind_rows(lapply(names(horizon_data), function(sis_lambda_name) {
      group_data <- horizon_data[[sis_lambda_name]]
      model_names <- setdiff(names(group_data), c("tuned_params","sis_features"))
      bind_rows(lapply(model_names, function(model_name) {
        metrics_tbl <- group_data[[model_name]]
        tibble(
          horizon = horizon_name,
          SIS_lambda = sis_lambda_name,
          model = model_name,
          VaR_95 = metrics_tbl$VaR_95[1],
          CVaR_95 = metrics_tbl$CVaR_95[1],
          Mean_Strategy_Return = metrics_tbl$Mean_Strategy_Return[1], 
          MSE = metrics_tbl$MSE[1],
          RMSE = metrics_tbl$RMSE[1],
          Level_MSE = metrics_tbl$Level_MSE[1],
          Accuracy = metrics_tbl$Accuracy[1],
          Precision = metrics_tbl$Precision[1],
          Recall = metrics_tbl$Recall[1],
          F1 = metrics_tbl$F1[1],
          TP = metrics_tbl$TP[1],
          TN = metrics_tbl$TN[1],
          FP = metrics_tbl$FP[1],
          FN = metrics_tbl$FN[1],
          Num_Selected = metrics_tbl$Num_Selected[1]
        )
      }))
    }))
  })) %>%
    mutate(
      SIS = ifelse(str_detect(SIS_lambda, "^NO_SIS"), "No SIS", "SIS"),
      lambda_rule = ifelse(str_detect(SIS_lambda, "MIN$"), "min", "1se")
    ) %>%
    select(-SIS_lambda) %>%
    mutate(
      SIS = factor(SIS, levels = c("No SIS", "SIS")),
      lambda_rule = factor(lambda_rule, levels = c("min","1se")),
      model = factor(model, levels = c("LASSO","POST_LASSO","RIDGE","ELASTIC_NET"))
    )
}

performance_table <- extract_results(results_all)

tuned_lookup <- bind_rows(lapply(names(results_all), function(horizon_name){
  horizon_data <- results_all[[horizon_name]]
  bind_rows(lapply(names(horizon_data), function(sis_lambda_name){
    tp <- horizon_data[[sis_lambda_name]]$tuned_params
    tibble(
      horizon = horizon_name,
      SIS_lambda = sis_lambda_name,
      tuned_lasso = tp$lasso %||% NA,
      tuned_post_lasso = tp$post_lasso %||% NA,
      tuned_ridge = tp$ridge %||% NA,
      tuned_elastic_alpha = if(!is.null(tp$elastic$alpha)) tp$elastic$alpha else NA,
      tuned_elastic_lambda = if(!is.null(tp$elastic$lambda)) tp$elastic$lambda else NA
    )
  }))
}))

performance_table <- performance_table %>%
  left_join(
    tuned_lookup %>% mutate(SIS = ifelse(str_detect(SIS_lambda, "^NO_SIS"), "No SIS", "SIS"),
                            lambda_rule = ifelse(str_detect(SIS_lambda, "MIN$"), "min", "1se")) %>%
      select(-SIS_lambda),
    by = c("horizon","SIS","lambda_rule")
  ) %>%
  mutate(
    best_hyperparam = case_when(
      model == "LASSO" ~ as.character(tuned_lasso),
      model == "POST_LASSO" ~ as.character(tuned_post_lasso),
      model == "RIDGE" ~ as.character(tuned_ridge),
      model == "ELASTIC_NET" ~ paste0("alpha=", tuned_elastic_alpha, ", lambda=", tuned_elastic_lambda),
      TRUE ~ NA_character_
    )
  ) %>%
  select(horizon, model, VaR_95, CVaR_95, Mean_Strategy_Return, MSE, RMSE, Level_MSE, Accuracy, Precision, Recall, F1, TP, TN, FP, FN, Num_Selected, best_hyperparam, SIS, lambda_rule)

write.csv(performance_table, "results/lasso_ridge_en_results.csv", row.names = FALSE)
print(performance_table)

performance_transposed <- performance_table %>%
  # combine model, SIS, lambda_rule into one row identifier
  mutate(model_cfg = paste(model, SIS, lambda_rule, sep = "_")) %>%
  select(horizon, model_cfg, Level_MSE) %>%
  pivot_wider(
    names_from = horizon,      # horizons become columns
    values_from = Level_MSE    # values = Level_MSE
  )

performance_transposed
write.csv(performance_transposed, "results/lasso_ridge_en_results_mse.csv", row.names = FALSE)

performance_transposed2 <- performance_table %>%
  # combine model, SIS, lambda_rule into one row identifier
  mutate(model_cfg = paste(model, SIS, lambda_rule, sep = "_")) %>%
  select(horizon, model_cfg, Accuracy) %>%
  pivot_wider(
    names_from = horizon,      # horizons become columns
    values_from = Accuracy    # values = Level_MSE
  )

performance_transposed2
write.csv(performance_transposed2, "results/lasso_ridge_en_results_accuracy.csv", row.names = FALSE)

performance_transposed3 <- performance_table %>%
  # combine model, SIS, lambda_rule into one row identifier
  mutate(model_cfg = paste(model, SIS, lambda_rule, sep = "_")) %>%
  select(horizon, model_cfg, VaR_95) %>%
  pivot_wider(
    names_from = horizon,      # horizons become columns
    values_from = VaR_95    # values = Level_MSE
  )

performance_transposed3
write.csv(performance_transposed3, "results/lasso_ridge_en_results_VaR.csv", row.names = FALSE)


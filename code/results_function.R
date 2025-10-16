## MSE function 
MSE <- function(pred, truth){  
  return(mean((truth - pred)^2)) 
}

## RMSE function
RMSE <- function(pred, truth){  
  return(sqrt(MSE(pred, truth)))
}

## Directional accuracy function 
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
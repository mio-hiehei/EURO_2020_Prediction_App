cross_val_function <- function(k, df, model){
  
  n <- nrow(df)
  
  data_list <- split(df, sample(rep(1:ceiling(n),  each = ceiling(n / k), length.out = n)))
  
  iter_cv <- function(df){
  
    df$predictions <- predict(model, newdat = df, type = "class")
    
    percent_correctly_predicted <- sum(diag(as.matrix(table(df$predictions, df$result))) ) / nrow(df)
    return(percent_correctly_predicted)
  
  }
  
  cv_outcome <- sapply(data_list, iter_cv)
  
  mean_cv <- mean(cv_outcome)
  
  return(list(cv_outcome = cv_outcome,
              mean_cv = mean_cv))

}

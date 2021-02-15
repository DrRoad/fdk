
optim_ts <- function(.data, ts_model = character(), optim_conf = list(), parameter = list(), export_fit = FALSE){
  if(is.null(ts_model)){
    stop("No model selected")
  } else {
    train_index <- 1:(nrow(.data) - (optim_conf$test_size + lag) + 1)
    test_index <- (nrow(.data) - optim_conf$test_size + 1):nrow(.data)
    
    out <- map(1:test_size, function(x){
      fit <- fit_ts(.data = .data[1:(length(train_index) + x),], ts_model = ts_model, parameter = parameter)
      fitted <- fitted(fit)
      predicted <- predict(object = fit, newdata = .data[test_index,], type = "response")
      error <- .data[test_index,]$y_var - predicted
      out <- list(fit = fit, fitted = fitted, predicted = predicted
                  , error = error
                  , observed = .data[test_index,][["y_var"]])
      return(out)
    })
    
    optim_out_t <- transpose(out)
    fitted_matrix <- plyr::ldply(optim_out_t$fitted, rbind)
    error_matrix <- matrix(unlist(optim_out_t$error), ncol = .test_size, byrow = T)
    predicted_matrix <- matrix(unlist(optim_out_t$predicted), ncol = .test_size, byrow = T)
    observed_matrix <- matrix(unlist(optim_out_t$observed), ncol = .test_size, byrow = T)
    mape_matrix <- abs(error_matrix)/predicted_matrix
    spa_matrix <- observed_matrix/predicted_matrix
    mape_vec <- abs(diag(error_matrix))/diag(observed_matrix)
    spa_vec <- diag(observed_matrix)/diag(predicted_matrix)
    mape <- sum(abs(diag(error_matrix)))/sum(diag(observed_matrix))
    spa <- sum(diag(observed_matrix))/sum(diag(predicted_matrix))
    mse <- sum((diag(error_matrix))^2)/test_size
    mae <- sum(abs(diag(error_matrix)))/test_size
    
    optim_out <- list(
      model = ts_model
      , fitted_matrix = round(fitted_matrix, 3)
      , error_matrix = round(error_matrix, 3)
      , predicted_matrix = round(predicted_matrix, 3)
      , mape_matrix = round(mape_matrix, 3)
      , spa_matrix = round(spa_matrix, 3)
      , mape_vec = round(mape_vec, 3)
      , spa_vec = round(spa_vec, 3)
      , mape = round(mape, 3)
      , spa = round(spa, 3)
      , mse = round(mse, 3)
      , mae = round(mae, 3)
    )
    
    class(optim_out) <- "optim_ts"
    
    if(export_fit == TRUE){
      append(optim_out, values = list(fit = optim_out_t$fit), after = 0)
    } else {
      optim_out
    }
  }
}

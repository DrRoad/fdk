
optim_ts <- function(.data, ts_model = character(), optim_conf = list(), parameter = list(), export_fit = FALSE){
  if(is.null(ts_model)){
    stop("No model selected")
  } else {
    if((nrow(.data) - optim_conf$test_size - optim_conf$lag)<13 & 
       ts_model %in% c("glmnet", "gam", "svm")){
      #message(paste0("Time series is too short for optimization for ", ts_model, " model."))
      return(NULL)
    } else {
      train_index <- 1:(nrow(.data) - (optim_conf$test_size + optim_conf$lag) + 1)
      test_index <- (nrow(.data) - optim_conf$test_size + 1):nrow(.data)
      
      out <- map(1:optim_conf$test_size, function(x){
        train <- .data[1:(length(train_index) + x),]
        fit <- fit_ts(.data = train, ts_model = ts_model, parameter = parameter)
        fitted_values <- predict_ts(fit, .data = train
                                    , optim_conf = optim_conf
                                    , parameter = parameter
                                    , ts_model = ts_model
                                    , add_fitted = TRUE
                                    , type = "response")
        predicted <- predict_ts(fit = fit, .data = .data
                                , parameter = parameter
                                , optim_conf = optim_conf
                                , ts_model = ts_model
                                , add_fitted = FALSE
                                , type = "response")
        error <- .data[test_index,]$y_var - predicted
        out <- list(fit = fit, fitted_values = fitted_values, predicted = predicted
                    , error = error
                    , observed = .data[test_index,][["y_var"]])
        return(out)
      })
      
      optim_out_t <- transpose(out)
      fitted_matrix <- plyr::ldply(optim_out_t$fitted_values, rbind)
      error_matrix <- matrix(unlist(optim_out_t$error), ncol = optim_conf$test_size, byrow = T)
      predicted_matrix <- matrix(unlist(optim_out_t$predicted), ncol = optim_conf$test_size, byrow = T)
      observed_matrix <- matrix(unlist(optim_out_t$observed), ncol = optim_conf$test_size, byrow = T)
      mape_matrix <- abs(error_matrix)/predicted_matrix
      spa_matrix <- observed_matrix/predicted_matrix
      mape_vec <- abs(diag(error_matrix))/diag(observed_matrix)
      spa_vec <- diag(observed_matrix)/diag(predicted_matrix)
      mape <- sum(abs(diag(error_matrix)))/sum(diag(observed_matrix))
      spa <- sum(diag(observed_matrix))/sum(diag(predicted_matrix))
      mse <- sum((diag(error_matrix))^2)/optim_conf$test_size
      mae <- sum(abs(diag(error_matrix)))/optim_conf$test_size
      
      optim_out <- list(
        model = ts_model
        , key = attributes(.data)[["key"]]
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
      
      if(export_fit == TRUE){
        optim_out <- append(optim_out, values = list(fit = optim_out_t$fit), after = 0)
        class(optim_out) <- c("list", "optim_ts")
        optim_out
      } else {
        class(optim_out) <- c("list", "optim_ts")
        optim_out
      }
    }
  }
}

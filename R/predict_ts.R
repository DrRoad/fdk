predict_ts <- function(fit, .data, ts_model, parameter, optim_conf, add_fitted = logical(), type="response"){
  train_index <- 1:(nrow(.data) - (optim_conf$test_size + optim_conf$lag) + 1)
  test_index <- (nrow(.data) - optim_conf$test_size + 1):nrow(.data)
  
  if(ts_model == "glmnet"){
    if(add_fitted == T){
      train <- .data
      key_int <- attributes(train)[["key"]]
      features <- setdiff(names(train), c("date_var", "y_var", unlist(parameter$glmnet$excluded_features)))
      features_cont <- features[unlist(lapply(features, FUN = function(x) is.numeric(train[[x]])))]
      features_factor <- setdiff(features, c(features_cont))
      features_matrix <- train %>%
        dplyr::select(all_of(features)) %>% 
        fastDummies::dummy_cols(select_columns = features_factor
                                , remove_selected_columns = T
                                , remove_first_dummy = T) %>% 
        as.matrix()
    } else {
      #test <- .data
      test <- .data[test_index, ]
      key_int <- attributes(test)[["key"]]
      features <- setdiff(names(test), c("date_var", "y_var", unlist(parameter$glmnet$excluded_features)))
      features_cont <- features[unlist(lapply(features, FUN = function(x) is.numeric(test[[x]])))]
      features_factor <- setdiff(features, c(features_cont))
      trend_decay_tmp <- get_trend_decay(y_var_length = (max(train_index) + 1)
                                         , trend_decay = parameter$glmnet$trend_decay
                                         , horizon = (length(test_index) + optim_conf$lag - 2)
                                         , lag = optim_conf$lag) %>% 
        round(2) %>% 
        .[2:length(.)]
      
      test <- test %>% 
        mutate(trend = trend_decay_tmp)
      
      features_matrix <- test %>%
        dplyr::select(all_of(features)) %>% 
        fastDummies::dummy_cols(select_columns = features_factor
                                , remove_selected_columns = T
                                , remove_first_dummy = T) %>% 
        as.matrix() 
    }
    
    pred <- round(as.numeric(predict.glmnet(object = fit, newx = features_matrix, type = type)), 2)
    pred[pred<0] <- 0
    pred
    
  } else if(ts_model == "gam"){
    if(add_fitted == T){
      train <- .data
      key_int <- attributes(train)[["key"]]
      features <- setdiff(names(train), c("date_var", "y_var", unlist(parameter$gam$excluded_features)))
      features_cont <- features[unlist(lapply(features, FUN = function(x) is.numeric(train[[x]])))]
      features_factor <- setdiff(features, c(features_cont))
      features_matrix <- train
    } else {
      test <- .data[test_index, ]
      key_int <- attributes(test)[["key"]]
      features <- setdiff(names(test), c("date_var", "y_var", unlist(parameter$gam$excluded_features)))
      features_cont <- features[unlist(lapply(features, FUN = function(x) is.numeric(test[[x]])))]
      features_factor <- setdiff(features, c(features_cont))
      trend_decay_tmp <- get_trend_decay(y_var_length = (max(train_index) + 1)
                                         , trend_decay = parameter$gam$trend_decay
                                         , horizon = (length(test_index) + optim_conf$lag - 2)
                                         , lag = optim_conf$lag) %>% 
        round(2) %>% 
        .[2:length(.)]
      
      test <- test %>% 
        mutate(trend = trend_decay_tmp)
      
      features_matrix <- test %>% 
        dplyr::select(all_of(c(features_cont,features_factor )))
    }
    pred <- round(as.numeric(predict.gam(object = fit, newdata = features_matrix, type = type)), 2)
    pred[pred<0] <- 0
    pred
  } else if(ts_model == "glm"){
    if(add_fitted == T){
      train <- .data
      key_int <- attributes(train)[["key"]]
      features <- setdiff(names(train), c("date_var", "y_var", unlist(parameter$gam$excluded_features)))
      features_cont <- features[unlist(lapply(features, FUN = function(x) is.numeric(train[[x]])))]
      features_factor <- setdiff(features, c(features_cont))
      features_matrix <- train
    } else {
      test <- .data[test_index, ]
      key_int <- attributes(test)[["key"]]
      features <- setdiff(names(test), c("date_var", "y_var", unlist(parameter$glm$excluded_features)))
      features_cont <- features[unlist(lapply(features, FUN = function(x) is.numeric(test[[x]])))]
      features_factor <- setdiff(features, c(features_cont))
      trend_decay_tmp <- get_trend_decay(y_var_length = (max(train_index) + 1)
                                         , trend_decay = parameter$gam$trend_decay
                                         , horizon = (length(test_index) + optim_conf$lag - 2)
                                         , lag = optim_conf$lag) %>% 
        round(2) %>% 
        .[2:length(.)]
      
      test <- test %>%
        mutate(trend = trend_decay_tmp)
      
      features_matrix <- test %>% 
        dplyr::select(all_of(c(features_cont,features_factor )))
    }
    pred <- round(as.numeric(predict.glm(object = fit, newdata = features_matrix, type = type)), 2)
    pred[pred<0] <- 0
    pred
  } else if(ts_model == "arima"){
    if(add_fitted == T){
      pred <- as.numeric(round(fit$fitted, 2))
      pred[pred<0] <- 0
      pred
    } else {
      n_ahead <- optim_conf$test_size + optim_conf$lag - 1
      n_test <- seq(from = (optim_conf$lag), to = n_ahead)
      pred <- round(as.numeric(predict(object = fit, n.ahead = n_ahead)[["pred"]]), 2)
      pred[pred<0] <- 0
      pred[n_test]
      #pred
    }
  } else if(ts_model %in% c("croston")){
    if(add_fitted == T){
      pred <- rep(fit, length(train_index)) 
    } else {
      pred <- rep(fit, length(test_index)) 
    }
    pred
  } else if(ts_model == "ets"){
    if(add_fitted  == T){
      pred <- as.numeric(fitted(fit)) %>% round(2)
    } else {
      n_ahead <- optim_conf$test_size + optim_conf$lag - 1
      n_test <- seq(from = (optim_conf$lag), to = n_ahead)
      pred <- round(as.numeric(predict(object = fit, h = n_ahead)[["mean"]]), 2)
      pred[pred<0] <- 0
      pred[n_test]
    }
  } else {
    message("NO MODEL AVAILABLE")
  }
}




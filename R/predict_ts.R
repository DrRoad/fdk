predict_ts <- function(fit, .data, ts_model, parameter, optim_conf){
  train_index <- 1:(nrow(.data) - (optim_conf$test_size + lag) + 1)
  test_index <- (nrow(.data) - optim_conf$test_size + 1):nrow(.data)
  
  if(ts_model == "glmnet"){
    #test <- .data
    key_int <- attributes(test)[["key"]]
    features <- setdiff(names(test), c("date_var", "y_var", unlist(parameter$glmnet$excluded_features)))
    features_cont <- features[unlist(lapply(features, FUN = function(x) is.numeric(test[[x]])))]
    features_factor <- setdiff(features, c(features_cont))
    
    trend_decay_tmp <- get_trend_decay(y_var_length = max(train_index)
                                       , trend_decay = parameter$glmnet$trend_decay
                                       , horizon = length(test_index)) %>% 
      round(2)
    
    features_matrix <- test %>%
      dplyr::select(all_of(features)) %>% 
      fastDummies::dummy_cols(select_columns = features_factor
                              , remove_selected_columns = T
                              , remove_first_dummy = T) %>% 
      as.matrix() 
    
    predict.glmnet(object = fit, newx = features_matrix)
  }
}




fit_ts <- function(.data, y_var, date_var, model, parameter = NULL){
  if(model == "glmnet"){
    get_glmnet(.data = .data, y_var = y_var, date_var = date_var, parameter = parameter)
  } else if(model == "glm"){
    get_glm(.data = .data, y_var = y_var, date_var = date_var, parameter = parameter)
  } else if(model == "ets"){
    get_ets(.data = .data, y_var = y_var, parameter = parameter)
  } else if(model == "arima"){
    get_arima(.data = .data, y_var = y_var, parameter = parameter)
  } else if(model == "neural_network"){
    get_neural_network(.data = .data, y_var = y_var, parameter = parameter)
  } else if(model == "seasonal_naive"){
    get_seasonal_naive(.data = .data, y_var = y_var, parameter = parameter)
  } else if(model == "tbats"){
    get_tbats(.data = .data, y_var = y_var, parameter = parameter)
  } else if(model == "croston"){
    get_croston(.data = .data, y_var = y_var, parameter = parameter)
  } 
}


autoforecast <- function(.data, parameter, test_size, lag, horizon, model, optim_profile){
  cat("\nProcedures applied: \n- Feaure engineering \n- Winsorize cleansing\n");
  
  .data_tmp <- .data  %>% 
    feature_engineering_ts() %>% 
    clean_ts(method = "winsorize")
  
  get_forecast_int <- function(.data, model, horizon, parameter){
    .data %>% 
      fit_ts(model = model, parameter = parameter) %>% 
      get_forecast(horizon = horizon)
  }
  
  optim_join <- function(.data, model, parameter, horizon, best_model){
    if(model %in% c("glmnet", "glm")){ # missing arima
      get_forecast_int(.data, model = model
                       , parameter = update_parameter(parameter
                                                      , best_model$parameter[[which(best_model$model==model)]]
                                                      , model = model, optim = TRUE)
                       , horizon = 100)
    } else {
      get_forecast_int(.data, model = model, parameter = parameter, horizon = horizon)
    }
  }
  
  if(optim_profile == "fast"){
    cat(paste0("\nFast optimization for: ", length(model), " models + unweighted ensemble forecast"))
    forecast_tmp <- map(model, ~get_forecast_int(.data = .data_tmp
                                               , model = .x, horizon = horizon
                                               , parameter = parameter)) %>% 
      bind_rows() %>% 
      rename(date_var = date, y_var = y_var_fcst) %>% 
      mutate(type = "forecast") %>% 
      bind_rows(.data_tmp, .) %>% 
      select(key:y_var, model, type) %>% 
      replace_na(replace = list(type = "history", model = "history"))
    
    ensemble_tmp <- forecast_tmp %>% 
      filter(type != "history") %>% 
      group_by(date_var) %>% 
      summarise(y_var = mean(y_var), model = "ensemble", type = "forecast", .groups = "drop")
    
    forecast_tmp <- bind_rows(forecast_tmp, ensemble_tmp)
    
    attr(forecast_tmp, "output_type") <- "optim_output"
  } else if(optim_profile == "light"){
    best_model_int <- optim_ts(.data_tmp, test_size = test_size, lag = lag, parameter = parameter, model = model)
    
    forecast_tmp <- map(model, ~optim_join(.data_tmp, model = .x, parameter = parameter
                                                , horizon = horizon, best_model = best_model_int)) %>% 
      bind_rows() %>% 
      rename(date_var = date, y_var = y_var_fcst) %>% 
      mutate(type = "forecast") %>% 
      bind_rows(.data_tmp, .) %>% 
      select(key:y_var, model, type) %>% 
      replace_na(replace = list(type = "history", model = "history"))
    
    ensemble_tmp <- forecast_tmp %>% 
      filter(type != "history") %>% 
      group_by(date_var) %>% 
      summarise(y_var = mean(y_var), model = "ensemble", type = "forecast", .groups = "drop")
    
    forecast_tmp <- bind_rows(forecast_tmp, ensemble_tmp)
    
    attr(forecast_tmp, "output_type") <- "optim_output"
  }
  return(forecast_tmp)
}

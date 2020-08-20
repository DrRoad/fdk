
gen_fcst <- function(data, models, seas = TRUE, parameters = NULL, h = 36){
  
  # Frequency Handling --------------
  
  if(is.null(attributes(data)$frequency)){
    yearly_seasonality <- "auto"
    weekly_seasonality <- FALSE
    daily_seasonality <- FALSE
    time_freq <- "month"
  }else{
    yearly_seasonality <- weekly_seasonality <- daily_seasonality <- FALSE
    seas_param <- attributes(data)$frequency
    # For prophet 
    if(seas_param == 12){
      yearly_seasonality <- TRUE
      time_freq <- "month"
    }
    if(seas_param == 52){
      weekly_seasonality <- TRUE
      time_freq <- "week"
    }
    if(seas_param == 365){
      daily_seasonality <- TRUE
      time_freq <- "day"
    }
  }
  
  # Frequency handling + disable lm for daily forecast
  
  if(time_freq == "day"){
    models <- models[!models %in% c("tslm","glmnet","mlr")]
  }
  
  # Results --------------
  
  results <- data.frame(matrix(nrow = 0,ncol = 6))
  colnames(results) <- c("model","time","predicted","real","mape","parameters")
  
  # Models --------------

  xd1 <- data[[1]] # Time Series
  xd2 <- data[[2]] # Tibble
  
  # Time attributes for tibble
  
  attr(xd2,"yearly_seasonality") <- yearly_seasonality
  attr(xd2,"weekly_seasonality") <- weekly_seasonality
  attr(xd2,"daily_seasonality") <- daily_seasonality
  attr(xd2,"time_freq") <- time_freq

  # Stat Models --------------

  # naive/snaive
  if("naive" %in% models){
    output <- get_naive(xd = xd1, h = h, mode = "fcst")
    results <- rbind(results,output)
  }

  # snaive
  if("snaive" %in% models){
    output <- get_snaive(xd = xd1, h = h, mode = "fcst")
    results <- rbind(results,output)
  }

  # croston
  if("croston" %in% models){
    output <- get_croston(xd = xd1, parameters = parameters$params_croston, h = h, mode = "fcst")
    results <- rbind(results,output)
  }

  # ets
  if("ets" %in% models){
    output <- get_ets(xd = xd1, h = h, mode = "fcst")
    results <- rbind(results,output)
  }

  # theta
  if("theta" %in% models){
    output <- get_theta(xd = xd1, h = h, mode = "fcst")
    results <- rbind(results,output)
  }
  
  # arima
  if("arima" %in% models){
    output <- get_arima(xd = xd1, seas = seas, parameters = parameters$params_arima, h = h, mode = "fcst")
    results <- rbind(results,output)
  }
  
  # tbats
  if("tbats" %in% models){
    output <- get_tbats(xd = xd1, h = h, mode = "fcst")
    results <- rbind(results,output)
  }
  
  # ensemble
  if("ensemble" %in% models){
    output <- get_ensemble(xd = xd1, seas = seas, h = h, mode = "fcst")
    results <- rbind(results,output)
  }
  
  # bagged_ets
  if("bagged_ets" %in% models){
    output <- get_bagged_ets(xd = xd1, h = h, mode = "fcst")
    results <- rbind(results,output)
  }
  
  # theta_dyn
  if("theta_dyn" %in% models){
    output <- get_theta_dyn(xd = xd1, h = h, mode = "fcst")
    results <- rbind(results,output)
  }
  
  # stlm
  if("stlm" %in% models){
    output <- get_stlm(xd = xd1, h = h, mode = "fcst")
    results <- rbind(results,output)
  }
  
  # nnetar
  if("nn" %in% models){
    output <- get_nn(xd = xd1, h = h, mode = "fcst")
    results <- rbind(results,output)
  }
  
  # tslm
  if("tslm" %in% models){
    output <- get_tslm(xd = xd1, h = h, mode = "fcst")
    results <- rbind(results,output)
  }
  
  # prophet
  if("prophet" %in% models){
    output <- get_prophet(xd = xd2, h = h, mode = "fcst")
    results <- rbind(results,output)
  }
  
  # Machine Learning Models --------------
  
  # glmnet
  if("glmnet" %in% models){
    output <- optim_glmnet(data = train[[2]], hyperparam = hyperparam_list,
                           random_sample = .20, config = job_config)
  }
  
  # # mlr
  # if("mlr" %in% models){
  #   output <- get_mlr(xd2, seas, parameters)
  #   results <- rbind(results,output)
  # }
  
  # Return
  
  return(results)
  
}


get_forecast_experimental <- function(.fit_output, x_matrix = NULL, horizon){
  
  if(.fit_output$prescription$freq == 12){
    freq_string <- "month"
  }
  
  if(.fit_output[["model"]]=="arima"){
    tibble(
      date = seq.Date(from = (.fit_output$meta_data$max_date + months(1)), length.out = horizon, by = "months")
      , forecast = as.numeric(forecast(.fit_output[["model_fit"]], horizon)[["mean"]])
      , model = .fit_output$model
    )
  } else if(.fit_output[["model"]]=="glmnet"){
    
    x_matrix <- make_reg_matrix(.fit_output = .fit_output, horizon = horizon)
    
    
    predict.glmnet(object = .fit_output$model_fit, newx = x_matrix) %>%
      as.vector() %>% 
      enframe(name = "date", value = "forecast") %>% 
      mutate(date = seq.Date(from = (.fit_output$prescription$max_date + months(1))
                             , by = freq_string
                             , length.out = horizon)
             , model = "glmnet")
  }
}








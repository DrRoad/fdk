
# croston

get_croston <- function(xd, parameters, h, mode){
  model_name <- "croston"
  if(mode == "sim"){ # For simulation
    if(is.null(parameters)){
      model <- croston(xd$train, h = h)
    }else{
      model <- croston(xd$train, h = h, alpha = parameters)
    }
    fcst <- forecast(model)
    error <- mape(fcst$mean[h],xd$test)
    # Timelapse
    aux <- date_decimal(as.numeric(time(xd$test)))
    time_test <- as.Date(aux, format = "%Y-%m-%d")
    # Output
    output <- data.frame(model = model_name,
                         time = time_test,
                         predicted = as.numeric(fcst$mean[h]),
                         real = as.numeric(xd$test),
                         mape = as.numeric(error),
                         parameters = paste0("Alpha:"," ",
                                             ifelse(is.null(parameters),
                                                    0.1,parameters))
                         )
    return(output) # Output
  }
  if(mode == "fcst"){ # For forecast
    if(is.null(parameters)){
      model <- croston(xd, h = h)
    }else{
      model <- croston(xd, h = h, alpha = parameters)
    }
    fcst <- forecast(model)
    # Timelapse
    aux <- date_decimal(as.numeric(time(fcst$mean)))
    time_test <- as.Date(aux, format = "%Y-%m-%d")
    # Output
    output <- data.frame(model = model_name,
                         time = time_test,
                         predicted = as.numeric(fcst$mean),
                         parameters = paste0("Alpha:"," ",
                                             ifelse(is.null(parameters),
                                                    0.1,parameters))
                         )
    return(output) # Output
  }
}


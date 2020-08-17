
# neural network

get_nn <- function(xd, h, mode){
  model_name <- "nn"
  set.seed(123)
  if(mode == "sim"){ # Simulation
  model <- nnetar(xd$train)
  fcst <- forecast(model, h = h)
  error <- mape(fcst$mean[4],xd$test)
  # Timelapse
  aux <- date_decimal(as.numeric(time(xd$test)))
  time_test <- as.Date(aux, format = "%Y-%m-%d")
  # Output
  output <- data.frame(model = model_name,
                       time = time_test,
                       predicted = as.numeric(fcst$mean[4]),
                       real = as.numeric(xd$test),
                       mape = as.numeric(error),
                       parameters = fcst$method
                       )
  return(output)
  }
  if(mode == "fcst"){ # Forecast
    model <- nnetar(xd)
    fcst <- forecast(model, h = h)
    # Timelapse
    aux <- date_decimal(as.numeric(time(fcst$mean)))
    time_test <- as.Date(aux, format = "%Y-%m-%d")
    # Output
    output <- data.frame(model = model_name,
                         time = time_test,
                         predicted = as.numeric(fcst$mean),
                         parameters = fcst$method
                         )
    return(output)
  }
}


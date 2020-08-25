
# naive

get_naive <- function(xd, h, mode){
  model_name <- "naive"
  if(mode == "sim"){ # Simulation
    model <- naive(xd$train, h = h)
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
                         parameters = fcst$method
                         )
    return(output)
  }
  if(mode == "fcst"){ # Forecast
    model <- naive(xd, h = h)
    fcst <- forecast(model)
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

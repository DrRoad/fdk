
# snaive

get_snaive <- function(xd, h, mode){
  model_name <- "snaive"
  if(mode == "sim"){ # Simulation
    model <- snaive(xd$train, h = h)
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
    model <- snaive(xd, h = h)
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

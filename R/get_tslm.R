
# tslm

get_tslm <- function(xd, h, mode){
  model_name <- "tslm"
  if(mode == "sim"){ # Simulation
    time_series <- xd$train
    model <- tslm(time_series ~ trend + season)
    fcst <- forecast(model, h = h)
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
                         parameters = "Default Trend + Season"
    )
    return(output)
  }
  if(mode == "fcst"){ # Forecast
    time_series <- xd
    model <- tslm(time_series ~ trend + season)
    fcst <- forecast(model, h = h)
    # Timelapse
    aux <- date_decimal(as.numeric(time(fcst$mean)))
    time_test <- as.Date(aux, format = "%Y-%m-%d")
    # Output
    output <- data.frame(model = model_name,
                         time = time_test,
                         predicted = as.numeric(fcst$mean),
                         parameters = "Default Trend + Season"
    )
    return(output)
  }
}

#---
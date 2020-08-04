
# snaive

get_snaive <- function(xd, h, mode){
  model_name <- "snaive"
  if(mode == "sim"){ # Simulation
    model <- snaive(xd$train, h = h)
    fcst <- forecast(model)
    error <- mape(fcst$mean[4],xd$test)
    output <- data.frame(model = model_name,
                         time = as.Date(xd$test, format = "%Y-%m-%d"),
                         predicted = as.numeric(fcst$mean[4]),
                         real = as.numeric(xd$test),
                         mape = as.numeric(error),
                         parameters = fcst$method
    )
    return(output)
  }
  if(mode == "fcst"){ # Forecast
    model <- snaive(xd, h = h)
    fcst <- forecast(model)
    output <- data.frame(model = model_name,
                         time = as.Date(fcst$mean, format = "%Y-%m-%d"),
                         predicted = as.numeric(fcst$mean),
                         parameters = fcst$method
    )
    return(output)
  }
}

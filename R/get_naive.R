
# naive

get_naive <- function(xd, h, mode){
  model_name <- "naive"
  if(mode == "sim"){ # Simulation
    model <- naive(xd$train, h = h)
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
    model <- naive(xd, h = h)
    fcst <- forecast(model)
    output <- data.frame(model = model_name,
                         time = as.Date(fcst$mean, format = "%Y-%m-%d"),
                         predicted = as.numeric(fcst$mean),
                         parameters = fcst$method
                         )
    return(output)
  }
}

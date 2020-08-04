
# ets

get_ets <- function(xd, h, mode){
  model_name <- "ets"
  if(mode == "sim"){ # Simulation
    model <- ets(xd$train)
    fcst <- forecast(model, h = h)
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
    model <- ets(xd)
    fcst <- forecast(model, h = h)
    output <- data.frame(model = model_name,
                         time = as.Date(fcst$mean, format = "%Y-%m-%d"),
                         predicted = as.numeric(fcst$mean),
                         parameters = fcst$method
    )
    return(output)
  }
}

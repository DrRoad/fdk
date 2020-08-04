
# arima

get_arima <- function(xd, seas = seas, parameters = NULL, h, mode){
  model_name <- "arima"
  if(mode == "sim"){ # Simulation
    # No Seas
    if (seas == FALSE){
      if (is.null(parameters$params_arima)){
        model <- auto.arima(xd$train, seasonal = FALSE)
      } else {
        model <- Arima(xd$train, order = parameters[1:3])
      }
    }
    # Sarima (Seas)
    if (seas == TRUE){
      if (is.null(parameters$params_arima)){
        model <- auto.arima(xd$train, seasonal = TRUE)
      } else {
        model <- Arima(xd$train, order=parameters$params_arima[1:3], seasonal=parameters[4:6])
      }
    }
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
    # No Seas
    if (seas == FALSE){
      if (is.null(parameters$params_arima)){
        model <- auto.arima(xd, seasonal = FALSE)
      } else {
        model <- Arima(xd, order = parameters[1:3])
      }
    }
    # Sarima (Seas)
    if (seas == TRUE){
      if (is.null(parameters$params_arima)){
        model <- auto.arima(xd, seasonal = TRUE)
      } else {
        model <- Arima(xd, order=parameters[1:3], seasonal=parameters[4:6])
      }
    }
    fcst <- forecast(model, h = h)
    output <- data.frame(model = model_name,
                         time = as.Date(fcst$mean, format = "%Y-%m-%d"),
                         predicted = as.numeric(fcst$mean),
                         parameters = fcst$method
                         )
    return(output)
  }
}

#---

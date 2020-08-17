
# arima

get_arima <- function(xd, seas, parameters = NULL, h, mode){
  model_name <- "arima"
  if(mode == "sim"){ # Simulation
    # No Seas
    if (seas == FALSE){
      if (is.null(parameters$params_arima)){
        model <- auto.arima(xd$train, stepwise = TRUE, seasonal = FALSE)
      } else {
        model <- Arima(xd$train, order = parameters[1:3])
      }
    }
    # Sarima (Seas)
    if (seas == TRUE){
      if (is.null(parameters$params_arima)){
        model <- auto.arima(xd$train, stepwise = TRUE, seasonal = TRUE)
      } else {
        model <- Arima(xd$train, order=parameters$params_arima[1:3], seasonal=parameters[4:6])
      }
    }
    fcst <- forecast(model, h = h)
    error <- mape(fcst$mean[h],xd$test)
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
    # No Seas
    if (seas == FALSE){
      if (is.null(parameters$params_arima)){
        model <- auto.arima(xd, seasonal = FALSE)
      } else {
        model <- Arima(xd, stepwise = TRUE, order = parameters[1:3])
      }
    }
    # Sarima (Seas)
    if (seas == TRUE){
      if (is.null(parameters$params_arima)){
        model <- auto.arima(xd, stepwise = TRUE, seasonal = TRUE)
      } else {
        model <- Arima(xd, order=parameters[1:3], seasonal=parameters[4:6])
      }
    }
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

#---

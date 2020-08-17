
# ensemble

eat_ensemble <- function(y, seas, h){
  require(forecast)
  fets <- forecast(ets(y, model="ZZZ", damped=NULL, allow.multiplicative.trend=FALSE), h = h)
  if(seas == TRUE){
    farima <- forecast(auto.arima(y, stepwise = TRUE, seasonal = TRUE), h = h)
  }else{
    farima <- forecast(auto.arima(y, stepwise = TRUE, seasonal = FALSE), h = h)
  }
  ftheta <- thetaf(y, h = h)
  comb <- fets
  comb$mean <-(fets$mean + farima$mean + ftheta$mean)/3
  comb$method <- "ETS-ARIMA-Theta Combination"
  return(comb)
}

#---

get_ensemble <- function(xd, seas, h, mode){
  model_name <- "ensemble"
  if(mode == "sim"){ # Simulation
    fcst <- eat_ensemble(xd$train, seas = seas, h = h)
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
    fcst <- eat_ensemble(xd, seas = seas, h = h)
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

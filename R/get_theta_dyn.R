
# theta_dyn

get_theta_dyn <- function(xd, h, mode){
  model_name <- "theta_dyn"
  if(mode == "sim"){ # Simulation
    model <- dotm(xd$train, h = h)
    error <- mape(model$mean[h],xd$test)
    # Timelapse
    aux <- date_decimal(as.numeric(time(xd$test)))
    time_test <- as.Date(aux, format = "%Y-%m-%d")
    # Output
    output <- data.frame(model = model_name,
                         time = time_test,
                         predicted = as.numeric(model$mean[h]),
                         real = as.numeric(xd$test),
                         mape = as.numeric(error),
                         parameters = model$method
    )
    return(output)
  }
  if(mode == "fcst"){ # Forecast
    model <- dotm(xd, h = h)
    # Timelapse
    aux <- date_decimal(as.numeric(time(model$mean)))
    time_test <- as.Date(aux, format = "%Y-%m-%d")
    # Output
    output <- data.frame(model = model_name,
                         time = time_test,
                         predicted = as.numeric(model$mean),
                         parameters = model$method
    )
    return(output)
  }
}

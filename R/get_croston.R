
# croston

get_croston <- function(xd, parameters, h, mode){
  model_name <- "croston"
  if(mode == "sim"){ # For simulation
    if(is.null(parameters)){
      model <- croston(xd$train, h = h)
    }else{
      model <- croston(xd$train, h = h, alpha = parameters)
    }
    fcst <- forecast(model)
    error <- mape(fcst$mean[4],xd$test)
    output <- data.frame(model = model_name,
                         time = as.Date(xd$test, format = "%Y-%m-%d"),
                         predicted = as.numeric(fcst$mean[4]),
                         real = as.numeric(xd$test),
                         mape = as.numeric(error),
                         parameters = paste0("Alpha:"," ",
                                             ifelse(is.null(parameters),
                                                    0.1,parameters))
                         )
    return(output) # Output
  }
  if(mode == "fcst"){ # For forecast
    if(is.null(parameters)){
      model <- croston(xd, h = h)
    }else{
      model <- croston(xd, h = h, alpha = parameters)
    }
    fcst <- forecast(model)
    output <- data.frame(model = model_name,
                         time = as.Date(fcst$mean, format = "%Y-%m-%d"),
                         predicted = as.numeric(fcst$mean),
                         parameters = paste0("Alpha:"," ",
                                             ifelse(is.null(parameters),
                                                    0.1,parameters))
                         )
    return(output) # Output
  }
}


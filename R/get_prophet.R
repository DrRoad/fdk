
# get_prophet

get_prophet <- function(xd, h, mode){
  # Model name
  model_name <- "prophet"
  # Run models
  if(mode == "sim"){ # Simulation
    # Reformatting xd
    xd$train <- xd$train %>% rename(ds = date)
    xd$test <- xd$test %>% rename(ds = date)
    # Model
    model <- prophet(xd$train, growth ="linear",
                     weekly.seasonality = attr(xd,"weekly_seasonality"),
                     daily.seasonality = attr(xd,"daily_seasonality"),
                     yearly.seasonality = attr(xd,"yearly_seasonality"),
                     seasonality.prior.scale = 50)
    future <- make_future_dataframe(model, periods = h, freq = attr(xd,"time_freq"))
    forecast <- predict(model, future)
    fcst <- forecast$yhat[length(xd$train$y)+h]
    error <- mape(fcst,xd$test$y)
    output <- data.frame(model = model_name,
                         time = as.Date(xd$test$ds),
                         predicted = as.numeric(fcst),
                         real = as.numeric(xd$test$y),
                         mape = as.numeric(error),
                         parameters = "Default"
                         )
    return(output)
  }
  if(mode == "fcst"){ # Forecast
    # Reformatting xd
    xd <- xd %>% rename(ds = date)
    # Model
    model <- prophet(xd, growth ="linear",
                     weekly.seasonality = attr(xd,"weekly_seasonality"),
                     daily.seasonality = attr(xd,"daily_seasonality"),
                     yearly.seasonality = attr(xd,"yearly_seasonality"),
                     seasonality.prior.scale = 50)
    future <- make_future_dataframe(model, periods = h, freq = attr(xd,"time_freq"))
    forecast <- predict(model, future)
    forecast_out <- forecast[(nrow(xd)+1):(nrow(xd)+h),]
    output <- data.frame(model = model_name,
                         time = as.Date(forecast_out$ds),
                         predicted = as.numeric(forecast_out$yhat),
                         parameters = "Default"
                         )
    return(output)
  }
}

#---

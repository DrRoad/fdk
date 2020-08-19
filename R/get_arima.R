

# ARIMA -------------------------------------------------------------------


get_arima <- function(.data, yvar, is_seasonal = TRUE, parameter = NULL, frequency = 12){
  
  yvar <- ts(pull(select(.data, {{yvar}})), frequency = frequency)
  
  if(is.null(parameters)==TRUE){
    message("ARIMA optimization...")
    model_fit <- auto.arima(yvar, stepwise = TRUE, seasonal = is_seasonal)
  } else if(is.null(parameters[["arima"]][["seasonal"]])==FALSE){
    model_fit <- Arima(yvar, order = parameters[["arima"]][["order"]], seasonal = parameters[["arima"]][["seasonal"]])
  } else {
    model_fit <- Arima(yvar, order = parameters[["arima"]][["order"]])
  }
  
  fit_output <- list(model = "arima"
                 , model_fit = model_fit
                 , parameter = arimaorder(model_fit)
                 , meta_data = list(
                   max_date = max(.data[["date"]])
                 ))
  return(fit_output)
}


  

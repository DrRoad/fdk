

# ARIMA -------------------------------------------------------------------

get_arima <- function(.data, y_var, is_seasonal = TRUE, parameter = NULL, frequency = 12){
  
  if(is.null(attributes(.data)[["prescription"]]) == FALSE) {
    prescription <- attributes(.data)[["prescription"]]
    y_var <- "y_var"
    date_var <- "date"
    frequency <- prescription$frequency
    na_exclude <- unique(c(prescription$key, y_var, date_var))
  }
  
  y_var <- ts(.data[[y_var]], frequency = frequency) # maybe not optimal
  
  if(is.null(parameters)==TRUE){
    message("ARIMA optimization...")
    model_fit <- auto.arima(y_var, stepwise = TRUE, seasonal = is_seasonal)
  } else if(is.null(parameters[["arima"]][["seasonal"]])==FALSE){
    model_fit <- Arima(y_var, order = parameters[["arima"]][["order"]], seasonal = parameters[["arima"]][["seasonal"]])
  } else {
    model_fit <- Arima(y_var, order = parameters[["arima"]][["order"]])
  }
  
  fit_output <- list(model = "arima"
                 , model_fit = model_fit
                 , parameter = arimaorder(model_fit))
  
  return(fit_output)
}




  
